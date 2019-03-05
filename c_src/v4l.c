
#include <stdio.h>
#include <string.h>
#include <fcntl.h>
#include <sys/mman.h>
#include <sys/ioctl.h>
#include <linux/videodev2.h>
#include <math.h>

#include "jpeglib.h"

/* For default.elf.do
ELF_LDLIBS="-ljpeg -lm"
*/

#define WRITE write
#define READ  read
#include "system.h"
#include "port.h"

struct stats {
    uint32_t tag;
    uint16_t width;
    uint16_t height;
    float motion;
    float brightness;
};

// FIXME
#ifndef MAIN
#define MAIN poll_main
#endif

#ifndef BUILDINFO
#define BUILDINFO ""
#endif


// jpeg example comes from
// https://github.com/LuaDist/libjpeg/blob/master/example.c
// https://stackoverflow.com/questions/4559648/write-to-memory-buffer-instead-of-file-with-libjpeg
// https://github.com/libjpeg-turbo/libjpeg-turbo/blob/master/libjpeg.txt

uint8_t *jpeg_buf = NULL;
unsigned long jpeg_size = 0;
#define WIDTH  640
#define HEIGHT 480
uint8_t last_y[WIDTH * HEIGHT];
uint8_t frame[WIDTH * HEIGHT * 2];

void compress(uint8_t *buf, struct stats *stats) {
    struct jpeg_compress_struct cinfo;
    struct jpeg_error_mgr jerr;
    cinfo.err = jpeg_std_error(&jerr);
    jpeg_create_compress(&cinfo);

    cinfo.image_width  = WIDTH;
    cinfo.image_height = HEIGHT;
    cinfo.input_components = 3;
    cinfo.in_color_space = JCS_YCbCr;
    jpeg_set_defaults(&cinfo);

    int quality = 75;
    jpeg_set_quality(&cinfo, quality, TRUE);

    jpeg_mem_dest(&cinfo, &jpeg_buf, &jpeg_size);
    jpeg_start_compress(&cinfo, TRUE);
    uint8_t out_row[3 * WIDTH];
    JSAMPROW row_pointer[1] = {&out_row[0]};

    /* Use one loop to layout the pixel in the format desired by the
     * jpeg library, and to perform motion estimation. */
    uint64_t square_diff_acc = 0;
    uint32_t level_acc = 0;
    while (cinfo.next_scanline < HEIGHT) {
        uint8_t *in_row = &buf[cinfo.next_scanline * 2 * WIDTH];
        uint8_t *last_row = &last_y[WIDTH * cinfo.next_scanline];
        for (int x=0; x<WIDTH; x++) {
            int x2 = x/2;
            // Y Cr Cb Y Cr Cb <- Y Cr Y Cb
            out_row[x*3]   = in_row[x*2];
            out_row[x*3+1] = in_row[x2*4+1];
            out_row[x*3+2] = in_row[x2*4+3];
            // motion
            uint32_t diff = in_row[x*2] - last_row[x];
            last_row[x] = in_row[x*2];
            square_diff_acc += diff * diff;
            level_acc += in_row[x*2];
        }
        (void) jpeg_write_scanlines(&cinfo, row_pointer, 1);
    }
    jpeg_finish_compress(&cinfo);
    jpeg_destroy_compress(&cinfo);
    float scale = 1.0f / ((float)(WIDTH * HEIGHT));
    stats->brightness = scale * (float)level_acc;
    stats->motion     = scale * sqrtf((float)square_diff_acc);
}


struct buffer {
    void   *start;
    size_t  length;
};

// Stripped down version of
// https://linuxtv.org/downloads/v4l-dvb-apis/uapi/v4l/capture.c.html

int MAIN(int argc, char **argv) {

    const char *dev = "/dev/video0";
    const char *ssh_dev = getenv("SSH_ORIGINAL_COMMAND");
    if (ssh_dev) { dev = ssh_dev; }
    if (argc == 2) { dev = argv[1]; };
    int fd;
    LOG("dev = %s\n", dev);
    ASSERT_ERRNO(fd = open(dev, O_RDWR, 0));

    struct v4l2_capability cap = {};
    /* My devices do not support V4L2_CAP_READWRITE, so use mmap */
    ASSERT_ERRNO(ioctl(fd, VIDIOC_QUERYCAP, &cap));
    ASSERT(cap.capabilities & V4L2_CAP_VIDEO_CAPTURE);
    ASSERT(cap.capabilities & V4L2_CAP_STREAMING);

    /* Comment out to preserve original settings as set by v4l2-ctl */
    struct v4l2_format fmt = {
        .type = V4L2_BUF_TYPE_VIDEO_CAPTURE,
        .fmt = {
            .pix = {
                .width       = WIDTH,
                .height      = HEIGHT,
                .pixelformat = V4L2_PIX_FMT_YUYV,
            }
        }
    };
    ASSERT_ERRNO(ioctl(fd, VIDIOC_S_FMT, &fmt));

    /* Setup up buffers and queue them. */
    struct v4l2_requestbuffers req = {
        .count = 2,
        .type = V4L2_BUF_TYPE_VIDEO_CAPTURE,
        .memory = V4L2_MEMORY_MMAP
    };
    ASSERT_ERRNO(ioctl(fd, VIDIOC_REQBUFS, &req));
    ASSERT(req.count >= 2);
    struct buffer buffers[req.count];
    for (int i=0; i<req.count; i++) {
        struct v4l2_buffer buf = {
            .type   = V4L2_BUF_TYPE_VIDEO_CAPTURE,
            .memory = V4L2_MEMORY_MMAP,
            .index  = i
        };
        ASSERT_ERRNO(ioctl(fd, VIDIOC_QUERYBUF, &buf));
        buffers[i].length = buf.length;
        buffers[i].start =
            mmap(NULL /* start anywhere */,
                 buf.length,
                 PROT_READ | PROT_WRITE /* required */,
                 MAP_SHARED /* recommended */,
                 fd, buf.m.offset);
        ASSERT(MAP_FAILED != buffers[i].start);
        ASSERT_ERRNO(ioctl(fd, VIDIOC_QBUF, &buf));
    }


    /* Main streaming loop. */
    enum v4l2_buf_type type = V4L2_BUF_TYPE_VIDEO_CAPTURE;
    ASSERT_ERRNO(ioctl(fd, VIDIOC_STREAMON, &type));
    for(;;) {
        /* Other side needs to request frame.  This solves two
         * problems: it limits the data rate to something the pipe can
         * handle, and will kill this process when the pipe is
         * closed. */
        int cmd_len = assert_read_u32(0);
        //LOG("cmd_len=%d\n", cmd_len);
        ASSERT(cmd_len == 0);
        //if (cmd_len) {
        //    uint8_t cmd_buf[cmd_len];
        //    assert_read_fixed(0, &cmd_buf[0], cmd_len);
        //}

        for (int p = 0; p < WIDTH * HEIGHT; p++) {
            frame[p*2]   = 0;
            frame[p*2+1] = 0x80;
        }
        for (int i = 0; i < 1; i ++) {
            struct v4l2_buffer buf = {
                .type   = V4L2_BUF_TYPE_VIDEO_CAPTURE,
                .memory = V4L2_MEMORY_MMAP
            };
            ASSERT_ERRNO(ioctl(fd, VIDIOC_DQBUF, &buf));
            ASSERT(buf.index < req.count);
            //LOG("buf:%d bytes:%d.\n", buf.index, buf.bytesused);
            uint8_t *src = buffers[buf.index].start;
            for (int p = 0; p < WIDTH * HEIGHT; p++) {
                frame[p*2] += src[p*2];
                frame[p*2+1] += src[p*2+1] - 0x80;
            }
            ASSERT_ERRNO(ioctl(fd, VIDIOC_QBUF, &buf));
        }
        struct stats stats = {
            .tag    = 1,
            .width  = WIDTH,
            .height = HEIGHT
        };
        compress(frame, &stats);

        //LOG("motion: %f\n", motion);
        //LOG("jpeg_size: %d.\n", (int)jpeg_size);
        //write(1, jpeg_buf, jpeg_size); exit(0);
        //assert_write_port32(1, buffers[buf.index].start, buf.bytesused);
        assert_write_port32(1, jpeg_buf, jpeg_size);
        assert_write_port32(1, &stats, sizeof(stats));
        // cleanup
        free(jpeg_buf);
        jpeg_buf = NULL;
        jpeg_size = 0;
    }
}

