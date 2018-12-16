#include "system.h"

#include <stdio.h>
#include <string.h>
#include <fcntl.h>              /* low-level i/o */
#include <sys/mman.h>
#include <sys/ioctl.h>
#include <linux/videodev2.h>


// FIXME
#ifndef MAIN
#define MAIN poll_main
#endif

#ifndef BUILDINFO
#define BUILDINFO ""
#endif

struct buffer {
    void   *start;
    size_t  length;
};

// Stripped down version of
// https://linuxtv.org/downloads/v4l-dvb-apis/uapi/v4l/capture.c.html

int MAIN(int argc, char **argv) {
    char *dev = "/dev/video0";
    if (argc == 2) { dev = argv[1]; };
    int fd;
    ASSERT_ERRNO(fd = open(dev, O_RDWR, 0));

    struct v4l2_capability cap = {};
    struct v4l2_format fmt = {
        .type = V4L2_BUF_TYPE_VIDEO_CAPTURE,
        .fmt = {
            .pix = {
                .width       = 640,
                .height      = 480,
                .pixelformat = V4L2_PIX_FMT_YUYV,
                .field       = V4L2_FIELD_INTERLACED
            }
        }
    };
    /* My devices do not support V4L2_CAP_READWRITE, so use mmap */
    ASSERT_ERRNO(ioctl(fd, VIDIOC_QUERYCAP, &cap));
    ASSERT(cap.capabilities & V4L2_CAP_VIDEO_CAPTURE);
    ASSERT(cap.capabilities & V4L2_CAP_STREAMING);

    /* Comment out to preserve original settings as set by v4l2-ctl */
    ASSERT_ERRNO(ioctl(fd, VIDIOC_S_FMT, &fmt));

    /* Setup up buffers and queue them. */
    struct v4l2_requestbuffers req = {
        .count = 4,
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
        struct v4l2_buffer buf = {
            .type   = V4L2_BUF_TYPE_VIDEO_CAPTURE,
            .memory = V4L2_MEMORY_MMAP
        };
        ASSERT_ERRNO(ioctl(fd, VIDIOC_DQBUF, &buf));
        ASSERT(buf.index < req.count);
        LOG("buf:%d bytes:%d.\n", buf.index, buf.bytesused);
        //  process_image(buffers[buf.index].start, buf.bytesused);
        ASSERT_ERRNO(ioctl(fd, VIDIOC_QBUF, &buf));
    }
}
