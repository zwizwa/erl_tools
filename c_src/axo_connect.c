#include "system.h"
#include "port.h"
#include <libusb-1.0/libusb.h>
#include <pthread.h>
#include <stdint.h>
#include <unistd.h>

struct libusb_device_handle *handle;
const unsigned char ep_in  = 0x82;
const unsigned char ep_out = 0x02;

static int transfer(unsigned char ep, void *buf, int len) {
    int transferred;
    int rv =
        libusb_bulk_transfer(
            handle, ep, buf, len,
            &transferred, 0);
    if (rv) {
        ERROR("transfer error: ep=0x%x, rv=%d, %s\n",
              ep, rv, libusb_strerror(rv));
    }
    return transferred;
}

static void *reader_main(void *ctx) {
    uint8_t buf[4096];
    for(;;) {
        int n = transfer(ep_in, buf, sizeof(buf));
        // FIXME: handle long packets
        // LOG("received %d bytes\n", n);
        assert_write_port32(1, buf, n);
    }
}

#define TO_AXO(var) to_axo(&(var),sizeof(var))
static void to_axo(const void *buf, int len) {
    int n = transfer(ep_out, buf, len);
    LOG("sent %d bytes\n", n);
}

void open_usb(void) {
    int err = libusb_init(NULL);
    if (err) ERROR("libusb_init error = %d\n", err);

    // 16c0:0442
    // http://libusb.sourceforge.net/api-1.0/group__libusb__asyncio.html

    struct libusb_device **devs;
    ssize_t cnt = libusb_get_device_list(NULL, &devs);
    ASSERT(cnt > 0);
    for(ssize_t i=0; i<cnt; i++) {
        struct libusb_device *dev = devs[i];
        struct libusb_device_descriptor desc;
        ASSERT(0 == libusb_get_device_descriptor(dev, &desc));
        if ((desc.idVendor == 0x16c0) &&
            (desc.idProduct == 0x0442)) {
            LOG("%04x:%04x", desc.idVendor, desc.idProduct);
            int rv;
            if (0 == (rv = libusb_open(dev, &handle))) {
                LOG(" ok\n");
                return handle;
            }
            else {
                ERROR(" error = %d (%s)\n", rv, libusb_strerror(rv));
            }
        }
    }
    ERROR("device not found\n");
}
int main(int argc, char **argv) {
    LOG("axo_connect.c\n");
    open_usb();
    pthread_t reader;
    pthread_create(&reader, NULL, &reader_main, NULL);

    for(;;) {
        uint32_t len;
        void *buf = assert_read_packet4_len(0, &len);
        to_axo(buf, len);
        free(buf);
    }
}
