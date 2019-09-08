/*
 *  Adapted from iceprog:
 *  iceprog -- simple programming tool for FTDI-based Lattice iCE programmers
 *
 *  Copyright (C) 2015  Clifford Wolf <clifford@clifford.at>
 *  Copyright (C) 2019  Tom Schouten <tom@zwizwa.be>
 *
 *  Permission to use, copy, modify, and/or distribute this software for any
 *  purpose with or without fee is hereby granted, provided that the above
 *  copyright notice and this permission notice appear in all copies.
 *
 *  THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 *  WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 *  MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 *  ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 *  WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 *  ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 *  OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *
 *  Relevant Documents:
 *  -------------------
 *  http://www.latticesemi.com/~/media/Documents/UserManuals/EI/icestickusermanual.pdf
 *  http://www.micron.com/~/media/documents/products/data-sheet/nor-flash/serial-nor/n25q/n25q_32mb_3v_65nm.pdf
 *  http://www.ftdichip.com/Support/Documents/AppNotes/AN_108_Command_Processor_for_MPSSE_and_MCU_Host_Bus_Emulation_Modes.pdf
 *
 *  For a list of MPSSE commands see
 *  https://github.com/legege/libftdi/blob/master/src/ftdi.h
 */

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <sys/stat.h>
#include <stdint.h>

#define _GNU_SOURCE

/* Why is this different on openwrt?  Is it libftdi vs libftdi1 ?
 * I've disabled building the binary for anything other than host. */
//#if UCTOOLS_ARCH_nexx
//#include <libftdi1/ftdi.h>
//#else
#include <ftdi.h>
//#endif

#include <poll.h>

#include "system.h"
#include "port.h"

/* See default.elf.do
ELF_LDLIBS=-lftdi
*/

struct ftdi_context ftdic;

void send_byte(uint8_t data) {
    ASSERT(1 == ftdi_write_data(&ftdic, &data, 1));
}
uint8_t recv_byte() {
    uint8_t data;
    while (1) {
        int rc;
        ASSERT(rc = ftdi_read_data(&ftdic, &data, 1) >= 0);
        if (rc == 1) break;
        usleep(100);
    }
    return data;
}
void send_spi(const uint8_t *data, int n) {
    if (n < 1) return;
    send_byte(MPSSE_DO_WRITE | MPSSE_WRITE_NEG);
    send_byte(n-1);
    send_byte((n-1) >> 8);
    int rc = ftdi_write_data(&ftdic, (uint8_t *)data, n);
    ASSERT(rc == n);
}

void send_spi_chunked(const uint8_t *buf, uint32_t len) {
    while (len > 0) {
        uint32_t chunk = len > 4096 ? 4096 : len;
        // LOG("left: %d\n", len);
        send_spi(buf, chunk);
        buf += chunk;
        len -= chunk;
    }
    usleep(10000);
}

void set_gpio(int slavesel_b, int creset_b) {
    uint8_t gpio = 1;
    if (slavesel_b) {
        // ADBUS4 (GPIOL0)
        gpio |= 0x10;
    }
    if (creset_b) {
        // ADBUS7 (GPIOL3)
        gpio |= 0x80;
    }
    send_byte(SET_BITS_LOW);
    send_byte(gpio);
    send_byte(WRITE_EXTENDED);
}

int get_cdone() {
    uint8_t data;
    send_byte(GET_BITS_LOW);
    data = recv_byte();
    // ADBUS6 (GPIOL2)
    return (data & 0x40) != 0;
}

void prog_sram_write(const uint8_t *buf, uint32_t len) {
    LOG("reset..\n");

    set_gpio(0, 0);
    usleep(100);

    set_gpio(0, 1);
    usleep(2000);

    LOG("cdone: %s\n", get_cdone() ? "high" : "low");

    send_spi_chunked(buf, len);

    // add 48 dummy bits
    send_byte(CLK_BYTES);
    send_byte(0x05);
    send_byte(0x00);

    // add 1 more dummy bit
    send_byte(CLK_BITS);
    send_byte(0x00);

    LOG("cdone: %s\n", get_cdone() ? "high" : "low");
}


void prog_sram(const char *filename) {
    LOG("reset..\n");

    set_gpio(0, 0);
    usleep(100);

    set_gpio(0, 1);
    usleep(2000);

    LOG("cdone: %s\n", get_cdone() ? "high" : "low");

    FILE *f;
    ASSERT(f = fopen(filename, "rb"));
    LOG("programming..\n");
    while (1) {
        static unsigned char buffer[4096];
        int rc = fread(buffer, 1, 4096, f);
        if (rc <= 0) break;
        send_spi(buffer, rc);
    }
    fclose(f);

    // add 48 dummy bits
    send_byte(CLK_BYTES);
    send_byte(0x05);
    send_byte(0x00);

    // add 1 more dummy bit
    send_byte(CLK_BITS);
    send_byte(0x00);

    LOG("cdone: %s\n", get_cdone() ? "high" : "low");

    set_gpio(1, 1);


}



void raw_spi_write(const uint8_t *buf, uint32_t len) {

    set_gpio(0, 1);
    usleep(1000);
    LOG("sending..\n");
    send_spi_chunked(buf, len);
    LOG("done\n");
    set_gpio(1, 1);
    usleep(100);
}



void raw_spi(const char *filename) {
    set_gpio(0, 1);
    usleep(1000);

    FILE *f;
    ASSERT(f = fopen(filename, "rb"));
    LOG("sending..\n");
    while (1) {
        static unsigned char buffer[4096];
        int rc = fread(buffer, 1, 4096, f);
        if (rc <= 0) break;
        send_spi(buffer, rc);
    }
    fclose(f);
    LOG("done\n");
    set_gpio(1, 1);
    usleep(100);
}

struct command {
    uint8_t cmd;
    uint8_t reply_size;
    uint8_t reserved[2];
    uint8_t data[];
} __attribute__((__packed__));


static void ack(uint8_t *msg, uint8_t size) {
    uint8_t buf[4] = {0,0,0,size};
    assert_write(1, buf, sizeof(buf));
    if (size) assert_write(1, msg, size);
}

// This is supposed to be run through authorized_keys COMMAND=
// mechanism, which allows fine-grained access control to perform
// update of port binaries.

// SSH_ORIGINAL_COMMAND=i:0x0403:0x6010 ./ftdi_connect.host.elf

int main(int argc, char **argv) {
    const char *devname = getenv("SSH_ORIGINAL_COMMAND");
    if (!devname) {
        ASSERT(argc >= 2);
        devname = argv[1];
    }
    ASSERT(devname);
    // FIXME: Assert that devname contains a valid device name.
    // Command injection is possible.
    LOG("devname = %s\n", devname);

    enum ftdi_interface ifnum = INTERFACE_A;
    ftdi_init(&ftdic);
    ftdi_set_interface(&ftdic, ifnum);
    ASSERT(0 == ftdi_usb_open_string(&ftdic, devname));
    ASSERT(0 == ftdi_usb_reset(&ftdic));
    ASSERT(0 == ftdi_usb_purge_buffers(&ftdic));

    // Multi-Protocol Synchronous Serial Engine
    // FIXME: Should this be configurable?
    ASSERT(ftdi_set_bitmode(&ftdic, 0xff, BITMODE_MPSSE) >= 0);

    // enable clock divide by 5
    send_byte(EN_DIV_5);

    // clock divide
    send_byte(TCK_DIVISOR);
    send_byte(0x00); // 6MHz
    send_byte(0x00);

    LOG("cdone: %s\n", get_cdone() ? "high" : "low");

    set_gpio(1, 1);
    usleep(100000);

    struct pollfd pfd[1] = {{ .events = POLLERR | POLLIN, .fd = 0 }};

    for(;;) {
        int timeout_ms = 1000;
        int rv;
        ASSERT_ERRNO(rv = poll(&pfd[0], 1, timeout_ms));
        if (rv == 0) {
            // FIXME: We always know what to expect. Async is not necessary.
            // FIXME: Use poll for this as well?

            uint8_t buf[1024];
            int nb_read;
            ASSERT((nb_read = ftdi_read_data(&ftdic, &buf[0], sizeof(buf))) >= 0);
            if (nb_read) {
                //LOG("have ftdi bytes\n");
            }
            else {
                //LOG("no ftdi bytes\n");
            }
        }
        else {
            // Data available.
            uint32_t len = 0;
            struct command *c = assert_read_packet4_len(0, &len);
            ASSERT(len >= sizeof(*c));
            uint32_t data_len = len - sizeof(*c);

            switch(c->cmd) {
            case 0: { // simple command / response transaction
                LOG("MPSSE command/response\n");
                for (uint32_t i = 0; i<data_len; i++) {
                    send_byte(c->data[i]);
                }
                if (c->reply_size) {
                    uint8_t reply[c->reply_size];
                    for (uint32_t i=0; i<c->reply_size; i++) {
                        reply[i] = recv_byte();
                    }
                    ack(reply, c->reply_size);
                }
                else {
                    ack(NULL, 0);
                }
                break;
            }
            case 1: // high level SPI transfer
                LOG("SPI transfer\n");
                raw_spi_write(c->data, data_len);
                ack(NULL, 0);
                break;
            case 2: // iCE40 program transfer
                LOG("SPI bitstream transfer\n");
                prog_sram_write(c->data, data_len);
                ack(NULL, 0);
                break;
            default:
                ERROR("bad command %d\n", (int)c->cmd);
            }
        }
    }

    //prog_sram("
}


