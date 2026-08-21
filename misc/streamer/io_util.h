#include "io_cnst.h"

unsigned char bufD[padln + pktln + padln];
int bufS;

void err(char* e) {
    printf("%s\n", e);
    exit(1);
}

void iou_read();

void iou_write();

void iou_stop();

void iou_pmsb(int ofs, int val) {
    bufD[ofs + 0] = val >> 24;
    bufD[ofs + 1] = val >> 16;
    bufD[ofs + 2] = val >> 8;
    bufD[ofs + 3] = val;
}

int iou_gmsb(int ofs) {
    return (bufD[ofs + 0] << 24) | (bufD[ofs + 1] << 16) | (bufD[ofs + 2] << 8) | bufD[ofs + 3];
}

void iou_bswp() {
    for (int p = 0; p < bufS; p += smpbt) {
        unsigned char b0 = bufD[p + padln + 0];
#if smpbt > 1
        unsigned char b1 = bufD[p + padln + 1];
#endif
#if smpbt > 2
        unsigned char b2 = bufD[p + padln + 2];
#endif
#if smpbt > 3
        unsigned char b3 = bufD[p + padln + 3];
#endif
#if smpbt > 3
        bufD[p + padln + smpbt - 4] = b3;
#endif
#if smpbt > 2
        bufD[p + padln + smpbt - 3] = b2;
#endif
#if smpbt > 1
        bufD[p + padln + smpbt - 2] = b1;
#endif
        bufD[p + padln + smpbt - 1] = b0;
    }
}

void iou_loop() {
    for (;;) {
        iou_read();
        if (bufS < 1) break;
        iou_write();
    }
    iou_stop();
}

int iou_frmt() {
    if ((pktln % (smpbt * 2)) != 0) err("samples not fully fit");
#if smpbt == 1
    return SND_PCM_FORMAT_S8;
#endif
#if smpbt == 2
    return SND_PCM_FORMAT_S16_LE;
#endif
#if smpbt == 3
    return SND_PCM_FORMAT_S24_3LE;
#endif
#if smpbt == 4
    return SND_PCM_FORMAT_S32_LE;
#endif
}
