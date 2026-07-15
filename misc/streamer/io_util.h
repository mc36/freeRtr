#define srate 48000
#define pktln 1280
#define padln 12

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

void iou_bswp() {
    for (int p=0; p < bufS; p+=2) {
        unsigned char b0 = bufD[p+0];
        unsigned char b1 = bufD[p+1];
        bufD[p+0] = b1;
        bufD[p+1] = b0;
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
