int fcsTab[256];

void makeFcsTab() {
    int b, v, i;
    for (b = 0; b < 256; b++) {
        v = b;
        for (i = 8; i--;) {
            v = v & 1 ? (v >> 1) ^ 0x8408 : v >> 1;
        }
        fcsTab[b] = v & 0xffff;
    }
}

int doFcsCalc(unsigned char* buf, int siz) {
    int fcs = 0xffff;
    int i;
    for (i = 0; i < siz; i++) {
        fcs = (fcs >> 8) ^ fcsTab[(fcs ^ buf[i]) & 0xff];
    }
    return fcs ^ 0xffff;
}
