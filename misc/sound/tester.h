#include "io_incl.h"
#include "io_cnst.h"
#undef smpbt
#undef pktln
#define smpbt testerDepth
#define pktln 60000
#include "io_tim0.h"
#include "io_util.h"
#include "io_chn0.h"
#include "in_nil.h"
#include "out_nil.h"

unsigned char testerBak[pktln * 4];

int testerCmp;

void tester_lod() {
    memcpy(&bufD[padln], testerBak, pktln);
}

void tester_cmp() {
    testerCmp = memcmp(&bufD[padln], testerBak, pktln);
}

void tester_clr() {
    memset(bufD, 123, sizeof(bufD));
}

void tester_beg() {
    for (int i=0; i < pktln; i++) testerBak[i] = rand();
    tester_lod();
    bufS = pktln;
    testerCmp = 0;
}

void tester_end() {
    int old = testerCmp;
    tester_cmp();
    if ((old != 0) && (testerCmp == 0) && (bufS == pktln)) {
        printf(" ok!");
    }  else {
        printf(" FAiLED!");
    }
    testerCmp = 0;
}


void tester_bswap() {
    printf("bswap:");
    int xorer = smpbt == 1 ? 1 : 0;
    tester_beg();
    iou_bswp2msb();
    iou_bswp2lsb();
    bufD[padln] ^= xorer;
    tester_cmp();
    iou_bswp2msb();
    iou_bswp2lsb();
    bufD[padln] ^= xorer;
    tester_end();
    printf("\n");
}


void tester_sampl() {
    int tmp[pktln];
    printf("sampl:");
    tester_beg();
    for (int i=0, o=0; i < pktln; i += smpbt, o++) tmp[o] = iou_gsam(i);
    tester_clr();
    tester_cmp();
    for (int i=0, o=0; i < pktln; i += smpbt, o++) iou_psam(i, tmp[o]);
    tester_end();
    printf("\n");
}


void tester_mono() {
    printf("mono:");
    tester_beg();
    monoVol = 10;
    iou_mono(0, 0);
    iou_mono(smpbt, smpbt);
    tester_cmp();
    tester_lod();
    monoVol = 100;
    iou_mono(0, 0);
    iou_mono(smpbt, smpbt);
    tester_end();
    printf("\n");
}


void tester_planar() {
    printf("planar:");
    for (int n = 1; n < 5; n++) {
        tester_beg();
        for (int i = 0; i < n; i++) iou_toPlnr();
        tester_cmp();
        for (int i = 0; i < n; i++) iou_unPlnr();
        tester_end();
    }
    printf("\nunplanar:");
    for (int n = 1; n < 5; n++) {
        tester_beg();
        for (int i = 0; i < n; i++) iou_unPlnr();
        tester_cmp();
        for (int i = 0; i < n; i++) iou_toPlnr();
        tester_end();
    }
    printf("\n");
}


void tester_depth() {
    unsigned char tmp[pktln * 4];
    printf("depth:");
    for (int n = 0; n < 4; n++) {
        tester_beg();
        bufS = iou_depth(&tmp[0], &bufD[padln], smpbt+n, smpbt, bufS);
        tester_clr();
        tester_cmp();
        bufS = iou_depth(&bufD[padln], &tmp[0], smpbt, smpbt+n, bufS);
        tester_end();
    }
    printf("\n");
}



int main(int argc, char**argv) {
    srand(getpid());
    printf("testing @ ");
    ply_init();
    rec_init();
    iou_loop();
    tester_mono();
    tester_sampl();
    tester_bswap();
    tester_depth();
    tester_planar();
    return 0;
}
