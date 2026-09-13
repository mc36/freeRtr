#define mixMax 64

int mixDly;

int mixNum;

int mixHnd[mixMax];

long mixVol;

long mixVolL[mixMax];

long mixVolR[mixMax];

int mixPosW[mixMax];

int mixPosR[mixMax];

int mixPkt[mixMax];

int mixOvr[mixMax];

int mixUnd[mixMax];

int mixTrn[mixMax];


unsigned char *mixBuf[mixMax];


long vol2rng(long cur, int dir) {
    long mov = cur / 10;
    if (mov < 1) {
        mov = 1;
    }
    cur += dir * mov;
    if (cur < 0) {
        cur = 0;
    }
    if (cur > 999) {
        cur = 999;
    }
    return cur;
}



void mixDec(int n) {
    if (bufS < 0) {
        mixUnd[n]++;
        return;
    }
    if (bufS < pktln) {
        for (int i = bufS; i < pktln; i++) bufD[padln + i] = 0;
        mixTrn[n]++;
    }
    memcpy(mixBuf[n], &bufD[padln], pktln);
}


void iou_chan() {
    mixDec(0);
    for (int i = 1; i < mixNum; i++) {
        recHnd = mixHnd[i];
        recFnc();
        mixDec(i);
    }
    recHnd = mixHnd[0];
}
