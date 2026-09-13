#define mixMax 64

#define mixLen (pktln / smpbt)

int mixDly;

int mixSrc;

int mixHnd[mixMax];

long mixVolO;

long mixVolL[mixMax];

long mixVolR[mixMax];

int mixPosW[mixMax];

int mixPosR[mixMax];

int mixPkt[mixMax];

int mixOvr[mixMax];

int mixUnd[mixMax];

int mixTrn[mixMax];

int **mixBuf;


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



int mixDec(int n) {
    if (bufS < 1) {
        mixUnd[n]++;
        return 0;
    }
    if (bufS < pktln) {
        for (int i = bufS; i < pktln; i++) bufD[padln + i] = 0;
        mixTrn[n]++;
    }
    int* o = mixBuf[mixPosW[n] + (n * mixDly)];
    for (int i = 0; i < pktln; i += smpbt) {
        *o = iou_gsam(i);
        o++;
    }
    mixPosW[n] = (mixPosW[n] + 1) % mixDly;
    mixPkt[n]++;
    return 1;
}


void iou_chan() {
    mixDec(0);
    for (int i = 1; i < mixSrc; i++) {
        recHnd = mixHnd[i];
        int don = 0;
        for (;; don++) {
            recFnc();
            if (mixDec(i) == 0) break;
        }
        if (don >= mixDly) mixOvr[i]++;
    }
    recHnd = mixHnd[0];
    long res[mixLen];
    memset(&res, 0, sizeof(res));
    for (int n = 0; n < mixSrc; n++) {
        int* p = mixBuf[mixPosR[n] + (n * mixDly)];
        mixPosR[n] = (mixPosR[n] + 1) % mixDly;
        for (int i = 0; i < mixLen; i++) {
            res[i] += *p;
            p++;
        }
    }
    long* p = res;
    for (int i = 0; i < pktln; i += smpbt) {
        long val = *p;
        p++;
        val *= mixVolO;
        val /= mixSrc;
        val /= 100;
        iou_psam(i, val);
    }
    bufS = pktln;
}
