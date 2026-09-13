#define mixMax 64

#define mixLen (pktln / smpbt)

int mixDly;

int mixSrc;

int mixSel;

int mixHnd[mixMax];

long mixVolO;

long mixVolL[mixMax];

long mixVolR[mixMax];

int mixPosW[mixMax];

int mixPosR[mixMax];

int mixPkt[mixMax];

int mixOvr[mixMax];

int mixExc[mixMax];

int mixUnd[mixMax];

int mixTrn[mixMax];

int **mixBuf;


long vol2rng(long cur, int dir) {
    cur += dir;
    if (cur < 0) {
        cur = 0;
    }
    if (cur > 999) {
        cur = 999;
    }
    return cur;
}



int mixDec(int n) {
    if (bufS < 1) return 0;
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
        if (don < 1) mixUnd[i]++;
        if (don > 1) mixExc[i] += don-1;
        if (don >= (mixDly - 1)) mixOvr[i]++;
    }
    recHnd = mixHnd[0];
    long res[mixLen];
    memset(&res, 0, sizeof(res));
    for (int n = 0; n < mixSrc; n++) {
        int* p = mixBuf[mixPosR[n] + (n * mixDly)];
        mixPosR[n] = (mixPosR[n] + 1) % mixDly;
        long volL = mixVolL[n];
        long volR = mixVolR[n];
        for (int i = 0; i < mixLen; i += 2) {
            long val = *p;
            p++;
            val *= volL;
            val /= 100;
            res[i + 0] += val;
            val = *p;
            p++;
            val *= volR;
            val /= 100;
            res[i + 1] += val;
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
    int i = 0;
    ioctl(STDIN_FILENO, FIONREAD, &i);
    if (i < 1) return;
    i = 0;
    read(STDIN_FILENO, &i, 1);
    switch (i) {
    case '0':
    case '1':
    case '2':
    case '3':
    case '4':
    case '5':
    case '6':
    case '7':
    case '8':
    case '9':
        mixSel = (i - '1' + 10) % 10;
        if (mixSel < mixSrc) {
            break;
        }
        mixSel = mixSrc - 1;
        break;
    case 'o':
    case 'O':
        mixSel = -1;
        break;
    case 'm':
    case 'M':
        if (mixSel < 0) {
            mixVolO = 0;
            break;
        }
        mixVolL[mixSel] = 0;
        mixVolR[mixSel] = 0;
        break;
    case '+':
    case 'u':
    case 'U':
        if (mixSel < 0) {
            mixVolO = vol2rng(mixVolO, +1);
            break;
        }
        mixVolL[mixSel] = vol2rng(mixVolL[mixSel], +1);
        mixVolR[mixSel] = vol2rng(mixVolR[mixSel], +1);
        break;
    case '-':
    case 'd':
    case 'D':
        if (mixSel < 0) {
            mixVolO = vol2rng(mixVolO, -1);
            break;
        }
        mixVolL[mixSel] = vol2rng(mixVolL[mixSel], -1);
        mixVolR[mixSel] = vol2rng(mixVolR[mixSel], -1);
        break;
    case '[':
    case 'l':
    case 'L':
        if (mixSel < 0) {
            break;
        }
        mixVolL[mixSel] = vol2rng(mixVolL[mixSel], +1);
        mixVolR[mixSel] = vol2rng(mixVolR[mixSel], -1);
        break;
    case ']':
    case 'r':
    case 'R':
        if (mixSel < 0) {
            break;
        }
        mixVolL[mixSel] = vol2rng(mixVolL[mixSel], -1);
        mixVolR[mixSel] = vol2rng(mixVolR[mixSel], +1);
        break;
    case 'x':
    case 'X':
    case 'q':
    case 'Q':
        err("\r\nuser requested");
        break;
    case '?':
        printf("\r\nenter=status, space=detail, 1..9=input, o=output, +,-,u,d=volume up/down, ],[,l,r=balance left/right, m=mute, c=clear, x=exit\r\n");
        break;
    case 'c':
    case 'C':
        memset(mixPkt, 0, sizeof(mixPkt));
        memset(mixOvr, 0, sizeof(mixPkt));
        memset(mixUnd, 0, sizeof(mixPkt));
        memset(mixExc, 0, sizeof(mixPkt));
        memset(mixTrn, 0, sizeof(mixPkt));
        break;
    case ' ':
        printf("\r\n\r\n\r        channel         packets          missed       truncated         overrun        underrun       excessive\r\n");
        for (i = 0; i < mixSrc; i++) {
            printf("\r%15i %15i %15i %15i %15i %15i %15i\r\n", i + 1, mixPkt[i], mixPkt[0] - mixPkt[i],  mixTrn[i], mixOvr[i], mixUnd[i], mixExc[i]);
        }
        printf("\r\n");
        break;
    }
    printf("\rsel:");
    if (mixSel < 0) {
        printf("out");
    }    else {
        printf("in%i", mixSel + 1);
    }
    printf("  out:%li  ", mixVolO);
    for (i = 0; i < mixSrc; i++) {
        printf("in%i: %li,%li  ", i+1, mixVolL[i], mixVolR[i]);
    }
    printf("    \r");
    fflush(stdout);
}
