#define mixMax 64

int mixDly;

int mixThr;

int mixSrc;

int mixSel;

long mixVolO;

struct mixOne {

    int src;

    int *buf;

    int posW;

    int posR;

    char stp;

    long volL;

    long volR;

    int pkt;

    int ovr;

    int exc;

    int und;

    int trn;

    int gap;

    int sln;

};

struct mixOne mixChn[mixMax];



void vol2rng(long *cur, int dir) {
    long mov = *cur / 50;
    if (mov < 1) {
        mov = 1;
    }
    *cur += dir * mov;
    if (*cur < 0) {
        *cur = 0;
    }
    if (*cur > 999) {
        *cur = 999;
    }
}

void mixIni(struct mixOne *d, long vl, long vr) {
    memset(d, 0, sizeof(*d));
    d->src = recHnd;
    d->stp = 1;
    d->volL = vl;
    d->volR = vr;
    vol2rng(&d->volL, 0);
    vol2rng(&d->volR, 0);
    int len = mixDly * sizeof(int) * (pktln / smpbt);
    d->buf = malloc(len);
    if (d->buf == NULL) err("error allocating");
    memset(d->buf, 0, len);
}

int mixDec(struct mixOne *d) {
    if (bufS < 1) return 0;
    if (bufS < pktln) {
        for (int i = bufS; i < pktln; i++) bufD[padln + i] = 0;
        d->trn++;
    }
    d->posW = (d->posW + 1) % mixDly;
    int* o = &d->buf[d->posW * (pktln / smpbt)];
    for (int i = 0; i < pktln; i += smpbt) {
        *o = iou_gsam(i);
        o++;
    }
    d->pkt++;
    return 1;
}


void iou_chan() {
    mixDec(&mixChn[0]);
    for (int i = 1; i < mixSrc; i++) {
        struct mixOne *d = &mixChn[i];
        recHnd = mixChn[i].src;
        int don = 0;
        for (;; don++) {
            recFnc();
            if (mixDec(&mixChn[i]) == 0) break;
        }
        if (don < 1) d->und++;
        if (don > 1) d->exc += don-1;
        if (don >= mixDly) d->ovr++;
    }
    recHnd = mixChn[0].src;
    long res[pktln / smpbt];
    memset(&res, 0, sizeof(res));
    for (int n = 0; n < mixSrc; n++) {
        struct mixOne *d = &mixChn[n];
        if (mixThr >= 0) {
            int used = (d->posW - d->posR + mixDly) % mixDly;
            if (d->stp != 0) {
                if (used < mixThr) {
                    d->sln++;
                    continue;
                }
                d->stp = 0;
            }
            if (used < 1) {
                d->stp = 1;
                d->gap++;
                continue;
            }
        }
        d->posR = (d->posR + 1) % mixDly;
        int* p = &d->buf[d->posR * (pktln / smpbt)];
        long volL = d->volL;
        long volR = d->volR;
        for (int i = 0; i < (pktln / smpbt); i += 2) {
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
    char ch = 0;
    read(STDIN_FILENO, &ch, sizeof(ch));
    switch (ch) {
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
        mixSel = (ch - '1' + 10) % 10;
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
        mixChn[mixSel].volL = 0;
        mixChn[mixSel].volR = 0;
        break;
    case '+':
    case 'u':
    case 'U':
        if (mixSel < 0) {
            vol2rng(&mixVolO, +1);
            break;
        }
        vol2rng(&mixChn[mixSel].volL, +1);
        vol2rng(&mixChn[mixSel].volR, +1);
        break;
    case '-':
    case 'd':
    case 'D':
        if (mixSel < 0) {
            vol2rng(&mixVolO, -1);
            break;
        }
        vol2rng(&mixChn[mixSel].volL, -1);
        vol2rng(&mixChn[mixSel].volR, -1);
        break;
    case '[':
    case 'l':
    case 'L':
        if (mixSel < 0) {
            break;
        }
        vol2rng(&mixChn[mixSel].volL, +1);
        vol2rng(&mixChn[mixSel].volR, -1);
        break;
    case ']':
    case 'r':
    case 'R':
        if (mixSel < 0) {
            break;
        }
        vol2rng(&mixChn[mixSel].volL, -1);
        vol2rng(&mixChn[mixSel].volR, +1);
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
        printf("\r\ncounters cleared\r\n");
        for (i = 0; i < mixSrc; i++) {
            struct mixOne *d = &mixChn[i];
            d->pkt = 0;
            d->ovr = 0;
            d->exc = 0;
            d->und = 0;
            d->trn = 0;
            d->gap = 0;
            d->sln = 0;
        }
        break;
    case ' ':
        printf("\r\n\r\n\rchn  packets   missed truncate  overrun underrun   excess     gaps  silence\r\n");
        for (i = 0; i < mixSrc; i++) {
            struct mixOne *d = &mixChn[i];
            printf("\r%3d %8d %8d %8d %8d %8d %8d %8d %8d\r\n", i + 1, d->pkt, mixChn[0].pkt - d->pkt,  d->trn, d->ovr, d->und, d->exc, d->gap, d->sln);
        }
        printf("\r\n");
        break;
    }
    printf("\rchange:");
    if (mixSel < 0) {
        printf("out");
    }    else {
        printf("in%i", mixSel + 1);
    }
    printf("  out:%li  ", mixVolO);
    for (i = 0; i < mixSrc; i++) {
        struct mixOne *d = &mixChn[i];
        printf("in%i: %li,%li  ", i+1, d->volL, d->volR);
    }
    printf("    \r");
    fflush(stdout);
}
