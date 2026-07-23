
double plyAvgL = 0.0;

double plyAvgR = 0.0;

int plyCnt = 0;

void ply_init() {
}


double vuSam(int ofs) {
    int i = ((char)bufD[ofs + padln + 1] << 8) + bufD[ofs + padln + 0];
    return (double) i / 32768.0;
}

double vuRms(double sum) {
    double rms = sqrt(sum * 4.0 / (double) bufS);
    return (20.0 * log10l(rms)) + 3.8;
}

double vuAng(double vu) {
    if (vu <= -20) {
        return -25;
    }
    if (vu >= 3) {
        return 25;
    }
    if (vu <= -20) {
        return -23;
    }
    if (vu <= -10) {
        return -23 + ((vu + 20) / 10) * 7;
    }
    if (vu <= -7) {
        return -16 + ((vu + 10) / 3) * 4;
    }
    if (vu <= -5) {
        return -12 + ((vu + 7) / 2) * 4;
    }
    if (vu <= -3) {
        return -8 + ((vu + 5) / 2) * 5;
    }
    if (vu <= -2) {
        return -3 + ((vu + 3) / 1) * 3;
    }
    if (vu <= -1) {
        return 0 + ((vu + 2) / 1) * 3.5;
    }
    if (vu <= 0) {
        return 3.5 + ((vu + 1) / 1) * 4.5;
    }
    if (vu <= 1) {
        return 8 + (vu / 1) * 5;
    }
    if (vu <= 2) {
        return 13 + ((vu - 1) / 1) * 5;
    }
    if (vu <= 3) {
        return 18 + ((vu - 2) / 1) * 7;
    } else {
        return 25;
    }
}

#define vuBarC(chr) {out[pos]=chr;pos++;}

#define vuBarM for (int i = 0; i < cur; i++) vuBarC(42)

#define vuBarS for (int i = cur; i < 50; i++) vuBarC(32)

void vuBars(double l, double r, char e) {
    char out[80];
    int pos = 0;
    int cur = 25+(int)l;
    vuBarS;
    vuBarM;
    vuBarC(32);
    vuBarC(32);
    cur = 25+(int)r;
    vuBarM;
    vuBarS;
    vuBarC(e);
    write(STDOUT_FILENO, out, pos);
}

void iou_write() {
    if (plyCnt >= srate) {
        plyAvgL /= ((double)srate / (double)pktln);
        plyAvgR /= ((double)srate / (double)pktln);
        vuBars(plyAvgL, plyAvgR, 10);
        plyAvgL = 0.0;
        plyAvgR = 0.0;
        plyCnt = 0;
    }
    double sumL = 0;
    double sumR = 0;
    for (int i=0; i < bufS; i+=4) {
        double o = vuSam(i + 0);
        sumL += o * o;
        o = vuSam(i + 2);
        sumR += o * o;
    }
    sumL = vuAng(vuRms(sumL));
    sumR = vuAng(vuRms(sumR));
    plyAvgL += sumL;
    plyAvgR += sumR;
    plyCnt += bufS;
    vuBars(sumL, sumR, 13);
}

void iou_stop() {
}
