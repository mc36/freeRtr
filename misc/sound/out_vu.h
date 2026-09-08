
double plyAvgL = 0.0;

double plyAvgR = 0.0;

int plyCnt = 0;

void ply_init() {
}


double vuSam(int ofs) {
    int i = ((char)bufD[ofs + padln + smpbt - 1] << 8) + bufD[ofs + padln + smpbt - 2];
    return (double) i / 32768.0;
}

double vuRms(double sum) {
    double rms = sqrt(sum * (double) (2 * smpbt) / (double) bufS);
    return fmax(0.0, fmin(50.0, (50.0 * log10(rms)) + 50.0));
}

#define vuBarC(chr) {out[pos]=chr;pos++;}

#define vuBarM for (int i = 0; i < cur; i++) vuBarC('*')

#define vuBarE for (int i = cur; i < 50; i++) vuBarC(' ')

void vuBars(double l, double r, char e) {
    char out[200];
    int pos = 0;
    int cur = (int)l;
    vuBarE;
    vuBarM;
    vuBarC(' ');
    vuBarC(' ');
    cur = (int)r;
    vuBarM;
    vuBarE;
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
    for (int i = 0; i < bufS; i += 2 * smpbt) {
        double o = vuSam(i + 0);
        sumL += o * o;
        o = vuSam(i + smpbt);
        sumR += o * o;
    }
    sumL = vuRms(sumL);
    sumR = vuRms(sumR);
    plyAvgL += sumL;
    plyAvgR += sumR;
    plyCnt += bufS;
    vuBars(sumL, sumR, 13);
}

void iou_stop() {
}
