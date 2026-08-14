SNDFILE *recFil;
SF_INFO recInf;
SRC_STATE *recSta;
SRC_DATA recDat;
int *recPos;
int recRem;
float recBuf[4096];
float recOut[4096];
int recRes[4096];

void rec_init(char*fil, char*pos) {
    int error;
    recFil = sf_open(fil, SFM_READ, &recInf);
    if (recFil == NULL) err("error opening source");
    if (recInf.channels != 2) err("not stereo source");
    sf_seek(recFil, atoi(pos) * recInf.samplerate, SEEK_SET);
    recSta = src_new(SRC_SINC_MEDIUM_QUALITY, 2, &error);
    if (recSta == NULL) err("error opening sampler");
    recDat.end_of_input = 0;
    recDat.input_frames = 0;
    recDat.data_in = recBuf;
    recDat.data_out = recOut;
    recDat.output_frames = sizeof(recOut) / (sizeof(float) * 2);
    recDat.src_ratio = (double)srate / recInf.samplerate;
    recRem = 0;
}

void iou_read() {
    bufS = 0;
    for (;;) {
        if (recDat.input_frames < 1) {
            recDat.input_frames = sf_readf_float(recFil, recBuf, 256);
            recDat.data_in = recBuf;
            if (recDat.input_frames < 1) break;
        }
        if (recRem < 1) {
            if (src_process(recSta, &recDat) != 0) err("error resampling");
            recDat.data_in += recDat.input_frames_used * 2;
            recDat.input_frames -= recDat.input_frames_used;
            recRem = recDat.output_frames_gen * 2;
            if (recRem < 1) break;
            src_float_to_int_array(recOut, recRes, recRem);
            recPos = recRes;
        }
        int need = (pktln - bufS) / smpbt;
        if (need > recRem) need = recRem;
        if (need < 1) break;
        recRem -= need;
        for (int pos = 0; pos < need; pos++) {
            int val = *recPos;
#if smpbt < 4
            val >>= 32 - (8 * smpbt);
#endif
            bufD[padln + bufS + 0] = val;
#if smpbt > 1
            bufD[padln + bufS + 1] = val >> 8;
#endif
#if smpbt > 2
            bufD[padln + bufS + 2] = val >> 16;
#endif
#if smpbt > 3
            bufD[padln + bufS + 3] = val >> 24;
#endif
            recPos++;
            bufS += smpbt;
        }
    }
    usleep(500000 * pktln / (srate * smpbt) - 500);
}
