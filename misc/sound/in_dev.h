snd_pcm_t *recHnd = NULL;

void rec_init(char*dev, char*vol) {
    monoVol = 100.0 * atof(vol);
    snd_pcm_hw_params_t *prm = NULL;
    if (snd_pcm_open(&recHnd, dev, SND_PCM_STREAM_CAPTURE, 0) < 0) err("cannot open pcm device");
    snd_pcm_hw_params_alloca(&prm);
    snd_pcm_hw_params_any(recHnd, prm);
    if (snd_pcm_hw_params_set_rate_resample(recHnd, prm, 1) < 0) err("unable to set resample");
    if (snd_pcm_hw_params_set_access(recHnd, prm, SND_PCM_ACCESS_RW_INTERLEAVED) < 0) err("unable to set mode");
    if (snd_pcm_hw_params_set_format(recHnd, prm, iou_frmt()) < 0) err("unable to set format");
    if (snd_pcm_hw_params_set_channels(recHnd, prm, 2) < 0) err("unable to set channel");
    if (snd_pcm_hw_params_set_rate(recHnd, prm, srate, 0) < 0) err("unable to set rate");
    if (snd_pcm_hw_params(recHnd, prm) < 0) err("cannot set parameters");
    if (snd_pcm_prepare(recHnd) < 0) err("cannot prepare");
}

void iou_read() {
#define iou_read1() bufS = snd_pcm_readi(recHnd, &recBuf[0], pktln / (2 * smpbt));
#define iou_read2() bufS *= 2 * (smpbt+smpad);
#if smpad == 0
#define iou_read3() memcpy(&bufD[padln], &recBuf[0], pktln);
#else
#define iou_read3() bufS = iou_depth(&bufD[padln], &recBuf[0], smpbt, smpbt+smpad, bufS);
#endif
    unsigned char recBuf[pktln * 4];
    iou_read1();
    if (bufS > 0) {
        iou_read2();
        iou_read3();
        return;
    }
    bufS = snd_pcm_recover(recHnd, bufS, 0);
    if (bufS != 0) err("error recovering");
    iou_read1();
    iou_read2();
    iou_read3();
}
