snd_pcm_t *recHnd = NULL;
unsigned char recBuf[pktln * 4];

void rec_init(char*dev, char*vol) {
    monoVol = 100.0 * atof(vol);
    snd_pcm_hw_params_t *prm = NULL;
    if (snd_pcm_open(&recHnd, dev, SND_PCM_STREAM_CAPTURE, 0) < 0) err("cannot open pcm device");
    snd_pcm_hw_params_alloca(&prm);
    iou_device_open(recHnd, prm);
}

void iou_read() {
    bufS = snd_pcm_readi(recHnd, &recBuf[0], pktln / (2 * smpbt));
    if (bufS > 0) {
        if (bufS != (pktln / (2 * smpbt))) err("halfread happened");
        bufS = iou_depth(&bufD[padln], &recBuf[0], smpbt, smpbt + sampAdj, bufS * (smpbt + sampAdj) * 2);
        return;
    }
    bufS = snd_pcm_recover(recHnd, bufS, 0);
    if (bufS != 0) err("error recovering");
    bufS = snd_pcm_readi(recHnd, &recBuf[0], pktln / (2 * smpbt));
    if (bufS != (pktln / (2 * smpbt))) err("halfread happened");
    bufS = iou_depth(&bufD[padln], &recBuf[0], smpbt, smpbt + sampAdj, bufS * (smpbt + sampAdj) * 2);
}
