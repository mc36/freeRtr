snd_pcm_t *plyHnd = NULL;
unsigned char plyBuf[pktln * 4];
int plyLen = 0;

void ply_init(char*dev) {
    snd_pcm_hw_params_t *prm = NULL;
    if (snd_pcm_open(&plyHnd, dev, SND_PCM_STREAM_PLAYBACK, 0) < 0) err("cannot open pcm device");
    snd_pcm_hw_params_alloca(&prm);
    iou_device_open(plyHnd, prm);
}

void iou_write() {
    if (plyLen > 0) {
        int res = snd_pcm_writei(plyHnd, &plyBuf[0], plyLen);
        if (res != plyLen) err("error writing");
        plyLen = 0;
    }
    iou_depth(&plyBuf[0], &bufD[padln], smpbt + sampAdj, smpbt, bufS);
    bufS = bufS / (2 * smpbt);
    int res = snd_pcm_writei(plyHnd, &plyBuf[0], bufS);
    if (res == bufS) return;
    if (res > 0) err("halfwrite happened");
    plyLen = bufS;
    res = snd_pcm_recover(plyHnd, res, 0);
    if (res != 0) err("error recovering");
}

void iou_stop() {
    snd_pcm_drain(plyHnd);
    snd_pcm_close(plyHnd);
}
