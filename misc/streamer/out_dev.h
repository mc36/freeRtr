snd_pcm_t *plyHnd = NULL;

void ply_init(char*dev) {
    snd_pcm_hw_params_t *prm = NULL;
    if (snd_pcm_open(&plyHnd, dev, SND_PCM_STREAM_PLAYBACK, 0) < 0) err("cannot open pcm device");
    snd_pcm_hw_params_alloca(&prm);
    snd_pcm_hw_params_any(plyHnd, prm);
    if (snd_pcm_hw_params_set_rate_resample(plyHnd, prm, 1) < 0) err("unable to set resample");
    if (snd_pcm_hw_params_set_access(plyHnd, prm, SND_PCM_ACCESS_RW_INTERLEAVED) < 0) err("unable to set mode");
    if (snd_pcm_hw_params_set_format(plyHnd, prm, iou_frmt()) < 0) err("unable to set format");
    if (snd_pcm_hw_params_set_channels(plyHnd, prm, 2) < 0) err("unable to set channel");
    if (snd_pcm_hw_params_set_rate(plyHnd, prm, srate, 0) < 0) err("unable to set rate");
    if (snd_pcm_hw_params(plyHnd, prm) < 0) err("cannot set parameters");
    if (snd_pcm_prepare(plyHnd) < 0) err("cannot prepare");
}

void iou_write() {
    bufS = bufS / (2 * smpbt);
    int res = snd_pcm_writei(plyHnd, &bufD[padln], bufS);
    if (res == bufS) return;
    res = snd_pcm_recover(plyHnd, res, 0);
    if (res != 0) err("error writing");
}

void iou_stop() {
    snd_pcm_drain(plyHnd);
    snd_pcm_close(plyHnd);
}
