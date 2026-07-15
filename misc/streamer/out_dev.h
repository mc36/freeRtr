snd_pcm_t *plyHnd = NULL;

void ply_init(char*dev) {
    snd_pcm_hw_params_t *prm = NULL;
    if (snd_pcm_open(&plyHnd, dev, SND_PCM_STREAM_PLAYBACK, 0) < 0) err("cannot open pcm device");
    snd_pcm_hw_params_alloca(&prm);
    snd_pcm_hw_params_any(plyHnd, prm);
    if (snd_pcm_set_params(plyHnd, SND_PCM_FORMAT_S16_LE, SND_PCM_ACCESS_RW_INTERLEAVED, 2, srate, 1, 50000) < 0) err("unable to set params");
    if (snd_pcm_hw_params_set_access(plyHnd, prm, SND_PCM_ACCESS_RW_INTERLEAVED) < 0) err("unable to set mode");
    if (snd_pcm_hw_params_set_format(plyHnd, prm, SND_PCM_FORMAT_S16_LE) < 0) err("unable to set format");
    if (snd_pcm_hw_params_set_channels(plyHnd, prm, 2) < 0) err("unable to set channel");
    if (snd_pcm_hw_params_set_rate(plyHnd, prm, srate, 0) < 0) err("unable to set rate");
    if (snd_pcm_hw_params(plyHnd, prm) < 0) err("cannot set parameters");
    if (snd_pcm_prepare(plyHnd) < 0) err("cannot prepare");
}

void iou_write() {
    bufS = bufS >> 2;
    int res = snd_pcm_writei(plyHnd, &bufD[padln], bufS);
    if (res == bufS) return;
    res = snd_pcm_recover(plyHnd, res, 1);
    if (res != 0) err("error writing");
}

void iou_stop() {
    snd_pcm_drain(plyHnd);
    snd_pcm_close(plyHnd);
}
