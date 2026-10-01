char* iou_device_one(snd_pcm_t *hnd, snd_pcm_hw_params_t *prm, int fmt) {
    sampAdj = fmt;
    switch (smpbt + sampAdj) {
    case 1:
        fmt = SND_PCM_FORMAT_S8;
        break;
    case 2:
        fmt = SND_PCM_FORMAT_S16_LE;
        break;
    case 3:
        fmt = SND_PCM_FORMAT_S24_3LE;
        break;
    case 4:
        fmt = SND_PCM_FORMAT_S32_LE;
        break;
    default:
        return "unknown bit depth";
    }
    snd_pcm_hw_params_any(hnd, prm);
    if (snd_pcm_hw_params_set_rate_resample(hnd, prm, 1) < 0) return "unable to set resample";
    if (snd_pcm_hw_params_set_access(hnd, prm, SND_PCM_ACCESS_RW_INTERLEAVED) < 0) return "unable to set mode";
    if (snd_pcm_hw_params_set_format(hnd, prm, fmt) < 0) return "unable to set format";
    if (snd_pcm_hw_params_set_channels(hnd, prm, 2) < 0) return "unable to set channel";
    if (snd_pcm_hw_params_set_rate(hnd, prm, srate, 0) < 0) return "unable to set rate";
    if (snd_pcm_hw_params_set_buffer_time(hnd, prm, 8000, 0) < 0) return "unable to set buffer";
    if (snd_pcm_hw_params_set_period_time(hnd, prm, 4000, 0) < 0) return "unable to set period";
    if (snd_pcm_hw_params(hnd, prm) < 0) return "cannot set parameters";
    if (snd_pcm_prepare(hnd) < 0) return "cannot prepare";
    return NULL;
}

void iou_device_open(snd_pcm_t *hnd, snd_pcm_hw_params_t *prm) {
    if (iou_device_one(hnd, prm, 0) == NULL) return;
    if (iou_device_one(hnd, prm, +1) == NULL) return;
    if (iou_device_one(hnd, prm, -1) == NULL) return;
    if (iou_device_one(hnd, prm, +2) == NULL) return;
    if (iou_device_one(hnd, prm, -2) == NULL) return;
    if (iou_device_one(hnd, prm, +3) == NULL) return;
    if (iou_device_one(hnd, prm, -3) == NULL) return;
    char* res = iou_device_one(hnd, prm, 0);
    if (res == NULL) return;
    err(res);
}
