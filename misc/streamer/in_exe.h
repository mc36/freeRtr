int recHnd[2];

void rec_init(char*fil, char*pos, char*vol) {
    if (pipe(recHnd)==-1) err("cannot create pipe");
    if (fork() != 0) {
        close(recHnd[1]);
        return;
    }
    dup2(recHnd[1], STDOUT_FILENO);
    close(recHnd[0]);
    close(recHnd[1]);
    snprintf((char *)&bufD[0], 15, "%i", srate);
    snprintf((char *)&bufD[16], 15, "volume=%s", vol);
    snprintf((char *)&bufD[32], 15, "pcm_s%ile", 8*smpbt);
    snprintf((char *)&bufD[48], 15, "s%ile", 8*smpbt);
    execlp(
        "ffmpeg",
        "ffmpeg",
        "-hide_banner",
        "-loglevel", "quiet",
        "-ss", pos,
        "-re",
        "-i", fil,
        "-vn", "-sn",
        "-ar", &bufD[0],
        "-af", &bufD[16],
        "-ac", "2",
        "-c:a", &bufD[32],
        "-f", &bufD[48],
        "-",
        (char *)0);
    err("execl failed");
}


void iou_read() {
    bufS = read(recHnd[0], &bufD[padln], pktln);
}
