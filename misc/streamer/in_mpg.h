int recHnd[2];

void rec_init(char*fil, char*pos) {
    if (pipe(recHnd)==-1) err("cannot create pipe");
    if (fork() != 0) {
        close(recHnd[1]);
        return;
    }
    dup2(recHnd[1], STDOUT_FILENO);
    close(recHnd[0]);
    close(recHnd[1]);
    snprintf((char *)&bufD[0], 15, "%i", srate);
    snprintf((char *)&bufD[16], 15, "pcm_s%ile", 8*smpbt);
    snprintf((char *)&bufD[32], 15, "s%ile", 8*smpbt);
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
        "-ac", "2",
        "-c:a", &bufD[16],
        "-f", &bufD[32],
        "-",
        (char *)0);
    err("execl failed");
}


void iou_read() {
    bufS = read(recHnd[0], &bufD[padln], pktln);
}
