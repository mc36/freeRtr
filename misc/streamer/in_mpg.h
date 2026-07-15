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
    snprintf((char *)&bufD, sizeof(bufD)-1, "%i", srate);
    execlp(
        "ffmpeg",
        "ffmpeg",
        "-hide_banner",
        "-loglevel", "quiet",
        "-ss", pos,
        "-re",
        "-i", fil,
        "-vn", "-sn",
        "-ar", bufD,
        "-ac", "2",
        "-c:a", "pcm_s16le",
        "-f", "s16le",
        "-",
        (char *)0);
    err("execl failed");
}


void iou_read() {
    bufS = read(recHnd[0], &bufD[padln], pktln);
}
