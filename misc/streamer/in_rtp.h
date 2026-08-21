void iou_read() {
    for (;;) {
        bufS = recv(recHnd, &bufD[padln - rtpln], sizeof (bufD) - padln, 0);
        if (bufS < padln) break;
        if (bufD[padln - rtpln + 1] == rtpty) break;
    }
    bufS -= rtpln;
    iou_bswp();
}
