void iou_read() {
    for (;;) {
        bufS = recv(recHnd, bufD, sizeof (bufD), 0);
        if (bufS < padln) break;
        if (bufD[1] == payty) break;
    }
    iou_bswp();
    bufS -= padln;
}
