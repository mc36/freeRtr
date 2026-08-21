void iou_read() {
    for (;;) {
        bufS = recv(recHnd, &bufD[padln - vbaln], sizeof (bufD) - padln, 0);
        if (bufS < padln) break;
        if (iou_gmsb(padln - vbaln + 0) != vbamg) continue;
        if (bufD[padln - vbaln + 4] != vbabr) continue;
        if (bufD[padln - vbaln + 7] == (smpbt - 1)) break;
    }
    bufS -= vbaln;
}
