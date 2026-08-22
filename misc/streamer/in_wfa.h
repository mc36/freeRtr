void iou_read() {
    for (;;) {
        bufS = recv(recHnd, &bufD[padln - wfaln], sizeof (bufD) - padln, 0);
        if (bufS < padln) break;
        if (iou_gmsb(padln - wfaln + 0) == wfamg) break;
    }
    bufS -= wfaln;
}
