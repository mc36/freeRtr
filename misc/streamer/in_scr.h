void iou_read() {
    for (;;) {
        bufS = recv(recHnd, &bufD[padln - scrln], sizeof (bufD) - padln, 0);
        if (bufS < padln) break;
        if (bufD[padln - scrln + 0] == scrbr) break;
        if (bufD[padln - scrln + 1] == (smpbt * 8)) break;
        if (bufD[padln - scrln + 3] == scrtp) break;
    }
    bufS -= scrln;
}
