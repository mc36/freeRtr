void iou_write() {
    iou_pmsb(padln - vbaln + 0, vbamg);
    bufD[padln - vbaln + 4] = vbabr;
    bufD[padln - vbaln + 5] = (bufS  / (2 * smpbt)) - 1;
    bufD[padln - vbaln + 6] = 1;
    bufD[padln - vbaln + 7] = smpbt - 1;
    iou_pmsb(padln - vbaln + 8, 0x6e6f6e65);
    iou_pmsb(padln - vbaln + 12, 0);
    iou_pmsb(padln - vbaln + 16, 0);
    iou_pmsb(padln - vbaln + 20, 0);
    iou_pmsb(padln - vbaln + 24, plySeq);
    plySeq++;
    bufS += vbaln;
    if (send(plyHnd, &bufD[padln - vbaln], bufS, 0) != bufS) err("error sending");
}

void iou_stop() {
}
