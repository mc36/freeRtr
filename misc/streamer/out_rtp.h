void iou_write() {
    iou_bswp();
    bufS += rtpln;
    iou_pmsb(padln - rtpln + 0, 0x80000000 | (rtpty << 16) | plySeq);
    iou_pmsb(padln - rtpln + 4, plyClk);
    iou_pmsb(padln - rtpln + 8, plySrc);
    plySeq = (plySeq + 1) & 0xffff;
    plyClk += (bufS - padln) / (2 * smpbt);
    if (send(plyHnd, &bufD[padln - rtpln], bufS, 0) != bufS) err("error sending");
}

void iou_stop() {
}
