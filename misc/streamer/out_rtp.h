void iou_write() {
    bufS += padln;
    iou_bswp();
    iou_pmsb(0, 0x80000000 | (payty << 16) | plySeq);
    iou_pmsb(4, plyClk);
    iou_pmsb(8, plySrc);
    plySeq = (plySeq + 1) & 0xffff;
    plyClk += (bufS - padln) / (2 * smpbt);
    if (send(plyHnd, bufD, bufS, 0) != bufS) err("error sending");
}

void iou_stop() {
}
