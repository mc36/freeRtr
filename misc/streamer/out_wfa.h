void iou_write() {
    iou_pmsb(padln - wfaln + 0, wfamg);
    iou_pmsb(padln - wfaln + 2, ((wfamg & 0xffff) << 16) | plySeq);
    iou_pmsb(padln - wfaln + 6, plyClk);
    plySeq = (plySeq + 1) & 0xffff;
    plyClk += bufS / (2 * smpbt);
    bufS += wfaln;
    if (send(plyHnd, &bufD[padln - wfaln], bufS, 0) != bufS) err("error sending");
}

void iou_stop() {
}
