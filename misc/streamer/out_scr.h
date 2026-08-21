void iou_write() {
    bufD[padln - scrln + 0] = scrbr;
    bufD[padln - scrln + 1] = smpbt * 8;
    bufD[padln - scrln + 2] = 2;
    bufD[padln - scrln + 3] = scrtp;
    bufD[padln - scrln + 4] = 0;
    bufS += scrln;
    if (send(plyHnd, &bufD[padln - scrln], bufS, 0) != bufS) err("error sending");
}

void iou_stop() {
}
