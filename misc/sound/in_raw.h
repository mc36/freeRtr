int recHnd;

void rec_init(char*fil) {
    recHnd = open(fil, O_RDONLY);
    if (recHnd < 0) err("error opening");
    lseek(recHnd, wavln, SEEK_SET);
}

void iou_read() {
    bufS = read(recHnd, &bufD[padln], pktln);
}
