int plyHnd;

void ply_init(char*fil) {
    plyHnd = open(fil, O_WRONLY | O_CREAT);
    if (plyHnd < 0) err("error creating");
}

void iou_write() {
    if (write(plyHnd, &bufD[padln], bufS) != bufS) err("error writing");
}

void iou_stop() {
    close(plyHnd);
}
