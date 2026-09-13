int timLst;

void iou_timr() {
    struct timeval timval;
    gettimeofday(&timval, NULL);
    int need = (timval.tv_usec - timLst + 1000000) % 100000;
    timLst = timval.tv_usec;
    need = (500000 * pktln / (srate * smpbt)) - need;
    if (need < 1) return;
    usleep(need);
    timLst += need;
}
