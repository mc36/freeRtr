int plyHnd;
int plySeq;
int plySrc;
int plyClk;

void ply_init(char*grp, char*src, char* prt) {
    struct sockaddr_in addrTmp;
    memset(&addrTmp, 0, sizeof (addrTmp));
    addrTmp.sin_family = AF_INET;
    addrTmp.sin_addr.s_addr = inet_addr(src);
    addrTmp.sin_port = htons(atoi(prt));
    if ((plyHnd = socket(AF_INET, SOCK_DGRAM, IPPROTO_UDP)) < 0) err("unable to open socket");
    if (bind(plyHnd, (struct sockaddr *) &addrTmp, sizeof (addrTmp)) < 0) err("failed to bind socket");
    memset(&addrTmp, 0, sizeof (addrTmp));
    addrTmp.sin_family = AF_INET;
    addrTmp.sin_addr.s_addr = inet_addr(grp);
    addrTmp.sin_port = htons(atoi(prt));
    if (connect(plyHnd, (struct sockaddr *) &addrTmp, sizeof (addrTmp)) < 0) err("failed to connect socket");
    plySrc = 255;
    if (setsockopt(plyHnd, IPPROTO_IP, IP_MULTICAST_TTL, &plySrc, sizeof(plySrc)) < 0) err("failed to set ttl");
    plySeq = 0;
    plySrc = rand();
    plyClk = 0;
}

void iou_write() {
    bufS += padln;
    iou_bswp();
    int* bufH = (int*) &bufD;
    iou_pmsb(0, 0x800a0000 | plySeq);
    iou_pmsb(4, plyClk);
    iou_pmsb(8, plySrc);
    plySeq = (plySeq + 1) & 0xffff;
    plyClk += (bufS - padln) >> 2;
    if (send(plyHnd, bufD, bufS, 0) != bufS) err("error sending");
}

void iou_stop() {
}
