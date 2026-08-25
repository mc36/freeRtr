int plyHnd;
int plySeq;
int plySrc;
int plyClk;
void(*plyFnc)();


void iou_write() {
    plyFnc();
}


void ply_udp() {
    iou_bswp();
    if (send(plyHnd, &bufD[padln], bufS, 0) != bufS) err("error sending");
}


void ply_rtp() {
    iou_bswp();
    iou_pmsb(padln - rtpln + 0, 0x80000000 | (rtpty << 16) | plySeq);
    iou_pmsb(padln - rtpln + 4, plyClk);
    iou_pmsb(padln - rtpln + 8, plySrc);
    plySeq = (plySeq + 1) & 0xffff;
    plyClk += bufS / (2 * smpbt);
    bufS += rtpln;
    if (send(plyHnd, &bufD[padln - rtpln], bufS, 0) != bufS) err("error sending");
}


void ply_scr() {
    bufD[padln - scrln + 0] = scrbr;
    bufD[padln - scrln + 1] = smpbt * 8;
    bufD[padln - scrln + 2] = 2;
    bufD[padln - scrln + 3] = scrtp;
    bufD[padln - scrln + 4] = 0;
    bufS += scrln;
    if (send(plyHnd, &bufD[padln - scrln], bufS, 0) != bufS) err("error sending");
}


void ply_vba() {
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


void ply_wfa() {
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


void ply_init(char*knd, char*grp, char*src, char* prt) {
    plyFnc = NULL;
    if (strcmp(knd,"rtp") == 0) plyFnc = &ply_rtp;
    if (strcmp(knd,"scr") == 0) plyFnc = &ply_scr;
    if (strcmp(knd,"vba") == 0) plyFnc = &ply_vba;
    if (strcmp(knd,"wfa") == 0) plyFnc = &ply_wfa;
    if (strcmp(knd,"udp") == 0) plyFnc = &ply_udp;
    if (plyFnc == NULL) err("no such kind");
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
    plySrc = (46 << 2) & 0xfc;
    if (setsockopt(plyHnd, IPPROTO_IP, IP_TOS, &plySrc, sizeof(plySrc)) < 0) err("failed to set dscp");
    srand(getpid());
    plySeq = 0;
    plySrc = rand();
    plyClk = 0;
}
