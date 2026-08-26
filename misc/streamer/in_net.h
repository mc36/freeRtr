int recHnd;
void(*recFnc)();


void iou_read() {
    recFnc();
}


void rec_rtp() {
    for (;;) {
        bufS = recv(recHnd, &bufD[padln - rtpln], sizeof (bufD) - padln, 0);
        if (bufS < padln) break;
        if (bufD[padln - rtpln + 1] == rtpty) break;
    }
    bufS -= rtpln;
    iou_bswp();
}


void rec_scr() {
    for (;;) {
        bufS = recv(recHnd, &bufD[padln - scrln], sizeof (bufD) - padln, 0);
        if (bufS < padln) break;
        if (bufD[padln - scrln + 0] != scrbr) continue;
        if (bufD[padln - scrln + 1] != (smpbt * 8)) continue;
        if (bufD[padln - scrln + 3] == scrtp) break;
    }
    bufS -= scrln;
}


void rec_vba() {
    for (;;) {
        bufS = recv(recHnd, &bufD[padln - vbaln], sizeof (bufD) - padln, 0);
        if (bufS < padln) break;
        if (iou_gmsb(padln - vbaln + 0) != vbamg) continue;
        if (bufD[padln - vbaln + 4] != vbabr) continue;
        if (bufD[padln - vbaln + 7] == (smpbt - 1)) break;
    }
    bufS -= vbaln;
}


void rec_wfa() {
    for (;;) {
        bufS = recv(recHnd, &bufD[padln - wfaln], sizeof (bufD) - padln, 0);
        if (bufS < padln) break;
        if (iou_gmsb(padln - wfaln + 0) == wfamg) break;
    }
    bufS -= wfaln;
}


void rec_udpm() {
    bufS = recv(recHnd, &bufD[padln], sizeof (bufD) - padln, 0);
    iou_bswp();
}


void rec_udpl() {
    bufS = recv(recHnd, &bufD[padln], sizeof (bufD) - padln, 0);
}


void rec_init(char*knd, char*grp, char*src, char* prt) {
    recFnc = NULL;
    if (strcmp(knd,"rtp") == 0) recFnc = &rec_rtp;
    if (strcmp(knd,"scr") == 0) recFnc = &rec_scr;
    if (strcmp(knd,"vba") == 0) recFnc = &rec_vba;
    if (strcmp(knd,"wfa") == 0) recFnc = &rec_wfa;
    if (strcmp(knd,"udpm") == 0) recFnc = &rec_udpm;
    if (strcmp(knd,"udpl") == 0) recFnc = &rec_udpl;
    if (recFnc == NULL) err("no such kind");
    struct sockaddr_in addrTmp;
    struct ip_mreq_source mcgrReq;
    memset(&addrTmp, 0, sizeof (addrTmp));
    memset(&mcgrReq, 0, sizeof (mcgrReq));
    addrTmp.sin_family = AF_INET;
    addrTmp.sin_addr.s_addr = htonl(INADDR_ANY);
    addrTmp.sin_port = htons(atoi(prt));
    if ((recHnd = socket(AF_INET, SOCK_DGRAM, IPPROTO_UDP)) < 0) err("unable to open socket");
    if (bind(recHnd, (struct sockaddr *) &addrTmp, sizeof (addrTmp)) < 0) err("failed to bind socket");
    mcgrReq.imr_multiaddr.s_addr = inet_addr(grp);
    mcgrReq.imr_interface.s_addr = htonl(INADDR_ANY);
    mcgrReq.imr_sourceaddr.s_addr = inet_addr(src);
    if (setsockopt(recHnd, IPPROTO_IP, IP_ADD_SOURCE_MEMBERSHIP, (char *)&mcgrReq, sizeof(mcgrReq)) == -1) err("error joining group");
    memset(&addrTmp, 0, sizeof (addrTmp));
    addrTmp.sin_family = AF_INET;
    addrTmp.sin_addr.s_addr = htonl(INADDR_ANY);
    if (setsockopt(recHnd, IPPROTO_IP, IP_MULTICAST_IF, (char *)&addrTmp, sizeof(addrTmp))== -1) err("error setting interface");
}
