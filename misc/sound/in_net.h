int recHnd;
int recPrt;
void(*recFnc)();

OpusDecoder *recDec;
unsigned char recBufD[pktln * 8];
unsigned char recBufC[pktln * 4];
int recBufS;


void iou_read() {
    recFnc();
}



void rec_blk(int mod) {
    int flags = fcntl(recHnd, F_GETFL, 0);
    if (flags < 0) return;
    if (mod == 0) {
        flags |= O_NONBLOCK;
    } else {
        flags &= ~O_NONBLOCK;
    }
    fcntl(recHnd, F_SETFL, flags);
}


void rec_rtp() {
    for (;;) {
        bufS = recv(recHnd, &bufD[padln - rtpln], sizeof (bufD) - padln, 0);
        bufS -= rtpln;
        if (bufS < padln) return;
        if (bufD[padln - rtpln + 1] == rtpty) break;
    }
    iou_bswp2msb();
}


void rec_scr() {
    for (;;) {
        bufS = recv(recHnd, &bufD[padln - scrln], sizeof (bufD) - padln, 0);
        bufS -= scrln;
        if (bufS < padln) return;
        if (bufD[padln - scrln + 0] != scrbr) continue;
        if (bufD[padln - scrln + 1] != (smpbt * 8)) continue;
        if (bufD[padln - scrln + 3] == scrtp) break;
    }
    iou_bswp2lsb();
}


void rec_vban() {
    for (;;) {
        bufS = recv(recHnd, &bufD[padln - vbaln], sizeof (bufD) - padln, 0);
        bufS -= vbaln;
        if (bufS < padln) return;
        if (iou_gmsb(padln - vbaln + 0) != vbamg) continue;
        if (bufD[padln - vbaln + 4] != vbabr) continue;
        if (bufD[padln - vbaln + 7] == (smpbt - 1)) break;
    }
    iou_bswp2lsb();
}


void rec_wfas() {
    for (;;) {
        bufS = recv(recHnd, &bufD[padln - wfaln], sizeof (bufD) - padln, 0);
        bufS -= wfaln;
        if (bufS < padln) return;
        if (iou_gmsb(padln - wfaln + 0) == wfamg) break;
    }
    iou_bswp2lsb();
}


void rec_jcku() {
    for (;;) {
        bufS = recv(recHnd, &bufD[padln - jkuln], sizeof (bufD) - padln, 0);
        bufS -= jkuln;
        if (bufS < padln) return;
        if ((iou_gmsb(padln - jkuln + 4) >> 16) == 2) break;
    }
    iou_bswp2msb();
}


void rec_jckt() {
    for (;;) {
        bufS = recv(recHnd, &bufD[padln - jktln], sizeof (bufD) - padln, 0);
        bufS -= jktln;
        if (bufS < padln) return;
        if (iou_gmsb(padln - jktln + 12) == jktbr) break;
    }
    iou_unPlnr();
    iou_bswp2msb();
}


void rec_avtp() {
    for (;;) {
        bufS = recv(recHnd, &bufD[padln - avtln], sizeof (bufD) - padln, 0);
        bufS -= avtln;
        if (bufS < padln) return;
        if (iou_gmsb(padln - avtln + 12) != recPrt) continue;
        if (iou_gmsb(padln - avtln + 20) == avtbr) break;
    }
    iou_bswp2msb();
}


void rec_avb() {
    for (;;) {
        bufS = recv(recHnd, &bufD[padln - avbln], sizeof (bufD) - padln, 0);
        bufS -= avbln;
        if (bufS < padln) return;
        if (iou_gmsb(padln - avbln + 12) != avbmg) continue;
        if (iou_gmsb(padln - avbln + 22) != recPrt) continue;
        if (iou_gmsb(padln - avbln + 30) == avtbr) break;
    }
    iou_bswp2msb();
}


void rec_iec() {
    for (;;) {
        bufS = recv(recHnd, &bufD[padln - iecln], sizeof (bufD) - padln, 0);
        bufS -= iecln;
        if (bufS < padln) return;
        if (iou_gmsb(padln - iecln + 12) != recPrt) continue;
        if (iou_gmsb(padln - iecln + 28) != iec1q) continue;
        if (iou_gmsb(padln - iecln + 32) == iec2q) break;
    }
    iou_bswp2msb();
}


void rec_udpm() {
    bufS = recv(recHnd, &bufD[padln], sizeof (bufD) - padln, 0);
    iou_bswp2msb();
}


void rec_udpl() {
    bufS = recv(recHnd, &bufD[padln], sizeof (bufD) - padln, 0);
    iou_bswp2lsb();
}


void rec_opus() {
    short *bufV = (short *)&recBufD[0];
    if (recBufS < (pktln / smpbt)) {
        bufS = recv(recHnd, &recBufC[0], sizeof (recBufC), 0);
        if (bufS < 1) return;
        bufS = opus_decode(recDec, &recBufC[0], bufS, &bufV[recBufS], sizeof (recBufC), 0);
        if (bufS < 1) return;
        recBufS += bufS * 2;
    }
    bufS = 0;
    for (int p = 0; p < (pktln / smpbt); p++) {
        iou_psam(bufS, bufV[p] << 16);
        bufS += smpbt;
    }
    recBufS -= pktln / smpbt;
    memmove(&bufV[0], &bufV[pktln / smpbt], sizeof (short) * recBufS);
    iou_bswp2lsb();
}


void rec_init(char*knd, char*grp, char*src, char* prt) {
    recFnc = NULL;
    if (strcmp(knd,"rtp") == 0) recFnc = &rec_rtp;
    if (strcmp(knd,"scr") == 0) recFnc = &rec_scr;
    if (strcmp(knd,"vban") == 0) recFnc = &rec_vban;
    if (strcmp(knd,"wfas") == 0) recFnc = &rec_wfas;
    if (strcmp(knd,"jcku") == 0) recFnc = &rec_jcku;
    if (strcmp(knd,"jckt") == 0) recFnc = &rec_jckt;
    if (strcmp(knd,"avtp") == 0) recFnc = &rec_avtp;
    if (strcmp(knd,"avb") == 0) recFnc = &rec_avb;
    if (strcmp(knd,"iec") == 0) recFnc = &rec_iec;
    if (strcmp(knd,"udpm") == 0) recFnc = &rec_udpm;
    if (strcmp(knd,"udpl") == 0) recFnc = &rec_udpl;
    if (strcmp(knd,"opus") == 0) {
        recDec = opus_decoder_create(srate, 2, &recBufS);
        if (recDec == NULL) err("error creating");
        recBufS = 0;
        recFnc = &rec_opus;
    }
    if (recFnc == NULL) err("no such kind");
    recPrt = atoi(prt);
    struct sockaddr_in addrTmp;
    struct ip_mreq_source mcgrReq;
    memset(&addrTmp, 0, sizeof (addrTmp));
    memset(&mcgrReq, 0, sizeof (mcgrReq));
    addrTmp.sin_family = AF_INET;
    addrTmp.sin_addr.s_addr = htonl(INADDR_ANY);
    addrTmp.sin_port = htons(recPrt);
    if ((recHnd = socket(AF_INET, SOCK_DGRAM, IPPROTO_UDP)) < 0) err("unable to open socket");
    int val = 1;
    setsockopt(recHnd, SOL_SOCKET, SO_REUSEADDR, (void *)&val, sizeof(val));
    if (bind(recHnd, (struct sockaddr *) &addrTmp, sizeof (addrTmp)) < 0) err("failed to bind socket");
    val = 0;
    setsockopt(recHnd, IPPROTO_IP, IP_MULTICAST_ALL, (void *)&val, sizeof(val));
    mcgrReq.imr_multiaddr.s_addr = inet_addr(grp);
    mcgrReq.imr_interface.s_addr = htonl(INADDR_ANY);
    mcgrReq.imr_sourceaddr.s_addr = inet_addr(src);
    if (setsockopt(recHnd, IPPROTO_IP, IP_ADD_SOURCE_MEMBERSHIP, (char *)&mcgrReq, sizeof(mcgrReq)) == -1) err("error joining group");
    memset(&addrTmp, 0, sizeof (addrTmp));
    addrTmp.sin_family = AF_INET;
    addrTmp.sin_addr.s_addr = htonl(INADDR_ANY);
    if (setsockopt(recHnd, IPPROTO_IP, IP_MULTICAST_IF, (char *)&addrTmp, sizeof(addrTmp))== -1) err("error setting interface");
}
