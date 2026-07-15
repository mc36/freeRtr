int recHnd;

void rec_init(char*grp, char*src, char* prt) {
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

void iou_read() {
    bufS = recv(recHnd, bufD, sizeof (bufD), 0);
    iou_bswp();
    bufS -= padln;
}
