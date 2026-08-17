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
    plySrc = (46 << 2) & 0xfc;
    if (setsockopt(plyHnd, IPPROTO_IP, IP_TOS, &plySrc, sizeof(plySrc)) < 0) err("failed to set dscp");
    srand(getpid());
    plySeq = 0;
    plySrc = rand();
    plyClk = 0;
}
