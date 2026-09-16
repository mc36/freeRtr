int ifaceIndex[maxPorts];
int ifaceSock[maxPorts];
int ifaceId[maxPorts];
struct sockaddr_ll addrIfc[maxPorts];


void sendPack(unsigned char *bufD, int bufS, int port) {
    send(ifaceSock[port], bufD, bufS, 0);
}

void setMtu(int port, int mtu) {
}

void setState(int port, int sta) {
}


void getStats(int port, unsigned char*buf, unsigned char*pre, int*len) {
}


void err(char*buf) {
    printf("%s\n", buf);
    _exit(1);
}


void doIfaceLoop(int * param) {
    int port = *param;
    int bufS;
    unsigned char cbuf[sizeof(struct cmsghdr) + sizeof(struct tpacket_auxdata) + sizeof(size_t)];
    struct iovec iov;
    struct msghdr msg;
    struct packetContext ctx;
    if (initContext(&ctx) != 0) err("error initializing context");
    unsigned char *bufD = ctx.bufD;
    ctx.stat = ifaceStat[port];
    iov.iov_base = &bufD[preBuff];
    iov.iov_len = totBuff - preBuff;
    msg.msg_name = NULL;
    msg.msg_namelen = 0;
    msg.msg_iov = &iov;
    msg.msg_iovlen = 1;
    msg.msg_control = cbuf;
    msg.msg_controllen = sizeof(cbuf);
    msg.msg_flags = 0;
    struct cmsghdr* cmsg = (struct cmsghdr*)cbuf;
    struct tpacket_auxdata* aux = (struct tpacket_auxdata*)CMSG_DATA(cmsg);
    for (;;) {
        aux->tp_status = 0;
        bufS = recvmsg(ifaceSock[port], &msg, 0);
        if (bufS < 0) break;
        if ((cmsg->cmsg_level == SOL_PACKET) && (cmsg->cmsg_type == PACKET_AUXDATA) && (aux->tp_status & TP_STATUS_VLAN_VALID)) {
            if ((aux->tp_status & TP_STATUS_VLAN_TPID_VALID) == 0) aux->tp_vlan_tpid = ETH_P_8021Q;
            bufS += 4;
            memmove(&bufD[preBuff + 16], &bufD[preBuff + 12], bufS - 12);
            put16msb(bufD, preBuff + 12, aux->tp_vlan_tpid);
            put16msb(bufD, preBuff + 14, aux->tp_vlan_tci);
        }
        processDataPacket(&ctx, bufS, port);
    }
    err("port thread exited");
}
