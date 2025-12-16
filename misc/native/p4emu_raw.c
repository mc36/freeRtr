#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <pthread.h>
#include <arpa/inet.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <poll.h>
#include <linux/if_ether.h>
#include <linux/if.h>
#include <linux/if_packet.h>
#include <sys/ioctl.h>


#include "p4emu_hdr.h"
#include "utils.h"


int ifaceIndex[maxPorts];
int ifaceSock[maxPorts];
struct sockaddr_ll addrIfc[maxPorts];

void sendPack(unsigned char *bufD, int bufS, int port) {
    sendto(ifaceSock[port], bufD, bufS, 0, (struct sockaddr *) &addrIfc[port], sizeof (addrIfc[port]));
}

void setMtu(int port, int mtu) {
}

void setState(int port, int sta) {
}

int getState(int port) {
    struct ifreq ifr;
    memset(&ifr, 0, sizeof (ifr));
    strcpy(ifr.ifr_name, ifaceName[port]);
    if (ioctl(ifaceSock[port], SIOCGIFFLAGS, &ifr) < 0) return 0;
    int needed = IFF_RUNNING | IFF_UP;
    if ((ifr.ifr_flags & needed) == needed) return 1;
    return 0;
}


void getStats(int port, unsigned char*buf, unsigned char*pre, int*len) {
}



void err(char*buf) {
    printf("%s\n", buf);
    _exit(1);
}




int ifaceId[maxPorts];







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




int main(int argc, char **argv) {

    dataPorts = 0;
    for (int i = 4; i < argc; i++) {
        initIface(dataPorts, argv[i]);
        dataPorts++;
    }
    if (dataPorts < 2) err("using: dp <addr> <port> <cpuport> <ifc0> <ifc1> [ifcN]");
    if (dataPorts > maxPorts) dataPorts = maxPorts;
    if (initTables() != 0) err("error initializing tables");
    int port = atoi(argv[2]);
    struct sockaddr_in addr;
    memset(&addr, 0, sizeof (addr));
    if (inet_aton(argv[1], &addr.sin_addr) == 0) err("bad addr address");
    addr.sin_family = AF_INET;
    addr.sin_port = htons(port);
    printf("connecting %s %i.\n", inet_ntoa(addr.sin_addr), port);
    commandSock = socket(AF_INET, SOCK_STREAM, 0);
    if (commandSock < 0) err("unable to open socket");
    if (connect(commandSock, (struct sockaddr*)&addr, sizeof(addr)) < 0) err("failed to connect socket");
    cpuPort = atoi(argv[3]);
    printf("cpu port is #%i of %i...\n", cpuPort, dataPorts);

    for (int o = 0; o < dataPorts; o++) {
        printf("opening interface %s\n", ifaceName[o]);
        if ((ifaceSock[o] = socket(PF_PACKET, SOCK_RAW, htons(ETH_P_ALL))) < 0) err("unable to open socket");
        struct ifreq ifr;
        memset(&ifr, 0, sizeof (ifr));
        strcpy(ifr.ifr_name, ifaceName[o]);
        if (ioctl(ifaceSock[o], SIOCGIFINDEX, &ifr) < 0) {
            if (o < (dataPorts-1)) err("unable to get ifcidx");
            dataPorts--;
            break;
        }
        ifaceIndex[o] = ifr.ifr_ifindex;
        memset(&addrIfc[o], 0, sizeof (addrIfc[o]));
        addrIfc[o].sll_family = AF_PACKET;
        addrIfc[o].sll_ifindex = ifaceIndex[o];
        addrIfc[o].sll_protocol = htons(ETH_P_ALL);
        if (bind(ifaceSock[o], (struct sockaddr *) &addrIfc[o], sizeof (addrIfc[o])) < 0) err("failed to bind socket");
        addrIfc[o].sll_pkttype = PACKET_OUTGOING;
        struct packet_mreq pmr;
        memset(&pmr, 0, sizeof (pmr));
        pmr.mr_ifindex = ifaceIndex[o];
        pmr.mr_type = PACKET_MR_PROMISC;
        if (setsockopt(ifaceSock[o], SOL_PACKET, PACKET_ADD_MEMBERSHIP, &pmr, sizeof (pmr)) < 0) err("failed to set promisc");
        int val = 1;
        if (setsockopt(ifaceSock[o], SOL_PACKET, PACKET_AUXDATA, &val, sizeof(val)) < 0) err("failed to set auxdata");
        ifaceId[o] = o;
    }

    doNegotiate("raw");
    pthread_t threadSock;
    if (pthread_create(&threadSock, NULL, (void*) & doSockLoop, NULL)) err("error creating socket thread");
    pthread_t threadStat;
    if (pthread_create(&threadStat, NULL, (void*) & doStatLoop, NULL)) err("error creating status thread");
    pthread_t threadRaw[maxPorts];
    for (int i=0; i < dataPorts; i++) {
        if (pthread_create(&threadRaw[i], NULL, (void*) & doIfaceLoop, &ifaceId[i])) err("error creating port thread");
    }

    doMainLoop();
}
