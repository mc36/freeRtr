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
#include <liburing.h>


#include "p4emu_hdr.h"
#include "utils.h"


#define queueMax 64

#define cmsgLen (sizeof(struct cmsghdr) + sizeof(struct tpacket_auxdata) + sizeof(size_t))


int ifaceIndex[maxPorts];
int ifaceSock[maxPorts];
int ifaceIdx[maxPorts];
pthread_mutex_t ifaceLock[maxPorts];
struct io_uring ifaceRingRx[maxPorts];
struct msghdr *ifaceMsgRx[maxPorts];
struct iovec *ifaceIovRx[maxPorts];
unsigned char *ifaceMemRx[maxPorts];
unsigned char *ifaceAuxRx[maxPorts];
struct io_uring ifaceRingTx[maxPorts];
struct msghdr *ifaceMsgTx[maxPorts];
struct iovec *ifaceIovTx[maxPorts];
unsigned char *ifaceMemTx[maxPorts];
struct sockaddr_ll addrIfc[maxPorts];

void sendPack(unsigned char *bufD, int bufS, int port) {
    pthread_mutex_lock(&ifaceLock[port]);
    struct io_uring_sqe *sqe = io_uring_get_sqe(&ifaceRingTx[port]);
    if (sqe == NULL) {
        pthread_mutex_unlock(&ifaceLock[port]);
        return;
    }
    int idx = ifaceIdx[port] = (ifaceIdx[port] + 1) % queueMax;
    pthread_mutex_unlock(&ifaceLock[port]);
    memcpy(ifaceMemTx[port] + (idx * totBuff), bufD, bufS);
    ifaceIovTx[port][idx].iov_base = ifaceMemTx[port] + (idx * totBuff);
    ifaceIovTx[port][idx].iov_len = bufS;
    ifaceMsgTx[port][idx].msg_name = NULL;
    ifaceMsgTx[port][idx].msg_namelen = 0;
    ifaceMsgTx[port][idx].msg_iov = &ifaceIovTx[port][idx];
    ifaceMsgTx[port][idx].msg_iovlen = 1;
    ifaceMsgTx[port][idx].msg_control = NULL;
    ifaceMsgTx[port][idx].msg_controllen = 0;
    ifaceMsgTx[port][idx].msg_flags = 0;
    io_uring_prep_sendmsg(sqe, ifaceSock[port], &ifaceMsgTx[port][idx], 0);
    io_uring_sqe_set_data(sqe, ifaceMemTx[port] + (idx * totBuff));
    io_uring_submit(&ifaceRingTx[port]);
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



void prepReceive(struct io_uring_sqe *sqe, int prt, int idx) {
    ifaceIovRx[prt][idx].iov_base = ifaceMemRx[prt] + (idx * totBuff);
    ifaceIovRx[prt][idx].iov_len = totBuff;
    ifaceMsgRx[prt][idx].msg_name = NULL;
    ifaceMsgRx[prt][idx].msg_namelen = 0;
    ifaceMsgRx[prt][idx].msg_iov = &ifaceIovRx[prt][idx];
    ifaceMsgRx[prt][idx].msg_iovlen = 1;
    ifaceMsgRx[prt][idx].msg_control = ifaceAuxRx[prt] + (idx*cmsgLen);
    ifaceMsgRx[prt][idx].msg_controllen = cmsgLen;
    ifaceMsgRx[prt][idx].msg_flags = 0;
    io_uring_prep_recvmsg(sqe, ifaceSock[prt], &ifaceMsgRx[prt][idx], 0);
    io_uring_sqe_set_data(sqe, ifaceMemRx[prt] + (idx * totBuff));
    io_uring_submit(&ifaceRingRx[prt]);
}






void doIfaceLoop(int * param) {
    int port = *param;
    int bufS;
    struct io_uring_cqe *cqe;
    struct io_uring_sqe *sqe;
    struct packetContext ctx;
    if (initContext(&ctx) != 0) err("error initializing context");
    unsigned char *bufD = ctx.bufD;
    ctx.stat = ifaceStat[port];
    for (;;) {
        if (io_uring_wait_cqe(&ifaceRingRx[port], &cqe) != 0) break;
        bufS = cqe->res;
        if (bufS < 0) break;
        unsigned char *pack = io_uring_cqe_get_data(cqe);
        int idx = (pack - ifaceMemRx[port]) / totBuff;
        struct cmsghdr* cmsg = (struct cmsghdr*)(ifaceAuxRx[port] + (idx*cmsgLen));
        struct tpacket_auxdata* aux = (struct tpacket_auxdata*)CMSG_DATA(cmsg);
        if ((cmsg->cmsg_level == SOL_PACKET) && (cmsg->cmsg_type == PACKET_AUXDATA) && (aux->tp_status & TP_STATUS_VLAN_VALID)) {
            if ((aux->tp_status & TP_STATUS_VLAN_TPID_VALID) == 0) aux->tp_vlan_tpid = ETH_P_8021Q;
            memcpy(&bufD[preBuff], pack, 12);
            put16msb(bufD, preBuff + 12, aux->tp_vlan_tpid);
            put16msb(bufD, preBuff + 14, aux->tp_vlan_tci);
            memcpy(&bufD[preBuff + 16], pack + 12, bufS - 12);
            bufS += 4;
        } else memcpy(&bufD[preBuff], pack, bufS);
        io_uring_cqe_seen(&ifaceRingRx[port], cqe);
        sqe = io_uring_get_sqe(&ifaceRingRx[port]);
        if (sqe == NULL) break;
        prepReceive(sqe, port, idx);
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

        if (io_uring_queue_init(queueMax, &ifaceRingRx[o], 0) < 0) err("failed to init ring");
        ifaceMemRx[o] = malloc(queueMax * totBuff);
        if (ifaceMemRx[o] == NULL) err("error allocating memory");
        ifaceMsgRx[o] = malloc(queueMax * sizeof(struct msghdr));
        if (ifaceMsgRx[o] == NULL) err("error allocating memory");
        ifaceIovRx[o] = malloc(queueMax * sizeof(struct iovec));
        if (ifaceIovRx[o] == NULL) err("error allocating memory");
        ifaceAuxRx[o] = malloc(queueMax * cmsgLen);
        if (ifaceAuxRx[o] == NULL) err("error allocating memory");

        if (io_uring_queue_init(queueMax, &ifaceRingTx[o], 0) < 0) err("failed to init ring");
        ifaceMemTx[o] = malloc(queueMax * totBuff);
        if (ifaceMemTx[o] == NULL) err("error allocating memory");
        ifaceMsgTx[o] = malloc(queueMax * sizeof(struct msghdr));
        if (ifaceMsgTx[o] == NULL) err("error allocating memory");
        ifaceIovTx[o] = malloc(queueMax * sizeof(struct iovec));
        if (ifaceIovTx[o] == NULL) err("error allocating memory");

        for (int i=0; i < queueMax; i++) {
            struct io_uring_sqe *sqe = io_uring_get_sqe(&ifaceRingRx[o]);
            if (sqe == NULL) err("error getting sqe");
            prepReceive(sqe, o, i);
        }
        pthread_mutex_init(&ifaceLock[o], NULL);
        ifaceIdx[o] = 0;
        ifaceId[o] = o;
    }

    doNegotiate("uring");
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
