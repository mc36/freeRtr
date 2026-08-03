#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <pthread.h>
#include <arpa/inet.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <linux/if_ether.h>
#include <linux/if.h>
#include <linux/if_packet.h>
#include <sys/ioctl.h>
#include <liburing.h>

#include "utils.h"


#define queueMax 64

#define cmsgLen (sizeof(struct cmsghdr) + sizeof(struct tpacket_auxdata) + sizeof(size_t))

char *ifaceName;
int ifaceIndex;
int ifaceSock;
struct io_uring ifaceRingRx;
struct msghdr *ifaceMsgRx;
struct iovec *ifaceIovRx;
unsigned char *ifaceMemRx;
unsigned char *ifaceAuxRx;
struct io_uring ifaceRingTx;
struct msghdr *ifaceMsgTx;
struct iovec *ifaceIovTx;
unsigned char *ifaceMemTx;
struct sockaddr_in addrLoc;
struct sockaddr_in addrRem;
struct sockaddr_ll addrIfc;
int portLoc;
int portRem;
int commSock;
pthread_t threadUdp;
pthread_t threadRaw;
pthread_t threadStat;
long byteRx;
long packRx;
long byteTx;
long packTx;

void err(char*buf) {
    printf("%s\n", buf);
    _exit(1);
}


void prepReceive(struct io_uring_sqe *sqe, int idx) {
    ifaceIovRx[idx].iov_base = ifaceMemRx + (idx * 16384);
    ifaceIovRx[idx].iov_len = 16384;
    ifaceMsgRx[idx].msg_name = NULL;
    ifaceMsgRx[idx].msg_namelen = 0;
    ifaceMsgRx[idx].msg_iov = &ifaceIovRx[idx];
    ifaceMsgRx[idx].msg_iovlen = 1;
    ifaceMsgRx[idx].msg_control = ifaceAuxRx + (idx*cmsgLen);
    ifaceMsgRx[idx].msg_controllen = cmsgLen;
    ifaceMsgRx[idx].msg_flags = 0;
    io_uring_prep_recvmsg(sqe, ifaceSock, &ifaceMsgRx[idx], 0);
    io_uring_sqe_set_data(sqe, ifaceMemRx + (idx * 16384));
    io_uring_submit(&ifaceRingRx);
}


void doRawLoop() {
    struct io_uring_cqe *cqe;
    struct io_uring_sqe *sqe;
    for (;;) {
        if (io_uring_wait_cqe(&ifaceRingRx, &cqe) != 0) break;
        int bufS = cqe->res;
        if (bufS < 0) break;
        unsigned char *bufD = io_uring_cqe_get_data(cqe);
        int idx = (bufD - ifaceMemRx) / 16384;
        struct cmsghdr* cmsg = (struct cmsghdr*)(ifaceAuxRx + (idx*cmsgLen));
        struct tpacket_auxdata* aux = (struct tpacket_auxdata*)CMSG_DATA(cmsg);
        if ((cmsg->cmsg_level == SOL_PACKET) && (cmsg->cmsg_type == PACKET_AUXDATA) && (aux->tp_status & TP_STATUS_VLAN_VALID)) {
            if ((aux->tp_status & TP_STATUS_VLAN_TPID_VALID) == 0) aux->tp_vlan_tpid = ETH_P_8021Q;
            bufS += 4;
            memmove(&bufD[16], &bufD[12], bufS - 12);
            put16msb(bufD, 12, aux->tp_vlan_tpid);
            put16msb(bufD, 14, aux->tp_vlan_tci);
        }
        packRx++;
        byteRx += bufS;
        send(commSock, bufD, bufS, 0);
        io_uring_cqe_seen(&ifaceRingRx, cqe);
        sqe = io_uring_get_sqe(&ifaceRingRx);
        if (sqe == NULL) break;
        prepReceive(sqe, idx);
    }
    err("raw thread exited");
}

void doUdpLoop() {
    struct io_uring_sqe *sqe;
    unsigned char bufD[16384];
    int bufS;
    int idx = 0;
    for (;;) {
        bufS = sizeof (bufD);
        bufS = recv(commSock, bufD, bufS, 0);
        if (bufS < 0) break;
        packTx++;
        byteTx += bufS;
        sqe = io_uring_get_sqe(&ifaceRingTx);
        if (sqe == NULL) break;
        idx = (idx + 1) % queueMax;
        memcpy(ifaceMemTx + (idx * 16384), bufD, bufS);
        ifaceIovTx[idx].iov_base = ifaceMemTx + (idx * 16384);
        ifaceIovTx[idx].iov_len = bufS;
        ifaceMsgTx[idx].msg_name = NULL;
        ifaceMsgTx[idx].msg_namelen = 0;
        ifaceMsgTx[idx].msg_iov = &ifaceIovTx[idx];
        ifaceMsgTx[idx].msg_iovlen = 1;
        ifaceMsgTx[idx].msg_control = NULL;
        ifaceMsgTx[idx].msg_controllen = 0;
        ifaceMsgTx[idx].msg_flags = 0;
        io_uring_prep_sendmsg(sqe, ifaceSock, &ifaceMsgTx[idx], 0);
        io_uring_sqe_set_data(sqe, ifaceMemTx + (idx * 16384));
        io_uring_submit(&ifaceRingTx);
    }
    err("udp thread exited");
}

void doStatLoop() {
    struct ifreq ifr;
    unsigned char buf[1];
    int needed = IFF_RUNNING | IFF_UP;
    for (;;) {
        sleep(1);
        memset(&ifr, 0, sizeof (ifr));
        strcpy(ifr.ifr_name, ifaceName);
        if (ioctl(ifaceSock, SIOCGIFFLAGS, &ifr) < 0) break;
        if ((ifr.ifr_flags & needed) == needed) buf[0] = 1;
        else buf[0] = 0;
        sendto(commSock, buf, 1, 0, (struct sockaddr *) &addrRem, sizeof (addrRem));
    }
    err("stat thread exited");
}

void doMainLoop() {
    unsigned char buf[1024];

doer:
    printf("> ");
    buf[0] = 0;
    int i = scanf("%1023s", buf);
    if (i < 1) {
        sleep(1);
        goto doer;
    }
    switch (buf[0]) {
    case 0:
        goto doer;
        break;
    case 'H':
    case 'h':
    case '?':
        printf("commands:\n");
        printf("h - this help\n");
        printf("q - exit process\n");
        printf("d - display counters\n");
        printf("c - clear counters\n");
        break;
    case 'Q':
    case 'q':
        err("exiting");
        break;
    case 'D':
    case 'd':
        printf("iface counters:\n");
        printf("                      packets                bytes\n");
        printf("received %20li %20li\n", packRx, byteRx);
        printf("sent     %20li %20li\n", packTx, byteTx);
        break;
    case 'C':
    case 'c':
        printf("counters cleared.\n");
        byteRx = 0;
        packRx = 0;
        byteTx = 0;
        packTx = 0;
        break;
    default:
        printf("unknown command '%s', try ?\n", buf);
        break;
    }
    printf("\n");

    goto doer;
}

int main(int argc, char **argv) {

    if (argc < 5) {
        if (argc <= 1) goto help;
        char*curr = argv[1];
        if ((curr[0] == '-') || (curr[0] == '/')) curr++;
        switch (curr[0]) {
        case 'V':
        case 'v':
            err("uring interface driver v1.0\n");
            break;
        case '?':
        case 'h':
        case 'H':
help :
            curr = argv[0];
            printf("using: %s <iface> <lport> <raddr> <rport> [laddr]\n", curr);
            printf("   or: %s <command>\n", curr);
            printf("commands: v=version\n");
            printf("          h=this help\n");
            _exit(1);
            break;
        default:
            err("unknown command, try -h");
            break;
        }
        _exit(1);
    }

    portLoc = atoi(argv[2]);
    portRem = atoi(argv[4]);
    memset(&addrLoc, 0, sizeof (addrLoc));
    memset(&addrRem, 0, sizeof (addrRem));
    if (inet_aton(argv[3], &addrRem.sin_addr) == 0) err("bad raddr address");
    if (argc > 5) {
        if (inet_aton(argv[5], &addrLoc.sin_addr) == 0) err("bad laddr address");
    } else {
        addrLoc.sin_addr.s_addr = htonl(INADDR_ANY);
    }
    addrLoc.sin_family = AF_INET;
    addrLoc.sin_port = htons(portLoc);
    addrRem.sin_family = AF_INET;
    addrRem.sin_port = htons(portRem);

    if ((commSock = socket(AF_INET, SOCK_DGRAM, IPPROTO_UDP)) < 0) err("unable to udp open socket");
    if (bind(commSock, (struct sockaddr *) &addrLoc, sizeof (addrLoc)) < 0) err("failed to bind socket");
    printf("binded to local port %s %i.\n", inet_ntoa(addrLoc.sin_addr), portLoc);
    if (connect(commSock, (struct sockaddr *) &addrRem, sizeof (addrRem)) < 0) err("failed to connect socket");
    printf("will send to %s %i.\n", inet_ntoa(addrRem.sin_addr), portRem);
    int sockOpt = 524288;
    setsockopt(commSock, SOL_SOCKET, SO_RCVBUF, &sockOpt, sizeof(sockOpt));
    setsockopt(commSock, SOL_SOCKET, SO_SNDBUF, &sockOpt, sizeof(sockOpt));

    ifaceName = malloc(strlen(argv[1]) + 1);
    if (ifaceName == NULL) err("error allocating memory");
    strcpy(ifaceName, argv[1]);
    printf("opening interface %s.\n", ifaceName);

    if ((ifaceSock = socket(PF_PACKET, SOCK_RAW, htons(ETH_P_ALL))) < 0) err("unable to raw open socket");
    struct ifreq ifr;
    memset(&ifr, 0, sizeof (ifr));
    strcpy(ifr.ifr_name, ifaceName);
    if (ioctl(ifaceSock, SIOCGIFINDEX, &ifr) < 0) err("unable to get ifcidx");
    ifaceIndex = ifr.ifr_ifindex;
    memset(&addrIfc, 0, sizeof (addrIfc));
    addrIfc.sll_family = AF_PACKET;
    addrIfc.sll_ifindex = ifaceIndex;
    addrIfc.sll_protocol = htons(ETH_P_ALL);
    if (bind(ifaceSock, (struct sockaddr *) &addrIfc, sizeof (addrIfc)) < 0) err("failed to bind socket");
    addrIfc.sll_pkttype = PACKET_OUTGOING;
    struct packet_mreq pmr;
    memset(&pmr, 0, sizeof (pmr));
    pmr.mr_ifindex = ifaceIndex;
    pmr.mr_type = PACKET_MR_PROMISC;
    if (setsockopt(ifaceSock, SOL_PACKET, PACKET_ADD_MEMBERSHIP, &pmr, sizeof (pmr)) < 0) err("failed to set promisc");

    int val = 1;
    if (setsockopt(ifaceSock, SOL_PACKET, PACKET_AUXDATA, &val, sizeof(val)) < 0) err("failed to set auxdata");

    if (io_uring_queue_init(queueMax, &ifaceRingRx, 0) < 0) err("failed to init ring");
    ifaceMemRx = malloc(queueMax * 16384);
    if (ifaceMemRx == NULL) err("error allocating memory");
    ifaceMsgRx = malloc(queueMax * sizeof(struct msghdr));
    if (ifaceMsgRx == NULL) err("error allocating memory");
    ifaceIovRx = malloc(queueMax * sizeof(struct iovec));
    if (ifaceIovRx == NULL) err("error allocating memory");
    ifaceAuxRx = malloc(queueMax * cmsgLen);
    if (ifaceAuxRx == NULL) err("error allocating memory");

    if (io_uring_queue_init(queueMax, &ifaceRingTx, 0) < 0) err("failed to init ring");
    ifaceMemTx = malloc(queueMax * 16384);
    if (ifaceMemTx == NULL) err("error allocating memory");
    ifaceMsgTx = malloc(queueMax * sizeof(struct msghdr));
    if (ifaceMsgTx == NULL) err("error allocating memory");
    ifaceIovTx = malloc(queueMax * sizeof(struct iovec));
    if (ifaceIovTx == NULL) err("error allocating memory");

    for (int i=0; i < queueMax; i++) {
        struct io_uring_sqe *sqe = io_uring_get_sqe(&ifaceRingRx);
        if (sqe == NULL) err("error getting sqe");
        prepReceive(sqe, i);
    }

    setgid(1);
    setuid(1);
    printf("serving others\n");

    byteRx = 0;
    packRx = 0;
    byteTx = 0;
    packTx = 0;
    if (pthread_create(&threadRaw, NULL, (void*) & doRawLoop, NULL)) err("error creating raw thread");
    if (pthread_create(&threadUdp, NULL, (void*) & doUdpLoop, NULL)) err("error creating udp thread");
    if (pthread_create(&threadStat, NULL, (void*) & doStatLoop, NULL)) err("error creating stat thread");

    doMainLoop();
}
