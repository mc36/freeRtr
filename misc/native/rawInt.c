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

#include "utils.h"



char *ifaceName;
int ifaceIndex;
int ifaceSock;
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

void doRawLoop() {
    unsigned char bufD[16384];
    int bufS;
    unsigned char cbuf[sizeof(struct cmsghdr) + sizeof(struct tpacket_auxdata) + sizeof(size_t)];
    struct iovec iov;
    struct msghdr msg;
    iov.iov_base = &bufD;
    iov.iov_len = sizeof(bufD);
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
        bufS = recvmsg(ifaceSock, &msg, 0);
        if (bufS < 0) break;
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
    }
    err("raw thread exited");
}

void doUdpLoop() {
    unsigned char bufD[16384];
    int bufS;
    for (;;) {
        bufS = sizeof (bufD);
        bufS = recv(commSock, bufD, bufS, 0);
        if (bufS < 0) break;
        packTx++;
        byteTx += bufS;
        sendto(ifaceSock, bufD, bufS, 0, (struct sockaddr *) &addrIfc, sizeof (addrIfc));
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
            err("raw interface driver v1.0\n");
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
