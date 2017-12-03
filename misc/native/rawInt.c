#include <arpa/inet.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <netinet/in.h>
#include <pthread.h>
#include <unistd.h>
#include <linux/if_ether.h>
#include <linux/if.h>
#include <linux/if_packet.h>
#include <sys/ioctl.h>



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
pthread_t threadPrint;
long int byteRx;
long int packRx;
long int byteTx;
long int packTx;

void err(char*buf) {
    printf("%s\n", buf);
    exit(1);
}

void doRawLoop() {
    unsigned char bufD[16384];
    int bufS;
    struct sockaddr_in addrTmp;
    unsigned int addrLen;
    for (;;) {
        addrLen = sizeof (addrTmp);
        bufS = sizeof (bufD);
        bufS = recvfrom(ifaceSock, bufD, bufS, 0, (struct sockaddr *) &addrTmp, &addrLen);
        if (bufS < 0) break;
        packRx++;
        byteRx += bufS;
        sendto(commSock, bufD, bufS, 0, (struct sockaddr *) &addrRem, sizeof (addrRem));
    }
    err("raw thread exited");
}

void doUdpLoop() {
    unsigned char bufD[16384];
    int bufS;
    struct sockaddr_in addrTmp;
    unsigned int addrLen;
    for (;;) {
        addrLen = sizeof (addrTmp);
        bufS = sizeof (bufD);
        bufS = recvfrom(commSock, bufD, bufS, 0, (struct sockaddr *) &addrTmp, &addrLen);
        if (bufS < 0) break;
        packTx++;
        byteTx += bufS;
        sendto(ifaceSock, bufD, bufS, 0, (struct sockaddr *) &addrIfc, sizeof (addrIfc));
    }
    err("udp thread exited");
}

void doStatLoop() {
    struct ifreq ifr;
    char buf[1];
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
    char buf[1024];

doer:
    printf("> ");
    buf[0] = 0;
    int i = scanf("%s", buf);
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
                exit(1);
                break;
            default:
                err("unknown command, try -h");
                break;
        }
        exit(1);
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

    if ((commSock = socket(AF_INET, SOCK_DGRAM, IPPROTO_UDP)) < 0) err("unable to open socket");
    if (bind(commSock, (struct sockaddr *) &addrLoc, sizeof (addrLoc)) < 0) err("failed to bind socket");
    printf("binded to local port %s %i.\n", inet_ntoa(addrLoc.sin_addr), portLoc);
    printf("will send to %s %i.\n", inet_ntoa(addrRem.sin_addr), portRem);

    ifaceName = malloc(1024);
    strcpy(ifaceName, argv[1]);
    printf("opening interface %s.\n", ifaceName);

    if ((ifaceSock = socket(PF_PACKET, SOCK_RAW, htons(ETH_P_ALL))) < 0) err("unable to open socket");
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
