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
#include <linux/if_tun.h>
#include <sys/ioctl.h>
#include <sys/fcntl.h>
#include "utils.h"

char *ifaceName;
int ifaceSock;
struct sockaddr_in addrLoc;
struct sockaddr_in addrRem;
struct sockaddr_in ifaceAddr;
struct sockaddr_in ifaceMask;
struct sockaddr ifaceMac;
int portLoc;
int portRem;
int commSock;
pthread_t threadUdp;
pthread_t threadTap;
pthread_t threadPrint;
long int byteRx;
long int packRx;
long int byteTx;
long int packTx;

void err(char*buf) {
    printf("%s\n", buf);
    exit(1);
}

void doTapLoop() {
    unsigned char bufD[16384];
    int bufS;
    struct sockaddr_in addrTmp;
    for (;;) {
        bufS = sizeof (bufD);
        bufS = read(ifaceSock, bufD, bufS);
        if (bufS < 0) break;
        packRx++;
        byteRx += bufS;
        sendto(commSock, bufD, bufS, 0, (struct sockaddr *) &addrRem, sizeof (addrRem));
    }
    err("tap thread exited");
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
        write(ifaceSock, bufD, bufS);
    }
    err("udp thread exited");
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

    if (argc < 8) {
        if (argc <= 1) goto help;
        char*curr = argv[1];
        if ((curr[0] == '-') || (curr[0] == '/')) curr++;
        switch (curr[0]) {
            case 'V':
            case 'v':
                err("tap interface driver v1.0\n");
                break;
            case '?':
            case 'h':
            case 'H':
                help :
                        curr = argv[0];
                printf("using: %s <iface> <lport> <raddr> <rport> <laddr> <addr> <mask>\n", curr);
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
    if (inet_aton(argv[5], &addrLoc.sin_addr) == 0) err("bad laddr address");
    addrLoc.sin_family = AF_INET;
    addrLoc.sin_port = htons(portLoc);
    addrRem.sin_family = AF_INET;
    addrRem.sin_port = htons(portRem);
    memset(&ifaceAddr, 0, sizeof (ifaceAddr));
    memset(&ifaceMask, 0, sizeof (ifaceMask));
    if (inet_aton(argv[6], &ifaceAddr.sin_addr) == 0) err("bad network address");
    if (inet_aton(argv[7], &ifaceMask.sin_addr) == 0) err("bad netmask address");
    ifaceAddr.sin_family = AF_INET;
    ifaceMask.sin_family = AF_INET;
    memset(&ifaceMac, 0, sizeof (ifaceMac));
    put16bits(ifaceMac.sa_data, 2, portLoc);
    put16bits(ifaceMac.sa_data, 4, portRem);
    ifaceMac.sa_family = 1;

    if ((commSock = socket(AF_INET, SOCK_DGRAM, IPPROTO_UDP)) < 0) err("unable to open socket");
    if (bind(commSock, (struct sockaddr *) &addrLoc, sizeof (addrLoc)) < 0) err("failed to bind socket");
    printf("binded to local port %s %i.\n", inet_ntoa(addrLoc.sin_addr), portLoc);
    printf("will send to %s %i.\n", inet_ntoa(addrRem.sin_addr), portRem);

    ifaceName = malloc(1024);
    strcpy(ifaceName, argv[1]);
    printf("creating interface %s.\n", ifaceName);
    printf("address will be %s", inet_ntoa(ifaceAddr.sin_addr));
    printf("/%s.\n", inet_ntoa(ifaceMask.sin_addr));

    if ((ifaceSock = open("/dev/net/tun", O_RDWR)) < 0) err("unable to open interface");
    struct ifreq ifr;
    memset(&ifr, 0, sizeof (ifr));
    strcpy(ifr.ifr_name, ifaceName);
    ifr.ifr_flags = IFF_TAP | IFF_NO_PI;
    if (ioctl(ifaceSock, TUNSETIFF, &ifr) < 0) err("unable to create interface");
    int configSock;
    if ((configSock = socket(PF_PACKET, SOCK_RAW, htons(ETH_P_ALL))) < 0) err("unable to open socket");
    memset(&ifr, 0, sizeof (ifr));
    strcpy(ifr.ifr_name, ifaceName);
    memcpy(&ifr.ifr_hwaddr, &ifaceMac, sizeof (ifr.ifr_hwaddr));
    if (ioctl(configSock, SIOCSIFHWADDR, &ifr) < 0) err("unable to set mac");
    memset(&ifr, 0, sizeof (ifr));
    strcpy(ifr.ifr_name, ifaceName);
    memcpy(&ifr.ifr_addr, &ifaceAddr, sizeof (ifr.ifr_addr));
    if (ioctl(configSock, SIOCSIFADDR, &ifr) < 0) err("unable to set network");
    memset(&ifr, 0, sizeof (ifr));
    strcpy(ifr.ifr_name, ifaceName);
    memcpy(&ifr.ifr_netmask, &ifaceMask, sizeof (ifr.ifr_netmask));
    if (ioctl(configSock, SIOCSIFNETMASK, &ifr) < 0) err("unable to set netmask");
    memset(&ifr, 0, sizeof (ifr));
    strcpy(ifr.ifr_name, ifaceName);
    ifr.ifr_flags = IFF_RUNNING | IFF_UP | IFF_BROADCAST | IFF_MULTICAST;
    if (ioctl(configSock, SIOCSIFFLAGS, &ifr) < 0) err("unable to set flags");

    printf("serving others\n");

    byteRx = 0;
    packRx = 0;
    byteTx = 0;
    packTx = 0;
    if (pthread_create(&threadTap, NULL, (void*) & doTapLoop, NULL)) err("error creating tap thread");
    if (pthread_create(&threadUdp, NULL, (void*) & doUdpLoop, NULL)) err("error creating udp thread");

    doMainLoop();
}
