#include <arpa/inet.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <pthread.h>
#include <unistd.h>
#include <tuntap.h>
#include "utils.h"

struct sockaddr_in addrLoc;
struct sockaddr_in addrRem;
struct device*ifaceHnd;
char ifaceAddr[64];
int ifaceMask;
char ifaceMac[32];
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
        bufS = tuntap_read(ifaceHnd, bufD, bufS);
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
    int addrLen;
    for (;;) {
        addrLen = sizeof (addrTmp);
        bufS = sizeof (bufD);
        bufS = recvfrom(commSock, bufD, bufS, 0, (struct sockaddr *) &addrTmp, &addrLen);
        if (bufS < 0) break;
        packTx++;
        byteTx += bufS;
        tuntap_write(ifaceHnd, bufD, bufS);
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

    if (argc < 7) {
        if (argc <= 1) goto help;
        char*curr = argv[1];
        if ((curr[0] == '-') || (curr[0] == '/')) curr++;
        switch (curr[0]) {
            case 'V':
            case 'v':
                err("libtuntap interface driver v1.0\n");
                break;
            case '?':
            case 'h':
            case 'H':
                help :
                        curr = argv[0];
                printf("using: %s <lport> <raddr> <rport> <laddr> <addr> <maskbits>\n", curr);
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

    portLoc = atoi(argv[1]);
    portRem = atoi(argv[3]);
    memset(&addrLoc, 0, sizeof (addrLoc));
    memset(&addrRem, 0, sizeof (addrRem));
    if (inet_aton(argv[2], &addrRem.sin_addr) == 0) err("bad raddr address");
    if (inet_aton(argv[4], &addrLoc.sin_addr) == 0) err("bad laddr address");
    addrLoc.sin_family = AF_INET;
    addrLoc.sin_port = htons(portLoc);
    addrRem.sin_family = AF_INET;
    addrRem.sin_port = htons(portRem);
    strcpy(ifaceAddr, argv[5]);
    ifaceMask = atoi(argv[6]);
    memset(&ifaceMac, 0, sizeof (ifaceMac));
    put16bits(ifaceMac, 2, portLoc);
    put16bits(ifaceMac, 4, portRem);
    snprintf(ifaceMac, sizeof (ifaceMac), "%02x:%02x:%02x:%02x:%02x:%02x", ifaceMac[0], ifaceMac[1], ifaceMac[2], ifaceMac[3], ifaceMac[4], ifaceMac[5]);

    if ((commSock = socket(AF_INET, SOCK_DGRAM, IPPROTO_UDP)) < 0) err("unable to open socket");
    if (bind(commSock, (struct sockaddr *) &addrLoc, sizeof (addrLoc)) < 0) err("failed to bind socket");
    printf("binded to local port %s %i.\n", inet_ntoa(addrLoc.sin_addr), portLoc);
    printf("will send to %s %i.\n", inet_ntoa(addrRem.sin_addr), portRem);

    printf("libtuntap version: %i\n", tuntap_version());

    printf("creating interface.\n");
    printf("address will be %s", ifaceAddr);
    printf("/%i.\n", ifaceMask);

    ifaceHnd = tuntap_init();
    if (tuntap_start(ifaceHnd, TUNTAP_MODE_ETHERNET, TUNTAP_ID_ANY) < 0) err("unable to start interface");
    if (tuntap_set_hwaddr(ifaceHnd, ifaceMac) < 0) err("unable to set mac");
    if (tuntap_set_ip(ifaceHnd, ifaceAddr, ifaceMask) < 0) err("unable to set ip");
    if (tuntap_up(ifaceHnd) < 0) err("unable to bring up");
    printf("interface %s created.\n", tuntap_get_ifname(ifaceHnd));

    printf("serving others\n");

    byteRx = 0;
    packRx = 0;
    byteTx = 0;
    packTx = 0;
    if (pthread_create(&threadTap, NULL, (void*) & doTapLoop, NULL)) err("error creating tap thread");
    if (pthread_create(&threadUdp, NULL, (void*) & doUdpLoop, NULL)) err("error creating udp thread");

    doMainLoop();
}
