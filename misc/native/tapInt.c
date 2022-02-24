#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <pthread.h>
#include <arpa/inet.h>
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
    for (;;) {
        bufS = sizeof (bufD);
        bufS = read(ifaceSock, bufD, bufS);
        if (bufS < 0) break;
        packRx++;
        byteRx += bufS;
        send(commSock, bufD, bufS, 0);
    }
    err("tap thread exited");
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
        if (write(ifaceSock, bufD, bufS) < 0) break;
    }
    err("udp thread exited");
}

void doMainLoop() {
    unsigned char buf[1024];

doer:
    printf("> ");
    buf[0] = 0;
    int i = scanf("%1024s", buf);
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

void doCmd(char *cmd) {
    printf("running %s...\n", cmd);
    if (system(cmd) < 0) err("error executing command");
}


int main(int argc, char **argv) {

    if (argc < 6) {
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
            printf("using: %s <iface> <lport> <raddr> <rport> <laddr> [addr/mask] [gw]\n", curr);
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

    if ((commSock = socket(AF_INET, SOCK_DGRAM, IPPROTO_UDP)) < 0) err("unable to open socket");
    if (bind(commSock, (struct sockaddr *) &addrLoc, sizeof (addrLoc)) < 0) err("failed to bind socket");
    printf("binded to local port %s %i.\n", inet_ntoa(addrLoc.sin_addr), portLoc);
    if (connect(commSock, (struct sockaddr *) &addrRem, sizeof (addrRem)) < 0) err("failed to connect socket");
    printf("will send to %s %i.\n", inet_ntoa(addrRem.sin_addr), portRem);
    int sockOpt = 524288;
    if (setsockopt(commSock, SOL_SOCKET, SO_RCVBUF, &sockOpt, sizeof(sockOpt)) < 0) err("failed to set socket rxbuf");
    if (setsockopt(commSock, SOL_SOCKET, SO_SNDBUF, &sockOpt, sizeof(sockOpt)) < 0) err("failed to set socket txbuf");

    ifaceName = malloc(strlen(argv[1]) + 1);
    if (ifaceName == NULL) err("error allocating memory");
    strcpy(ifaceName, argv[1]);
    printf("creating interface %s.\n", ifaceName);

    if ((ifaceSock = open("/dev/net/tun", O_RDWR)) < 0) err("unable to open interface");
    struct ifreq ifr;
    memset(&ifr, 0, sizeof (ifr));
    strcpy(ifr.ifr_name, ifaceName);
    ifr.ifr_flags = IFF_TAP | IFF_NO_PI;
    if (ioctl(ifaceSock, TUNSETIFF, &ifr) < 0) err("unable to create interface");

    char buf[1024];
    sprintf(buf, "ip link set %s address 00:00:%02x:%02x:%02x:%02x mtu 1500", ifaceName, portLoc >> 8, portLoc & 0xff, portRem >> 8, portRem & 0xff);
    doCmd(buf);
    sprintf(buf, "ip link set %s up", ifaceName);
    doCmd(buf);
    if (argc > 6) {
        sprintf(buf, "ip addr add %s dev %s", argv[6], ifaceName);
        doCmd(buf);
        sprintf(buf, "echo 0 > /proc/sys/net/ipv6/conf/%s/disable_ipv6", ifaceName);
        doCmd(buf);
    }
    if (argc > 7) {
        sprintf(buf, "ip route add 0.0.0.0/0 via %s dev %s", argv[7], ifaceName);
        doCmd(buf);
    }

    printf("serving others\n");

    byteRx = 0;
    packRx = 0;
    byteTx = 0;
    packTx = 0;
    if (pthread_create(&threadTap, NULL, (void*) & doTapLoop, NULL)) err("error creating tap thread");
    if (pthread_create(&threadUdp, NULL, (void*) & doUdpLoop, NULL)) err("error creating udp thread");

    doMainLoop();
}
