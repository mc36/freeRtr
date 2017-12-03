#include <arpa/inet.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <netinet/in.h>
#include <pthread.h>
#include <unistd.h>
#include "utils.h"

#define vlanMax 32

#define vlanType 0x8100



struct sockaddr_in addrLoc;
struct sockaddr_in addrRem;
int portLoc;
int portRem;
int commSock;
pthread_t threadUdp;
int vlanVal[vlanMax];
struct sockaddr_in vlanLoc[vlanMax];
struct sockaddr_in vlanRem[vlanMax];
int vlanSck[vlanMax];
pthread_t vlanThr[vlanMax];
int vlanHsh[4096];
int vlanNum;
long int byteRx;
long int packRx;
long int byteTx;
long int packTx;

void err(char*buf) {
    printf("%s\n", buf);
    exit(1);
}

void doLowerLoop() {
    unsigned char bufD[16384];
    unsigned char*bufP;
    int bufS;
    struct sockaddr_in addrTmp;
    unsigned int addrLen;
    int vln;
    for (;;) {
        addrLen = sizeof (addrTmp);
        bufS = sizeof (bufD);
        bufS = recvfrom(commSock, bufD, bufS, 0, (struct sockaddr *) &addrTmp, &addrLen);
        if (bufS < 0) break;
        bufP = bufD;
        vln = 0;
        if (get16bits(bufD, 12) == vlanType) {
            vln = vlanHsh[get16bits(bufD, 14) & 0xfff];
            if (vln > 0) {
                bufP += 4;
                bufS -= 4;
                memmove(bufP, bufD, 12);
            }
        }
        packTx++;
        byteTx += bufS;
        sendto(vlanSck[vln], bufP, bufS, 0, (struct sockaddr *) &vlanRem[vln], sizeof (addrRem));
    }
    err("lower thread exited");
}

void doNativeLoop() {
    unsigned char bufD[16384];
    int bufS;
    struct sockaddr_in addrTmp;
    unsigned int addrLen;
    for (;;) {
        addrLen = sizeof (addrTmp);
        bufS = sizeof (bufD);
        bufS = recvfrom(vlanSck[0], bufD, bufS, 0, (struct sockaddr *) &addrTmp, &addrLen);
        if (bufS < 0) break;
        packRx++;
        byteRx += bufS;
        sendto(commSock, bufD, bufS, 0, (struct sockaddr *) &addrRem, sizeof (addrRem));
    }
    err("native thread exited");
}

void doUpperLoop(void *arg) {
    int myVlan = *((int *) arg);
    unsigned char bufD[16384];
    int bufS;
    struct sockaddr_in addrTmp;
    unsigned int addrLen;
    for (;;) {
        addrLen = sizeof (addrTmp);
        bufS = sizeof (bufD);
        bufS = recvfrom(vlanSck[myVlan], bufD + 4, bufS, 0, (struct sockaddr *) &addrTmp, &addrLen);
        if (bufS < 0) break;
        bufS += 4;
        memmove(bufD, bufD + 4, 12);
        put16bits(bufD, 12, vlanType);
        put16bits(bufD, 14, vlanVal[myVlan]);
        packRx++;
        byteRx += bufS;
        sendto(commSock, bufD, bufS, 0, (struct sockaddr *) &addrRem, sizeof (addrRem));
    }
    err("upper thread exited");
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
            printf("vlan counters:\n");
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

    if (argc < 9) {
        if (argc <= 1) goto help;
        char*curr = argv[1];
        if ((curr[0] == '-') || (curr[0] == '/')) curr++;
        switch (curr[0]) {
            case 'V':
            case 'v':
                err("vlan interface driver v1.0");
                break;
            case '?':
            case 'h':
            case 'H':
                help :
                        curr = argv[0];
                printf("using: %s <lport> <raddr> <rport> <laddr> <lport0> <rport0> <vlan1> <lport1> <rport1> <vlan2> <lport2> <rport2> [..]\n", curr);
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

    if ((commSock = socket(AF_INET, SOCK_DGRAM, IPPROTO_UDP)) < 0) err("unable to open socket");
    if (bind(commSock, (struct sockaddr *) &addrLoc, sizeof (addrLoc)) < 0) err("failed to bind socket");
    printf("lower binded to local port %s %i, will send to %s %i.\n", inet_ntoa(addrLoc.sin_addr), portLoc, inet_ntoa(addrRem.sin_addr), portRem);

    vlanNum = ((argc - 3) / 3);
    if (vlanNum > vlanMax) vlanNum = vlanMax;

    int i;
    for (i = 0; i < vlanNum; i++) {
        printf("upper #%i ", i);
        memset(&vlanLoc[i], 0, sizeof (addrLoc));
        memset(&vlanRem[i], 0, sizeof (addrRem));
        if (inet_aton(argv[2], &vlanRem[i].sin_addr) == 0) err("bad raddr address");
        if (inet_aton(argv[4], &vlanLoc[i].sin_addr) == 0) err("bad laddr address");
        vlanVal[i] = atoi(argv[i * 3 + 4]);
        portLoc = atoi(argv[i * 3 + 5]);
        portRem = atoi(argv[i * 3 + 6]);
        vlanLoc[i].sin_family = AF_INET;
        vlanLoc[i].sin_port = htons(portLoc);
        vlanRem[i].sin_family = AF_INET;
        vlanRem[i].sin_port = htons(portRem);
        if ((vlanSck[i] = socket(AF_INET, SOCK_DGRAM, IPPROTO_UDP)) < 0) err("unable to open socket");
        if (bind(vlanSck[i], (struct sockaddr *) &vlanLoc[i], sizeof (addrLoc)) < 0) err("failed to bind socket");
        printf("binded to local port %s %i, will send to %s %i.\n", inet_ntoa(vlanLoc[i].sin_addr), portLoc, inet_ntoa(vlanRem[i].sin_addr), portRem);
    }
    vlanVal[0] = -1;
    printf("vlans:");
    memset(&vlanHsh, 0, sizeof (vlanHsh));
    for (i = 1; i < vlanNum; i++) {
        printf(" %i", vlanVal[i]);
        vlanHsh[vlanVal[i]] = i;
    }
    printf(".\n");

    printf("serving others\n");

    byteRx = 0;
    packRx = 0;
    byteTx = 0;
    packTx = 0;
    if (pthread_create(&threadUdp, NULL, (void*) & doLowerLoop, NULL)) err("error creating lower thread");

    if (pthread_create(&vlanThr[0], NULL, (void*) & doNativeLoop, NULL)) err("error creating native thread");

    for (i = 1; i < vlanNum; i++) {
        int *arg = malloc(sizeof (*arg));
        *arg = i;
        if (pthread_create(&vlanThr[i], NULL, (void*) & doUpperLoop, arg)) err("error creating upper thread");
    }

    doMainLoop();
}
