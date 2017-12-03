#include <arpa/inet.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <netinet/in.h>
#include <pthread.h>
#include <unistd.h>


#define bundleMax 32

struct sockaddr_in addrLoc;
struct sockaddr_in addrRem;
int portLoc;
int portRem;
int commSock;
pthread_t threadUdp;
struct sockaddr_in bunLoc[bundleMax];
struct sockaddr_in bunRem[bundleMax];
int bunSck[bundleMax];
pthread_t bunThr[bundleMax];
int bunNum;
int bunNxt;
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
    int bufS;
    struct sockaddr_in addrTmp;
    unsigned int addrLen;
    for (;;) {
        addrLen = sizeof (addrTmp);
        bufS = sizeof (bufD);
        bufS = recvfrom(commSock, bufD, bufS, 0, (struct sockaddr *) &addrTmp, &addrLen);
        if (bufS < 0) break;
        bunNxt = (bunNxt + 1) % bunNum;
        packTx++;
        byteTx += bufS;
        sendto(bunSck[bunNxt], bufD, bufS, 0, (struct sockaddr *) &bunRem[bunNxt], sizeof (addrRem));
    }
    err("lower thread exited");
}

void doUpperLoop(void *arg) {
    int myBun = *((int *) arg);
    unsigned char bufD[16384];
    int bufS;
    struct sockaddr_in addrTmp;
    unsigned int addrLen;
    for (;;) {
        addrLen = sizeof (addrTmp);
        bufS = sizeof (bufD);
        bufS = recvfrom(bunSck[myBun], bufD, bufS, 0, (struct sockaddr *) &addrTmp, &addrLen);
        if (bufS < 0) break;
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
            printf("bundle counters:\n");
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
                err("bundle interface driver v1.0");
                break;
            case '?':
            case 'h':
            case 'H':
                help :
                        curr = argv[0];
                printf("using: %s <lport> <raddr> <rport> <laddr> <lport1> <rport1> <lport2> <rport2> [..]\n", curr);
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

    bunNum = ((argc - 4) / 2);
    if (bunNum > bundleMax) bunNum = bundleMax;

    int i;
    for (i = 0; i < bunNum; i++) {
        printf("upper #%i ", i);
        memset(&bunLoc[i], 0, sizeof (addrLoc));
        memset(&bunRem[i], 0, sizeof (addrRem));
        if (inet_aton(argv[2], &bunRem[i].sin_addr) == 0) err("bad raddr address");
        if (inet_aton(argv[4], &bunLoc[i].sin_addr) == 0) err("bad laddr address");
        portLoc = atoi(argv[i * 2 + 5]);
        portRem = atoi(argv[i * 2 + 6]);
        bunLoc[i].sin_family = AF_INET;
        bunLoc[i].sin_port = htons(portLoc);
        bunRem[i].sin_family = AF_INET;
        bunRem[i].sin_port = htons(portRem);
        if ((bunSck[i] = socket(AF_INET, SOCK_DGRAM, IPPROTO_UDP)) < 0) err("unable to open socket");
        if (bind(bunSck[i], (struct sockaddr *) &bunLoc[i], sizeof (addrLoc)) < 0) err("failed to bind socket");
        printf("binded to local port %s %i, will send to %s %i.\n", inet_ntoa(bunLoc[i].sin_addr), portLoc, inet_ntoa(bunRem[i].sin_addr), portRem);
    }

    printf("serving others\n");

    byteRx = 0;
    packRx = 0;
    byteTx = 0;
    packTx = 0;
    if (pthread_create(&threadUdp, NULL, (void*) & doLowerLoop, NULL)) err("error creating lower thread");

    for (i = 0; i < bunNum; i++) {
        int *arg = malloc(sizeof (*arg));
        *arg = i;
        if (pthread_create(&bunThr[i], NULL, (void*) & doUpperLoop, arg)) err("error creating upper thread");
    }

    doMainLoop();
}
