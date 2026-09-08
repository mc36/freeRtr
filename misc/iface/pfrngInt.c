#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <pthread.h>
#include <arpa/inet.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <pfring.h>



char *ifaceName;
pfring *ifacePfring;
struct sockaddr_in addrLoc;
struct sockaddr_in addrRem;
int portLoc;
int portRem;
int commSock;
pthread_t threadUdp;
pthread_t threadRaw;
long byteRx;
long packRx;
long byteTx;
long packTx;

void err(char*buf) {
    printf("%s\n", buf);
    _exit(1);
}

void doRawLoop() {
    struct pfring_pkthdr hdr;
    unsigned char bufD[16384];
    unsigned char *bufP;
    bufP = (unsigned char*)&bufD;
    int bufS;
    for (;;) {
        if (pfring_recv(ifacePfring, &bufP, sizeof(bufD), &hdr, 1) != 1) break;
        int len = hdr.caplen;
        packRx++;
        byteRx += len;
        send(commSock, bufD, len, 0);
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
        pfring_send(ifacePfring, (char*)&bufD, bufS, 1);
    }
    err("udp thread exited");
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
            err("pfring interface driver v1.0");
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

    unsigned int vers;
    pfring_version_noring(&vers);
    printf("pfring version: v%i.%i\n", vers >> 16, vers & 0xffff);

    if ((commSock = socket(AF_INET, SOCK_DGRAM, IPPROTO_UDP)) < 0) err("unable to open socket");
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
    printf("opening interface %s\n", ifaceName);

    ifacePfring = pfring_open(ifaceName, 65535, PF_RING_PROMISC | PF_RING_DISCARD_INJECTED_PKTS);
    if (ifacePfring == NULL) err("unable to open interface");
    if (pfring_set_direction(ifacePfring, rx_only_direction) != 0) err("unable to set direction");
    if (pfring_set_socket_mode(ifacePfring, send_and_recv_mode) != 0) err("unable to set socket mode");
    if (pfring_set_poll_watermark(ifacePfring, 1) != 0) err("unable to set watermark");
    if (pfring_enable_ring(ifacePfring) != 0) err("unable to enable interface");

    setgid(1);
    setuid(1);
    printf("serving others\n");

    byteRx = 0;
    packRx = 0;
    byteTx = 0;
    packTx = 0;
    if (pthread_create(&threadRaw, NULL, (void*) & doRawLoop, NULL)) err("error creating raw thread");
    if (pthread_create(&threadUdp, NULL, (void*) & doUdpLoop, NULL)) err("error creating udp thread");

    doMainLoop();
}
