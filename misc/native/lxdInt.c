#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <pthread.h>
#include <arpa/inet.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <poll.h>
#include <xdp/xsk.h>
#include <sys/mman.h>


#define FRAMES_NUM 1024

char *ifaceName;
int ifaceFd;
struct xsk_umem *ifaceUmem;
struct xsk_socket *ifaceXsk;
struct xsk_ring_prod ifaceFq;
struct xsk_ring_cons ifaceCq;
struct xsk_ring_cons ifaceRx;
struct xsk_ring_prod ifaceTx;
char *ifaceBuf;
struct pollfd ifacePfd;
struct sockaddr_in addrLoc;
struct sockaddr_in addrRem;
int portLoc;
int portRem;
int commSock;
pthread_t threadUdp;
pthread_t threadRaw;
pthread_t threadPrint;
long int byteRx;
long int packRx;
long int byteTx;
long int packTx;

void err(char*buf) {
    printf("%s\n", buf);
    _exit(1);
}

void doRawLoop() {
    for (;;) {
        poll(&ifacePfd, 1, 1);
        int idx;
        if (xsk_ring_cons__peek(&ifaceRx, 1, &idx) < 1) continue;
        const struct xdp_desc *dsc = xsk_ring_cons__rx_desc(&ifaceRx, idx);
        char *dat = xsk_umem__get_data(ifaceBuf, dsc->addr);
        int len = dsc->len;
        packRx++;
        byteRx += len;
        send(commSock, dat, len, 0);
        xsk_ring_prod__reserve(&ifaceFq, 1, &idx);
        *xsk_ring_prod__fill_addr(&ifaceFq, idx) = dsc->addr;
        xsk_ring_prod__submit(&ifaceFq, 1);
        xsk_ring_cons__release(&ifaceRx, 1);
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
        int idx;
        xsk_ring_prod__reserve(&ifaceTx, 1, &idx);
        struct xdp_desc *dsc = xsk_ring_prod__tx_desc(&ifaceTx, idx);
        dsc->addr = (FRAMES_NUM + (idx % FRAMES_NUM)) * XSK_UMEM__DEFAULT_FRAME_SIZE;
        dsc->options = 0;
        dsc->len = bufS;
        memcpy(ifaceBuf + dsc->addr, bufD, bufS);
        xsk_ring_prod__submit(&ifaceTx, 1);
        idx = xsk_ring_cons__peek(&ifaceCq, 16, &bufS);
        xsk_ring_cons__release(&ifaceCq, idx);
        if (!xsk_ring_prod__needs_wakeup(&ifaceTx)) continue;
        sendto(xsk_socket__fd(ifaceXsk), NULL, 0, MSG_DONTWAIT, NULL, 0);
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
            err("afxdp interface driver v1.0");
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
    printf("opening interface %s\n", ifaceName);

    posix_memalign((void**)&ifaceBuf, getpagesize(), XSK_UMEM__DEFAULT_FRAME_SIZE * 2 * FRAMES_NUM);
    if (ifaceBuf == NULL) err("error allocating buffer");

    if (xsk_umem__create(&ifaceUmem, ifaceBuf, XSK_UMEM__DEFAULT_FRAME_SIZE * 2 * FRAMES_NUM, &ifaceFq, &ifaceCq, NULL) != 0) err("error creating umem");

    if (xsk_socket__create(&ifaceXsk, ifaceName, 0, ifaceUmem, &ifaceRx, &ifaceTx, NULL) != 0) err("error creating xsk");

    int i = 0;
    xsk_ring_prod__reserve(&ifaceFq, FRAMES_NUM, &i);
    for (i=0; i < FRAMES_NUM; i++) *xsk_ring_prod__fill_addr(&ifaceFq, i) = i * XSK_UMEM__DEFAULT_FRAME_SIZE;
    xsk_ring_prod__submit(&ifaceFq, FRAMES_NUM);

    memset(&ifacePfd, 0, sizeof (ifacePfd));
    ifacePfd.fd = xsk_socket__fd(ifaceXsk);
    ifacePfd.events = POLLIN | POLLERR;

    printf("serving others\n");

    byteRx = 0;
    packRx = 0;
    byteTx = 0;
    packTx = 0;
    if (pthread_create(&threadRaw, NULL, (void*) & doRawLoop, NULL)) err("error creating raw thread");
    if (pthread_create(&threadUdp, NULL, (void*) & doUdpLoop, NULL)) err("error creating udp thread");

    doMainLoop();
}
