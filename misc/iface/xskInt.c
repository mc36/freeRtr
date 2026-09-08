#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <pthread.h>
#include <arpa/inet.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <poll.h>
#include <linux/if_link.h>
#include <xdp/xsk.h>


#define framesNum 1024

char *ifaceName;
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
long byteRx;
long packRx;
long byteTx;
long packTx;

void err(char*buf) {
    printf("%s\n", buf);
    _exit(1);
}

void doRawLoop() {
    for (;;) {
        unsigned int idx;
        if (xsk_ring_cons__peek(&ifaceRx, 1, &idx) < 1) {
            poll(&ifacePfd, 1, 1);
            continue;
        }
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
        unsigned int idx;
        idx = xsk_ring_cons__peek(&ifaceCq, 16, &idx);
        xsk_ring_cons__release(&ifaceCq, idx);
        if (xsk_ring_prod__reserve(&ifaceTx, 1, &idx) < 1) continue;
        struct xdp_desc *dsc = xsk_ring_prod__tx_desc(&ifaceTx, idx);
        dsc->addr = (framesNum + (idx % framesNum)) * XSK_UMEM__DEFAULT_FRAME_SIZE;
        dsc->options = 0;
        dsc->len = bufS;
        memcpy(ifaceBuf + dsc->addr, bufD, bufS);
        xsk_ring_prod__submit(&ifaceTx, 1);
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

    if (argc < 6) {
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
            printf("using: %s <iface> <skb/drv/hw> <lport> <raddr> <rport> [laddr]\n", curr);
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

    int bpf_flag = 0;
    if (strcmp(argv[2],"skb") == 0) {
        bpf_flag = XDP_FLAGS_SKB_MODE;
    }
    if (strcmp(argv[2],"drv") == 0) {
        bpf_flag = XDP_FLAGS_DRV_MODE;
    }
    if (strcmp(argv[2],"hw") == 0) {
        bpf_flag = XDP_FLAGS_HW_MODE;
    }

    portLoc = atoi(argv[3]);
    portRem = atoi(argv[5]);
    memset(&addrLoc, 0, sizeof (addrLoc));
    memset(&addrRem, 0, sizeof (addrRem));
    if (inet_aton(argv[4], &addrRem.sin_addr) == 0) err("bad raddr address");
    if (argc > 5) {
        if (inet_aton(argv[6], &addrLoc.sin_addr) == 0) err("bad laddr address");
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
    setsockopt(commSock, SOL_SOCKET, SO_RCVBUF, &sockOpt, sizeof(sockOpt));
    setsockopt(commSock, SOL_SOCKET, SO_SNDBUF, &sockOpt, sizeof(sockOpt));

    ifaceName = malloc(strlen(argv[1]) + 1);
    if (ifaceName == NULL) err("error allocating memory");
    strcpy(ifaceName, argv[1]);
    printf("opening interface %s\n", ifaceName);

    posix_memalign((void**)&ifaceBuf, getpagesize(), XSK_UMEM__DEFAULT_FRAME_SIZE * 2 * framesNum);
    if (ifaceBuf == NULL) err("error allocating buffer");

    if (xsk_umem__create(&ifaceUmem, ifaceBuf, XSK_UMEM__DEFAULT_FRAME_SIZE * 2 * framesNum, &ifaceFq, &ifaceCq, NULL) != 0) err("error creating umem");

    struct xsk_socket_config cfg;
    memset(&cfg, 0, sizeof(cfg));
    cfg.rx_size = XSK_RING_CONS__DEFAULT_NUM_DESCS;
    cfg.tx_size = XSK_RING_PROD__DEFAULT_NUM_DESCS;
    cfg.xdp_flags = bpf_flag;
    if (xsk_socket__create(&ifaceXsk, ifaceName, 0, ifaceUmem, &ifaceRx, &ifaceTx, &cfg) != 0) err("error creating xsk");

    unsigned int i = 0;
    xsk_ring_prod__reserve(&ifaceFq, framesNum, &i);
    for (i=0; i < framesNum; i++) *xsk_ring_prod__fill_addr(&ifaceFq, i) = i * XSK_UMEM__DEFAULT_FRAME_SIZE;
    xsk_ring_prod__submit(&ifaceFq, framesNum);

    memset(&ifacePfd, 0, sizeof (ifacePfd));
    ifacePfd.fd = xsk_socket__fd(ifaceXsk);
    ifacePfd.events = POLLIN | POLLERR;

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
