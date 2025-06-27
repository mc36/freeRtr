#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <pthread.h>
#include <arpa/inet.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <termios.h>
#include <fcntl.h>
#include <sys/ioctl.h>

#include "fcs16.h"

#define PPP_FLAG 0x7e
#define PPP_ESCP 0x7d
#define PPP_TRNS 0x20

char *ttyName;
struct sockaddr_in addrLoc;
struct sockaddr_in addrRem;
int addrTty;
int portLoc;
int portRem;
int commSock;
int hackTty;
unsigned char accmTty[256];
pthread_t threadUdp;
pthread_t threadRaw;
pthread_t threadStat;
long byteRx;
long packRx;
long byteTx;
long packTx;
long byteBd;
long packBd;

void err(char*buf) {
    printf("%s\n", buf);
    _exit(1);
}

void doRawLoop() {
    unsigned char buf1d[16384];
    int buf1s = 0;
    int buf2p;
    unsigned char buf2d[1024];
    int buf2s;
    int sawEsc = 0;
    int i;
    for (;;) {
        buf2s = 1;
        if (ioctl(addrTty, FIONREAD, &buf2s) < 0) buf2s = 1;
        if (buf2s < 1) buf2s = 1;
        if (buf2s>sizeof (buf2d)) buf2s = sizeof (buf2d);
        buf2s = read(addrTty, buf2d, buf2s);
        if (buf2s < 1) break;
        for (buf2p = 0; buf2p < buf2s; buf2p++) {
            if (buf1s >= sizeof (buf1d)) {
                packBd++;
                byteBd += buf1s;
                buf1s = 0;
            }
            i = buf2d[buf2p] & 0xff;
            if (sawEsc != 0) {
                sawEsc = 0;
                buf1d[buf1s] = i ^ PPP_TRNS;
                buf1s++;
                continue;
            }
            if (i == PPP_ESCP) {
                sawEsc = 1;
                continue;
            }
            if (i != PPP_FLAG) {
                buf1d[buf1s] = i;
                buf1s++;
                continue;
            }
            if (buf1s < 2) {
                buf1s = 0;
                continue;
            }
            buf1s -= 2;
            i = doFcsCalc(buf1d, buf1s);
            if (((buf1d[buf1s] & 0xff) != (i & 0xff)) || ((buf1d[buf1s + 1] & 0xff) != (i >> 8))) {
                packBd++;
                byteBd += buf1s;
                buf1s = 0;
                continue;
            }
            if ((hackTty & 1) != 0) {
                memmove(&buf1d[2], &buf1d, buf1s);
                buf1s += 2;
                buf1d[0] = 0xff;
                buf1d[1] = 0x03;
            }
            packRx++;
            byteRx += buf1s;
            send(commSock, buf1d, buf1s, 0);
            buf1s = 0;
        }
    }
    err("raw thread exited");
}

void doUdpLoop() {
    unsigned char buf1d[16384];
    int buf1s;
    unsigned char buf2d[32768];
    int buf2s;
    int i, o;
    for (;;) {
        buf1s = sizeof (buf1d);
        buf1s = recv(commSock, buf1d, buf1s, 0);
        if (buf1s < 0) break;
        packTx++;
        byteTx += buf1s;
        if ((hackTty & 2) != 0) {
            buf1s -= 2;
            if (buf1s < 1) continue;
            memmove(&buf1d, &buf1d[2], buf1s);
        }
        i = doFcsCalc(buf1d, buf1s);
        buf1d[buf1s + 0] = i & 0xff;
        buf1d[buf1s + 1] = i >> 8;
        buf1s += 2;
        buf2d[0] = PPP_FLAG;
        buf2s = 1;
        for (i = 0; i < buf1s; i++) {
            o = buf1d[i] & 0xff;
            if (accmTty[o] == 0) {
                buf2d[buf2s] = o;
                buf2s++;
            } else {
                buf2d[buf2s] = PPP_ESCP;
                buf2s++;
                buf2d[buf2s] = o ^ PPP_TRNS;
                buf2s++;
            }
        }
        buf2d[buf2s] = PPP_FLAG;
        buf2s++;
        if (write(addrTty, buf2d, buf2s) != buf2s) break;
    }
    err("udp thread exited");
}

void doStatLoop() {
    if ((hackTty & 4) != 0) return;
    int flags;
    for (;;) {
        sleep(1);
        flags = 0;
        if (ioctl(addrTty, TIOCMGET, &flags) < 0) break;
        if ((flags & TIOCM_CD) == 0) break;
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
        printf("error    %20li %20li\n", packBd, byteBd);
        break;
    case 'C':
    case 'c':
        printf("counters cleared.\n");
        byteRx = 0;
        packRx = 0;
        byteTx = 0;
        packTx = 0;
        byteBd = 0;
        packBd = 0;
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
            err("async hdlc interface driver v1.0");
            break;
        case '?':
        case 'h':
        case 'H':
help :
            curr = argv[0];
            printf("using: %s <tty> <lport> <raddr> <rport> [laddr] [hacks] [accm]\n", curr);
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
    setsockopt(commSock, SOL_SOCKET, SO_RCVBUF, &sockOpt, sizeof(sockOpt));
    setsockopt(commSock, SOL_SOCKET, SO_SNDBUF, &sockOpt, sizeof(sockOpt));

    memset(accmTty, 0, 256);
    accmTty[PPP_FLAG] = 1;
    accmTty[PPP_ESCP] = 1;
    if (argc > 6) {
        hackTty = atoi(argv[6]);
    } else {
        hackTty = 0;
    }
    if (argc > 7) {
        int o = atoi(argv[7]);
        int i;
        for (i = 0; i < 32; i++) {
            if (((1 << i) & o) != 0) accmTty[i] = 1;
        }
    }
    if ((hackTty & 8) != 0) {
        int i;
        for (i = 0; i < 128; i++) accmTty[i + 128] = accmTty[i];
    }
    if ((hackTty & 65536) != 0) {
        printf("accm=");
        int i;
        for (i = 0; i < 256; i++) printf("%i", accmTty[i]);
        printf(" hack=%08x\n", hackTty);
    }

    ttyName = malloc(strlen(argv[1]) + 1);
    if (ttyName == NULL) err("error allocating memory");
    strcpy(ttyName, argv[1]);
    printf("opening tty %s.\n", ttyName);
    if ((addrTty = open(ttyName, O_RDWR)) < 0) err("unable to open file");

    setgid(1);
    setuid(1);
    printf("serving others\n");

    makeFcsTab();
    byteRx = 0;
    packRx = 0;
    byteTx = 0;
    packTx = 0;
    byteBd = 0;
    packBd = 0;
    if (pthread_create(&threadRaw, NULL, (void*) & doRawLoop, NULL)) err("error creating raw thread");
    if (pthread_create(&threadUdp, NULL, (void*) & doUdpLoop, NULL)) err("error creating udp thread");
    if (pthread_create(&threadStat, NULL, (void*) & doStatLoop, NULL)) err("error creating stat thread");
    doMainLoop();

}
