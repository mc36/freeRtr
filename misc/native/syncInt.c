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

#define BANG_CK (TIOCM_CD | TIOCM_DSR)
#define BANG_RX (TIOCM_RI | TIOCM_CTS)
#define BANG_TX (TIOCM_DTR | TIOCM_RTS)

char *ttyName;
struct sockaddr_in addrLoc;
struct sockaddr_in addrRem;
int addrTty;
int portLoc;
int portRem;
int commSock;
unsigned char txD[16384];
int txS = 0;
pthread_t threadUdp;
pthread_t threadRaw;
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
    unsigned char rxD[16384];
    int rxS = 0;
    int rxV = 0;
    int rxL = 0;
    int rxC = 0;
    int rxE = 0;
    int txV = 0;
    int txL = 0;
    int txC = 0;
    int txO = 0;
    int bit = 0;
    int cur = 0;
    int old = 0;
    for (;;) {
        usleep(1);
        old = cur;
        if (ioctl(addrTty, TIOCMGET, &cur) < 0) break;
        bit = cur & BANG_CK;
        if (bit == (old & BANG_CK)) continue;
        if (bit != 0) {
            bit = (txV >> txL) & 1;
            txL++;
            if (txC >= 5) {
                bit = 0;
                txC = 0;
                txL--;
            }
            if (bit == 0) {
                txC = 0;
            } else {
                txC++;
            }
            if (txV < 0) {
                bit = 1;
                if (txL <= 1) bit = 0;
                if (txL >= 8) bit = 0;
                txC = 0;
            }
            if (txL >= 8) {
                txV = txD[txO] & 0xff;
                txO++;
                txL = 0;
            }
            if (txO > txS) {
                txO = 0;
                txS = 0;
                txC = 0;
                txV = -1;
            }
            if (bit != 0) {
                cur &= ~BANG_TX;
            } else {
                cur |= BANG_TX;
            }
            if (ioctl(addrTty, TIOCMSET, &cur) < 0) break;
            continue;
        }
        bit = (cur & BANG_RX) == 0 ? 1 : 0;
        if (bit == 0) {
            if (rxC == 5) {
                rxC = 0;
                continue;
            }
            rxE = rxC > 5;
            rxC = 0;
        } else {
            rxE = 0;
            rxC++;
        }
        rxV |= bit << rxL;
        rxL++;
        if (rxL < 8) {
            if (rxE == 0) continue;
            rxL = 0;
            rxV = 0;
            rxS = 0;
            continue;
        }
        rxD[rxS] = rxV;
        rxS++;
        rxL = 0;
        rxV = 0;
        if (rxS >= sizeof (rxD)) {
            packBd++;
            byteBd += rxS;
            rxS = 0;
            continue;
        }
        if (rxE == 0) continue;
        rxS -= 3;
        if (rxS < 0) {
            rxS = 0;
            continue;
        }
        int i = doFcsCalc(rxD, rxS);
        if (((rxD[rxS] & 0xff) != (i & 0xff)) || ((rxD[rxS + 1] & 0xff) != (i >> 8))) {
            packBd++;
            byteBd += rxS;
            rxS = 0;
            continue;
        }
        packRx++;
        byteRx += rxS;
        send(commSock, rxD, rxS, 0);
        rxS = 0;
    }
    err("raw thread exited");
}

void doUdpLoop() {
    int i;
    for (;;) {
        if (txS > 0) {
            usleep(1);
            continue;
        }
        i = sizeof (txD);
        i = recv(commSock, txD, i, 0);
        if (i < 0) break;
        packTx++;
        byteTx += i;
        txS = i;
        i = doFcsCalc(txD, txS);
        txD[txS] = i & 0xff;
        txD[txS + 1] = i >> 8;
        txS += 2;
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
        printf("using: %s <tty> <lport> <raddr> <rport> [laddr]\n", argv[0]);
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
    doMainLoop();

}
