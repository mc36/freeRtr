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

#define BANG_RX (TIOCM_RI | TIOCM_CTS)
#define BANG_TX (TIOCM_DTR | TIOCM_RTS)

char *ttyName;
int addrTty;
int portLoc;
int lstnSock;
int commSock;
pthread_t threadRx;
pthread_t threadTx;
pthread_t threadAcc;
struct sockaddr_in addrLoc;
int spedRx;
int spedTx;
long byteRx;
long packRx;
long byteTx;
long packTx;

void err(char*buf) {
    printf("%s\n", buf);
    _exit(1);
}

void doAccLoop() {
    while (1) {
        int oldSock = commSock;
        commSock = accept(lstnSock, NULL, NULL);
        if (oldSock != -1) {
            close(oldSock);
        }
        if (commSock < 0) err("error accepting socket");
        printf("accepted connection\n");
    }
}

void doTxLoop() {
    unsigned char buf[1];
    int i, o, p;
    if (ioctl(addrTty, TIOCMGET, &o) < 0) err("error getting state");
    o &= ~BANG_TX;
    if (ioctl(addrTty, TIOCMSET, &o) < 0) err("error setting state");
    while (1) {
        if (commSock == -1) {
            sleep(1);
            continue;
        }
        i = sizeof (buf);
        i = recv(commSock, &buf, i, 0);
        if (i < 1) {
            sleep(1);
            continue;
        }
        byteTx += i;
        packTx++;
        p = buf[0];
        o |= BANG_TX;
        if (ioctl(addrTty, TIOCMSET, &o) < 0) break;
        usleep(spedTx);
        for (i = 0; i < 8; i++) {
            if ((p & (1 << i)) == 0) {
                o |= BANG_TX;
            } else {
                o &= ~BANG_TX;
            }
            if (ioctl(addrTty, TIOCMSET, &o) < 0) break;
            usleep(spedTx);
        }
        o &= ~BANG_TX;
        if (ioctl(addrTty, TIOCMSET, &o) < 0) break;
        usleep(spedTx);
    }
    err("tx thread exited");
}

void doRxLoop() {
    unsigned char buf[1];
    int i, o, p;
    while (1) {
        usleep(spedRx);
        if (ioctl(addrTty, TIOCMGET, &o) < 0) break;
        if ((o & BANG_RX) == 0) continue;
        p = 0;
        for (i = 0; i < 8; i++) {
            usleep(spedTx);
            if (ioctl(addrTty, TIOCMGET, &o) < 0) break;
            if ((o & BANG_RX) != 0) continue;
            p |= 1 << i;
        }
        usleep(spedTx);
        buf[0] = p;
        send(commSock, &buf, 1, 0);
        byteRx += 1;
        packRx++;
    }
    err("rx thread exited");
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
        printf("line counters:\n");
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

    if (argc < 4) {
        printf("using: %s <tty> <lport> <speed>\n", argv[0]);
        _exit(1);
    }

    spedTx = atoi(argv[3]);
    if (spedTx < 1) err("invalid speed");
    spedTx = 1000000 / spedTx;
    spedRx = spedTx / 2;
    if (spedRx < 1) err("bad speed");

    portLoc = atoi(argv[2]);
    memset(&addrLoc, 0, sizeof (addrLoc));
    addrLoc.sin_family = AF_INET;
    addrLoc.sin_addr.s_addr = htonl(INADDR_ANY);
    addrLoc.sin_port = htons(portLoc);
    lstnSock = socket(AF_INET, SOCK_STREAM, IPPROTO_TCP);
    if (lstnSock < 0) err("unable to open socket");
    if (bind(lstnSock, (struct sockaddr *) &addrLoc, sizeof (addrLoc)) < 0) err("failed to bind socket");
    if (listen(lstnSock, 1024) < 0) err("failed to listen socket");
    printf("listening on port %i.\n", portLoc);

    ttyName = malloc(strlen(argv[1]) + 1);
    if (ttyName == NULL) err("error allocating memory");
    strcpy(ttyName, argv[1]);
    printf("opening tty %s.\n", ttyName);
    if ((addrTty = open(ttyName, O_RDWR)) < 0) err("unable to open file");

    printf("serving others\n");

    commSock = -1;
    byteRx = 0;
    packRx = 0;
    byteTx = 0;
    packTx = 0;

    if (pthread_create(&threadRx, NULL, (void*) & doRxLoop, NULL)) err("error creating rx thread");
    if (pthread_create(&threadTx, NULL, (void*) & doTxLoop, NULL)) err("error creating tx thread");
    if (pthread_create(&threadAcc, NULL, (void*) & doAccLoop, NULL)) err("error creating accept thread");
    doMainLoop();

}
