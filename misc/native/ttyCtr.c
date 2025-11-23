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


char *ttyName;
int addrTty;
int portLoc;
int lstnSock;
int commSock;
pthread_t threadRx;
pthread_t threadTx;
pthread_t threadAcc;
struct sockaddr_in addrLoc;
long byteRx;
long byteTx;
int lastVal;

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
        lastVal = -1;
    }
}

void doTxLoop() {
    unsigned char buf[1024];
    int i, o;
    while (1) {
        if (commSock == -1) {
            sleep(1);
            continue;
        }
        if (ioctl(commSock, FIONREAD, &i) < 0) i = 1;
        if (i < 1) {
            i = 1;
        }
        if (i>sizeof (buf)) i = sizeof (buf);
        i = recv(commSock, &buf, i, 0);
        if (i < 1) {
            sleep(1);
            continue;
        }
        o = buf[i - 1] - 0x30;
        ioctl(addrTty, TIOCMGET, &i);
        i &= ~(TIOCM_DTR | TIOCM_RTS);
        if (o & 1) i |= TIOCM_DTR;
        if (o & 2) i |= TIOCM_RTS;
        ioctl(addrTty, TIOCMSET, &i);
        byteTx += i;
    }
}

void doRxLoop() {
    unsigned char buf[1024];
    int i;
    while (1) {
        ioctl(addrTty, TIOCMGET, &i);
        if (i == lastVal) {
            sleep(1);
            continue;
        }
        lastVal = i;
        i = 0;
        if ((lastVal & TIOCM_DTR) != 0) i |= 0x1;
        if ((lastVal & TIOCM_RTS) != 0) i |= 0x2;
        if ((lastVal & TIOCM_DSR) != 0) i |= 0x4;
        if ((lastVal & TIOCM_CTS) != 0) i |= 0x8;
        if ((lastVal & TIOCM_CD) != 0) i |= 0x10;
        if ((lastVal & TIOCM_RI) != 0) i |= 0x20;
        buf[0] = 0x30 + i;
        send(commSock, &buf, 1, 0);
        byteRx++;
    }
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
        printf("                      bytes\n");
        printf("received %20li\n", byteRx);
        printf("sent     %20li\n", byteTx);
        break;
    case 'C':
    case 'c':
        printf("counters cleared.\n");
        byteRx = 0;
        byteTx = 0;
        break;
    default:
        printf("unknown command '%s', try ?\n", buf);
        break;
    }
    printf("\n");

    goto doer;
}

int main(int argc, char **argv) {

    if (argc < 3) {
        if (argc <= 1) goto help;
        char*curr = argv[1];
        if ((curr[0] == '-') || (curr[0] == '/')) curr++;
        switch (curr[0]) {
        case 'V':
        case 'v':
            err("tty console v1.0");
            break;
        case '?':
        case 'h':
        case 'H':
help :
            curr = argv[0];
            printf("using: %s <tty> <lport>\n", curr);
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
    lastVal = -1;
    byteRx = 0;
    byteTx = 0;

    if (pthread_create(&threadRx, NULL, (void*) & doRxLoop, NULL)) err("error creating rx thread");
    if (pthread_create(&threadTx, NULL, (void*) & doTxLoop, NULL)) err("error creating tx thread");
    if (pthread_create(&threadAcc, NULL, (void*) & doAccLoop, NULL)) err("error creating accept thread");
    doMainLoop();

}
