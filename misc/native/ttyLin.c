#include <arpa/inet.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <netinet/in.h>
#include <termios.h>
#include <fcntl.h>
#include <sys/ioctl.h>
#include <pthread.h>


char *ttyName;
int addrTty;
int portLoc;
int lstnSock;
int commSock;
pthread_t threadRx;
pthread_t threadTx;
pthread_t threadAcc;
struct sockaddr_in addrLoc;
long int byteRx;
long int packRx;
long int byteTx;
long int packTx;

void err(char*buf) {
    printf("%s\n", buf);
    exit(1);
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
    unsigned char buf[8192];
    int i;
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
        write(addrTty, &buf, i);
        byteTx += i;
        packTx++;
    }
}

void doRxLoop() {
    unsigned char buf[8192];
    int i;
    while (1) {
        if (ioctl(addrTty, FIONREAD, &i) < 0) i = 1;
        if (i < 1) {
            i = 1;
        }
        if (i>sizeof (buf)) i = sizeof (buf);
        i = read(addrTty, &buf, i);
        if (i < 1) {
            sleep(1);
            continue;
        }
        send(commSock, &buf, i, 0);
        byteRx += i;
        packRx++;
    }
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
                exit(1);
                break;
            default:
                err("unknown command, try -h");
                break;
        }
        exit(1);
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

    ttyName = malloc(1024);
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
