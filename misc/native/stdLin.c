#include <arpa/inet.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <netinet/in.h>
#include <pthread.h>
#include <termios.h>


int portLoc;
int lstnSock;
int commSock;
pthread_t threadRx;
pthread_t threadTx;
struct sockaddr_in addrLoc;
long int byteRx;
long int byteTx;

void err(char*buf) {
    printf("%s\n", buf);
    fflush(stdout);
    exit(1);
}

void setupOneTerm(int fd) {
    struct termios tmp_tc;
    tcgetattr(fd, &tmp_tc);
    tmp_tc.c_lflag &= ~ECHO;
    tmp_tc.c_oflag &= ~ONLCR;
    tmp_tc.c_iflag &= ~ICRNL;
    tmp_tc.c_iflag &= ~(IXANY | IXOFF | IXON);
    tmp_tc.c_lflag &= ~ICANON;
    tmp_tc.c_iflag &= ~ICRNL;
    tmp_tc.c_cc[VMIN] = 1;
    tmp_tc.c_cc[VTIME] = 0;
    tmp_tc.c_cc[VLNEXT] = (cc_t) (_POSIX_VDISABLE);
    tmp_tc.c_cc[VINTR] = 0;
    tmp_tc.c_cc[VSTART] = 0;
    tmp_tc.c_cc[VSTOP] = 0;
    tmp_tc.c_cc[VSUSP] = 0;
    tmp_tc.c_oflag &= ~TABDLY;
    tmp_tc.c_lflag |= ECHOCTL;
    tmp_tc.c_iflag &= ~ISTRIP;
    tmp_tc.c_cflag &= ~(CSIZE | PARENB);
    tmp_tc.c_cflag |= CS8;
    tmp_tc.c_oflag &= ~OPOST;
    memset(&tmp_tc, 0, sizeof (tmp_tc));
    tcsetattr(fd, TCSANOW, &tmp_tc);
    tcflush(fd, TCIFLUSH);
}

void setupTerminal() {
    setupOneTerm(STDIN_FILENO);
    setupOneTerm(STDOUT_FILENO);
}

void doMainLoop() {
    while (1) {
        int oldSock = commSock;
        commSock = accept(lstnSock, NULL, NULL);
        if (oldSock != -1) {
            close(oldSock);
        }
        if (commSock < 0) err("error accepting socket");
        printf("\15\12\15\12accepted connection\15\12");
        fflush(stdout);
    }
}

void doTxLoop() {
    unsigned char buf[1];
    int i;
    while (1) {
        i = recv(commSock, &buf, sizeof (buf), 0);
        if (i < 1) {
            sleep(1);
            continue;
        }
        write(STDOUT_FILENO, &buf, i);
        byteTx += i;
    }
}

void doRxLoop() {
    int i;
    unsigned char buf[1];
    while (1) {
        i = read(STDIN_FILENO, &buf, sizeof (buf));
        if (i < 1) {
            sleep(1);
            continue;
        }
        send(commSock, &buf, i, 0);
        byteRx += i;
    }
}

int main(int argc, char **argv) {
    if (argc <= 1) goto help;
    char*curr = argv[1];

    portLoc = atoi(curr);
    if (portLoc < 1) {
        if ((curr[0] == '-') || (curr[0] == '/')) curr++;
        switch (curr[0]) {
            case 'V':
            case 'v':
                err("std io console v1.0");
                break;
            case '?':
            case 'h':
            case 'H':
                help :
                        curr = argv[0];
                printf("using: %s <lport>\n", curr);
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

    memset(&addrLoc, 0, sizeof (addrLoc));
    addrLoc.sin_family = AF_INET;
    addrLoc.sin_addr.s_addr = htonl(INADDR_ANY);
    addrLoc.sin_port = htons(portLoc);
    lstnSock = socket(AF_INET, SOCK_STREAM, IPPROTO_TCP);
    if (lstnSock < 0) err("unable to open socket");
    if (bind(lstnSock, (struct sockaddr *) &addrLoc, sizeof (addrLoc)) < 0) err("failed to bind socket");
    if (listen(lstnSock, 1024) < 0) err("failed to listen socket");
    printf("listening on port %i.\n", portLoc);

    printf("serving others\n");

    commSock = -1;
    byteRx = 0;
    byteTx = 0;

    setupTerminal();
    if (pthread_create(&threadRx, NULL, (void*) & doRxLoop, NULL)) err("error creating rx thread");
    if (pthread_create(&threadTx, NULL, (void*) & doTxLoop, NULL)) err("error creating tx thread");
    doMainLoop();
}
