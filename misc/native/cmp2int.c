#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <pthread.h>
#include <arpa/inet.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <signal.h>


char *ifaceName;
char *ifaceRecv;
char *ifaceXmit;
struct sockaddr_in addrLoc;
struct sockaddr_in addrRem;
int portLoc;
int portRem;
int commSock;
int dumpFifo;
int playFifo;
int dumpChld;
int playChld;
pthread_t threadUdp;
pthread_t threadRaw;
pthread_t threadStat;
long byteRx;
long packRx;
long byteTx;
long packTx;


void err(char*buf) {
    printf("%s\n", buf);
    _exit(1);
}

#define packHead 16


void doRawLoop() {
    unsigned char bufD[16384];
    int bufS;
    for (;;) {
        bufS = sizeof (bufD);
        bufS = read(dumpFifo, bufD, bufS);
        if (bufS < 0) break;
        packRx++;
        byteRx += bufS;
        send(commSock, bufD + packHead, bufS - packHead, 0);
    }
    kill(dumpChld, SIGKILL);
    kill(playChld, SIGKILL);
    err("raw thread exited");
}

void doUdpLoop() {
    unsigned char bufD[16384];
    int* bufI = (int*)&bufD;
    int bufS;
    for (;;) {
        bufS = sizeof (bufD) - packHead;
        bufS = recv(commSock, bufD + packHead, bufS, 0);
        if (bufS < 0) break;
        packTx++;
        byteTx += bufS;
        bufI[0] = 0;
        bufI[1] = 0;
        bufI[2] = bufS;
        bufI[3] = bufS;
        if (write(playFifo, bufD, bufS + packHead) < 0) break;
    }
    kill(dumpChld, SIGKILL);
    kill(playChld, SIGKILL);
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
        kill(dumpChld, SIGKILL);
        kill(playChld, SIGKILL);
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
            err("compatible interface driver v1.0\n");
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

    if ((commSock = socket(AF_INET, SOCK_DGRAM, IPPROTO_UDP)) < 0) err("unable to udp open socket");
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
    printf("opening interface %s.\n", ifaceName);
    ifaceRecv = malloc(strlen(argv[1]) + 10);
    if (ifaceRecv == NULL) err("error allocating memory");
    ifaceXmit = malloc(strlen(argv[1]) + 10);
    if (ifaceXmit == NULL) err("error allocating memory");
    strcpy(ifaceRecv, "/tmp/rx-");
    strcpy(&(ifaceRecv[8]), argv[1]);
    strcpy(ifaceXmit, "/tmp/tx-");
    strcpy(&(ifaceXmit[8]), argv[1]);
    printf("will use %s and %s fifos\n", ifaceRecv, ifaceXmit);
    unlink(ifaceRecv);
    unlink(ifaceXmit);
    mkfifo(ifaceRecv, 0600);
    mkfifo(ifaceXmit, 0600);


    playChld = fork();
    if (playChld == 0) {
        if (execlp("tcpreplay", "tcpreplay", "-q", "-t", "-i", ifaceName, ifaceXmit, (char*) NULL) == -1) err("error executing process");
        return 0;
    }
    dumpChld = fork();
    if (dumpChld == 0) {
        if (execlp("tcpdump", "tcpdump", "--immediate-mode", "-O", "-U", "-Q", "in", "-i", ifaceName, "-w", ifaceRecv, (char*) NULL) == -1) err("error executing process");
        return 0;
    }

    if ((playFifo = open(ifaceXmit, O_WRONLY)) < 0) err("unable to open tx fifo");
    if ((dumpFifo = open(ifaceRecv, O_RDONLY)) < 0) err("unable to open rx fifo");
    char buf[1024];
    int i = read(dumpFifo, buf, sizeof(buf));
    write(playFifo, buf, i);
    printf("tcpreplay is %i, tcpdump is %i, header is %i, record is %i\n", playChld, dumpChld, i, packHead);

    setgid(1);
    setuid(1);
    printf("serving others\n");

    if (pthread_create(&threadRaw, NULL, (void*) & doRawLoop, NULL)) err("error creating raw thread");
    if (pthread_create(&threadUdp, NULL, (void*) & doUdpLoop, NULL)) err("error creating udp thread");

    doMainLoop();
}
