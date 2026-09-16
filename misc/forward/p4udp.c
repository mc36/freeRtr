#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <pthread.h>
#include <arpa/inet.h>
#include <sys/socket.h>
#include <netinet/in.h>


#include "p4emu_hdr.h"


struct sockaddr_in peers[maxPorts];
int sockets[maxPorts];
int ifaceId[maxPorts];


void sendPack(unsigned char *bufD, int bufS, int port) {
    sendto(sockets[port], bufD, bufS, 0, (struct sockaddr *) &peers[port], sizeof(peers[port]));
}


void setMtu(int port, int mtu) {
}

void setState(int port, int sta) {
}

int getState(int port) {
    return 1;
}


void getStats(int port, unsigned char*buf, unsigned char*pre, int*len) {
}

void err(char*buf) {
    printf("%s\n", buf);
    _exit(1);
}






void doIfaceLoop(int * param) {
    int port = *param;
    int commSock = sockets[port];
    struct sockaddr_in addrTmp;
    unsigned int addrLen;
    int bufS;
    struct packetContext ctx;
    if (initContext(&ctx) != 0) err("error initializing context");
    unsigned char *bufD = ctx.bufD;
    ctx.stat = ifaceStat[port];
    for (;;) {
        addrLen = sizeof(addrTmp);
        bufS = totBuff - preBuff;
        bufS = recvfrom(commSock, &bufD[preBuff], bufS, 0, (struct sockaddr *) &addrTmp, &addrLen);
        if (bufS < 0) break;
        processDataPacket(&ctx, bufS, port);
    }
    err("port thread exited");
}



int main(int argc, char **argv) {
    dataPorts = (argc - 5) / 2;
    if (dataPorts < 2) err("using: dp <addr> <port> <cpuport> <laddr> <raddr> <lport1> <rport1> [lportN] [rportN]");
    if (dataPorts > maxPorts) dataPorts = maxPorts;
    struct sockaddr_in addrLoc;
    memset(&addrLoc, 0, sizeof(addrLoc));
    if (inet_aton(argv[4], &addrLoc.sin_addr) == 0) err("bad laddr address");
    addrLoc.sin_family = AF_INET;
    for (int i = 0; i < dataPorts; i++) {
        unsigned char buf[1024];
        sprintf((char*)&buf[0], "udport-%i", i);
        initIface(i, (char*)&buf[0]);
        memset(&peers[i], 0, sizeof(peers[i]));
        if (inet_aton(argv[5], &peers[i].sin_addr) == 0) err("bad raddr address");
        addrLoc.sin_port = htons(atoi(argv[(i*2)+6]));
        peers[i].sin_family = AF_INET;
        peers[i].sin_port = htons(atoi(argv[(i*2)+7]));
        if ((sockets[i] = socket(AF_INET, SOCK_DGRAM, IPPROTO_UDP)) < 0) err("unable to open socket");
        if (bind(sockets[i], (struct sockaddr *) &addrLoc, sizeof(addrLoc)) < 0) err("failed to bind socket");
        if (connect(sockets[i], (struct sockaddr *) &peers[i], sizeof(addrLoc)) < 0) err("failed to connect socket");
        int sockOpt = 524288;
        setsockopt(sockets[i], SOL_SOCKET, SO_RCVBUF, &sockOpt, sizeof(sockOpt));
        setsockopt(sockets[i], SOL_SOCKET, SO_SNDBUF, &sockOpt, sizeof(sockOpt));
        ifaceId[i] = i;
    }
    if (initTables() != 0) err("error initializing tables");
    int port = atoi(argv[2]);
    memset(&addrLoc, 0, sizeof(addrLoc));
    if (inet_aton(argv[1], &addrLoc.sin_addr) == 0) err("bad addr address");
    addrLoc.sin_family = AF_INET;
    addrLoc.sin_port = htons(port);
    printf("connecting %s %i.\n", inet_ntoa(addrLoc.sin_addr), port);
    commandSock = socket(AF_INET, SOCK_STREAM, 0);
    if (commandSock < 0) err("unable to open socket");
    if (connect(commandSock, (struct sockaddr*)&addrLoc, sizeof(addrLoc)) < 0) err("failed to connect socket");
    cpuPort = atoi(argv[3]);
    printf("cpu port is #%i of %i...\n", cpuPort, dataPorts);

    doNegotiate("udp");
    pthread_t threadSock;
    if (pthread_create(&threadSock, NULL, (void*) & doSockLoop, NULL)) err("error creating socket thread");
    pthread_t threadStat;
    if (pthread_create(&threadStat, NULL, (void*) & doStatLoop, NULL)) err("error creating status thread");
    pthread_t threadRaw[maxPorts];
    for (int i=0; i < dataPorts; i++) {
        if (pthread_create(&threadRaw[i], NULL, (void*) & doIfaceLoop, &ifaceId[i])) err("error creating port thread");
    }
    doMainLoop();
}
