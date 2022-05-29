#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <pthread.h>
#include <arpa/inet.h>

#undef debugging
#undef basicLoop

#include "p4cns.h"


struct sockaddr_in peers[maxPorts];
int sockets[maxPorts];
pthread_t threadRaw[maxPorts];
int commandSock;
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
    exit(1);
}


#include "p4tab.h"
#include "p4msg.h"
#include "p4fwd.h"



void doIfaceLoop(int * param) {
    int port = *param;
    int commSock = sockets[port];
    unsigned char bufA[16384];
    unsigned char bufB[16384];
    unsigned char bufC[16384];
    unsigned char bufD[16384];
    struct sockaddr_in addrTmp;
    unsigned int addrLen;
    int bufS;
    EVP_CIPHER_CTX *encrCtx = EVP_CIPHER_CTX_new();
    if (encrCtx == NULL) err("error getting encr context");
    EVP_MD_CTX *hashCtx = EVP_MD_CTX_new();
    if (hashCtx == NULL) err("error getting hash context");
    if (port == cpuport) {
        for (;;) {
            addrLen = sizeof(addrTmp);
            bufS = sizeof(bufD) - preBuff;
            bufS = recvfrom(commSock, &bufD[preBuff], bufS, 0, (struct sockaddr *) &addrTmp, &addrLen);
            if (bufS < 0) break;
            processCpuPack(&bufA[0], &bufB[0], &bufC[0], &bufD[0], bufS, encrCtx, hashCtx);
        }
    } else {
        for (;;) {
            addrLen = sizeof(addrTmp);
            bufS = sizeof(bufD) - preBuff;
            bufS = recvfrom(commSock, &bufD[preBuff], bufS, 0, (struct sockaddr *) &addrTmp, &addrLen);
            if (bufS < 0) break;
            processDataPacket(&bufA[0], &bufB[0], &bufC[0], &bufD[0], bufS, port, port, encrCtx, hashCtx);
        }
    }
    err("port thread exited");
}


void doSockLoop() {
    FILE *commands = fdopen(commandSock, "r");
    if (commands == NULL) err("failed to open file");
    unsigned char buf[1024];
    for (;;) {
        memset(&buf, 0, sizeof(buf));
        if (fgets((char*)&buf[0], sizeof(buf), commands) == NULL) break;
        if (doOneCommand(&buf[0]) != 0) break;
    }
    err("command thread exited");
}



void doStatLoop() {
    FILE *commands = fdopen(commandSock, "w");
    if (commands == NULL) err("failed to open file");
    fprintf(commands, "platform %spcap\r\n", platformBase);
    fprintf(commands, "capabilities %s\r\n", capabilities);
    for (int i = 0; i < ports; i++) fprintf(commands, "portname %i %s\r\n", i, ifaceName[i]);
    fprintf(commands, "cpuport %i\r\n", cpuport);
    fprintf(commands, "dynrange %i 65535\r\n", maxPorts);
    fflush(commands);
    int rnd = 0;
    for (;;) {
        doStatRound(commands, rnd);
        rnd++;
        usleep(100000);
    }
    err("stat thread exited");
}


void doMainLoop() {
    unsigned char buf[1024];
    for (;;) {
        printf("> ");
        buf[0] = 0;
        int i = scanf("%1023s", buf);
        if (i < 1) {
            sleep(1);
            continue;
        }
        if (doConsoleCommand(&buf[0]) != 0) break;
        printf("\n");
    }
    err("main thread exited");
}



int main(int argc, char **argv) {
    ports = (argc - 6) / 2;
    if (ports < 2) err("using: dp <addr> <port> <cpuport> <laddr> <raddr> <lport1> <rport1> <lport2> <rport2> ...");
    if (ports > maxPorts) ports = maxPorts;
    struct sockaddr_in addrLoc;
    memset(&addrLoc, 0, sizeof(addrLoc));
    if (inet_aton(argv[4], &addrLoc.sin_addr) == 0) err("bad laddr address");
    addrLoc.sin_family = AF_INET;
    for (int i = 0; i < ports; i++) {
        unsigned char buf[1024];
        sprintf((char*)&buf[0], "port-%i", i);
        initIface(ports, (char*)&buf[0]);
        memset(&peers[i], 0, sizeof(peers[i]));
        if (inet_aton(argv[5], &peers[i].sin_addr) == 0) err("bad raddr address");
        addrLoc.sin_port = htons(atoi(argv[(i*2)+6]));
        peers[i].sin_family = AF_INET;
        peers[i].sin_port = htons(atoi(argv[(i*2)+7]));
        if ((sockets[i] = socket(AF_INET, SOCK_DGRAM, IPPROTO_UDP)) < 0) err("unable to open socket");
        if (bind(sockets[i], (struct sockaddr *) &addrLoc, sizeof(addrLoc)) < 0) err("failed to bind socket");
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
    if(connect(commandSock, (struct sockaddr*)&addrLoc, sizeof(addrLoc)) < 0) err("failed to connect socket");
    cpuport = atoi(argv[3]);
    printf("cpu port is #%i of %i...\n", cpuport, ports);
    pthread_t threadSock;
    if (pthread_create(&threadSock, NULL, (void*) & doSockLoop, NULL)) err("error creating socket thread");
    pthread_t threadStat;
    if (pthread_create(&threadStat, NULL, (void*) & doStatLoop, NULL)) err("error creating status thread");
    for (int i=0; i < ports; i++) {
        if (pthread_create(&threadRaw[i], NULL, (void*) & doIfaceLoop, &ifaceId[i])) err("error creating port thread");
    }
    doMainLoop();
}
