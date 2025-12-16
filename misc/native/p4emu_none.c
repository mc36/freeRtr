#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <arpa/inet.h>
#include <sys/socket.h>
#include <netinet/in.h>

#include "p4emu_hdr.h"

#include "utils.h"


int commandSock;
FILE *commandRx;
FILE *commandTx;
int dataPorts;
int cpuPort;
char *ifaceName[maxPorts];
struct ifaceStat_entry *ifaceStat[maxPorts];



void initIface(int port, char *name) {
    ifaceName[port] = malloc(strlen(name)+1);
    if (ifaceName[port] == NULL) err("error allocating memory");
    strcpy(ifaceName[port], name);
    ifaceStat[port] = NULL;
}


int initTables() {
    return 0;
}


int initContext(struct packetContext *ctx) {
    ctx->bufC = malloc(totBuff);
    if (ctx->bufC == NULL) return 1;
    ctx->bufD = malloc(totBuff);
    if (ctx->bufD == NULL) return 1;
    ctx->bufH = malloc(preBuff);
    if (ctx->bufH == NULL) return 1;
    return 0;
}


int doOneCommand(struct packetContext *ctx, unsigned char* buf) {
    return 0;
}



void doNegotiate(char*name) {
    setgid(1);
    setuid(1);
    commandRx = fdopen(commandSock, "r");
    if (commandRx == NULL) err("failed to open file");
    commandTx = fdopen(commandSock, "w");
    if (commandTx == NULL) err("failed to open file");
    fprintf(commandTx, "platform p4pkt/%s\r\n", name);
    fprintf(commandTx, "capabilities nothing\r\n");
    for (int i = 0; i < dataPorts; i++) fprintf(commandTx, "portname %i %s\r\n", i, ifaceName[i]);
    fprintf(commandTx, "cpuport %i\r\n", cpuPort);
    fprintf(commandTx, "dynrange %i 1073741823\r\n", maxPorts);
    fprintf(commandTx, "vrfrange 1 1073741823\r\n");
    fprintf(commandTx, "neirange 4096 1073741823\r\n");
    fprintf(commandTx, "nomore\r\n");
    fflush(commandTx);
}

void doMainLoop() {
    for (;;) sleep(1);
}


void doSockLoop() {
    unsigned char buf[16384];
    for (;;) {
        if (fgets((char*)&buf[0], sizeof(buf), commandRx) == NULL) break;
    }
    err("command thread exited");
}



void doStatLoop() {
    for (;;) {
        sleep(1);
        for (int i = 0; i < dataPorts; i++) {
            int o = getState(i);
            fprintf(commandTx, "state %i %i\r\n", i, o);
        }
        fflush(commandTx);
    }
    err("stat thread exited");
}



int hashDataPacket(unsigned char *bufP) {
    return 0;
}


void processDataPacket(struct packetContext *ctx, int bufS, int prt) {
    unsigned char *bufD = ctx->bufD;
    if (prt == cpuPort) {
        int prt = get32msb(bufD, preBuff);
        if (prt < 0) return;
        if (prt >= dataPorts) return;
        sendPack(&bufD[preBuff + 4], bufS - 4, prt);
    } else {
        put32msb(bufD, preBuff - 4, prt);
        sendPack(&bufD[preBuff - 4], bufS + 4, cpuPort);
    }
}
