#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <arpa/inet.h>
#include <sys/socket.h>
#include <netinet/in.h>

#include "p4emu_hdr.h"

#include "utils.h"


int dataPorts;
int cpuPort;
char *ifaceName[maxPorts];



char* getCapas() {
    return "nothing";
}


void initIface(int port, char *name) {
    ifaceName[port] = malloc(strlen(name)+1);
    if (ifaceName[port] == NULL) err("error allocating memory");
    strcpy(ifaceName[port], name);
}


int initTables() {
    return 0;
}


int initContext(struct packetContext *ctx) {
    return 0;
}


int doOneCommand(struct packetContext *ctx, unsigned char* buf) {
    return 0;
}


void doStatRound(FILE *commands, int round) {
    if ((round % 10) != 0) return;
    for (int i = 0; i < dataPorts; i++) {
        int o = getState(i);
        fprintf(commands, "state %i %i\r\n", i, o);
    }
    fflush(commands);
}


int doConsoleCommand(unsigned char*buf) {
    return 0;
}


int hashDataPacket(unsigned char *bufP) {
    return 0;
}


void processDataPacket(struct packetContext *ctx, int bufS, int port, int prt) {
    unsigned char *bufD = ctx->bufD;
    put16msb(bufD, preBuff - 2, port);
    sendPack(&bufD[preBuff - 2], bufS + 2, cpuPort);
}


void processCpuPack(struct packetContext *ctx, int bufS) {
    unsigned char *bufD = ctx->bufD;
    int prt = get16msb(bufD, preBuff);
    if (prt < 0) return;
    if (prt >= dataPorts) return;
    sendPack(&bufD[preBuff + 2], bufS - 2, prt);
}
