#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <arpa/inet.h>
#include <sys/socket.h>
#include <sys/utsname.h>
#include <netinet/in.h>
#include <time.h>

#include "p4emu_hdr.h"
#include "dump.h"

long packs = 0;
long bytes = 0;
unsigned char *lastB = NULL;
int lastS = 0;


void sendPack(unsigned char *bufD, int bufS, int port) {
    packs++;
    bytes += bufS;
    lastB = bufD;
    lastS = bufS;
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


int main(int argc, char **argv) {
    struct utsname unamei;
    unsigned char origD[16384];
    uname(&unamei);
    *((int*)(&origD[0])) = 1;
    printf("code=%i, int=%i, long=%i, ptr=%i, order=", (int)((char*)&doOneCommand - (char*)&hashDataPacket), (int)sizeof(int), (int)sizeof(long), (int)sizeof(int*));
    if (origD[0] == 1) printf("lsb");
    else printf("msb");
    printf(", arch=%s\n", unamei.machine);
    fflush(stdout);
    int origS = 0;
    if (argc < 3) err("usage: <commands> <count> <bytes>");
    int count = atoi(argv[2]);
    dataPorts = 1;
    cpuPort = 1;
    initIface(0, "bench");
    initTables();
    struct packetContext ctx;
    if (initContext(&ctx) != 0) err("error initializing context");
    FILE* fil = fopen(argv[1], "r");
    if (fil == NULL) err("error opening commands");
    for (;;) {
        char* lin = NULL;
        size_t len = 0;
        if (getline(&lin, &len, fil) < 0) break;
        doOneCommand(&ctx, (unsigned char*) lin);
        free(lin);
    }
    fclose(fil);
    fil = fopen(argv[3], "r");
    if (fil == NULL) err("error opening bytes");
    for (;;) {
        char* lin = NULL;
        size_t len = 0;
        if (getline(&lin, &len, fil) < 0) break;
        for (int i = 0;; i++) {
            if (lin[i] == 0) break;
            if (lin[i] == 32) continue;
            if (sscanf(&lin[i], "%hhx", &origD[origS]) != 1) continue;
            origS++;
            i++;
        }
        free(lin);
    }
    fclose(fil);
    hexdump(origD, 0, origS);
    printf("input=%i, rounds=%i", origS, count);
    ctx.stat = ifaceStat[0];
    sleep(1);
    clock_t beg = clock();
    for (int i = 0; i < count; i++) {
        memcpy(&ctx.bufD[preBuff], &origD[0], origS);
        processDataPacket(&ctx, origS, 0);
    }
    clock_t end = clock();
    double spent = (double)(end - beg) / (double)CLOCKS_PER_SEC;
    if (spent <= 0) spent = 1;
    printf(", output=%i\n", lastS);
    hexdump(lastB, 0, lastS);
    printf("packets=%li, bytes=%li, time=%f\n", packs, bytes, spent);
    double prn = (double)packs / spent;
    printf("pps=%f, %f mpps\n", prn, prn / 1000000.0);
    prn = (double)bytes * 8.0 / spent;
    printf("bps=%f, %f gbps\n", prn, prn / 1000000000.0);
}
