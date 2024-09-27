#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <arpa/inet.h>
#include <sys/socket.h>
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
    unsigned char bufD[totBuff];
    unsigned char origD[totBuff];
    *((int*)(&bufD[0])) = 1;
    printf("code=%i, int=%i, long=%i, ptr=%i, ", (int)((char*)&processCpuPack - (char*)&processDataPacket), (int)sizeof(int), (int)sizeof(long), (int)sizeof(int*));
    if (bufD[0] == 1) printf("lsb");
    else printf("msb");
    printf("\n");
    fflush(stdout);
    int origS = 0;
    EVP_CIPHER_CTX *encrCtx = EVP_CIPHER_CTX_new();
    EVP_MD_CTX *hashCtx = EVP_MD_CTX_new();
    processCpuPack(&bufD[0], origS, encrCtx, hashCtx);
    if (argc < 3) err("usage: <commands> <count> <byte0> [byteN]");
    int count = atoi(argv[2]);
    for (int i = 3; i < argc; i++) {
        sscanf(argv[i], "%hhx", &origD[origS]);
        origS++;
    }
    printf("packet=%i, rounds=%i\n", origS, count);
    hexdump(origD, 0, origS);
    dataPorts = 1;
    cpuPort = 1;
    initTables();
    FILE * fil =fopen(argv[1], "r");
    if (fil == NULL) err("error opening commands");
    for (;;) {
        char* lin = NULL;
        size_t len = 0;
        if (getline(&lin, &len, fil) < 0) break;
        doOneCommand((unsigned char*) lin, encrCtx, hashCtx);
        free(lin);
    }
    fclose(fil);
    sleep(1);
    clock_t begin = clock();
    for (int i = 0; i < count; i++) {
        memcpy(&bufD[preBuff], &origD[0], origS);
        processDataPacket(&bufD[0], origS, 0, 0, encrCtx, hashCtx);
    }
    clock_t end = clock();
    double spent = (double)(end - begin) / (double)CLOCKS_PER_SEC;
    if (spent <= 0) spent = 1;
    hexdump(lastB, 0, lastS);
    printf("packets=%li, bytes=%li, time=%f\n", packs, bytes, spent);
    double prn = (double)packs / spent;
    printf("pps=%f, %f mpps\n", prn, prn / 1000000.0);
    prn = (double)bytes * 8.0 / spent;
    printf("bps=%f, %f gbps\n", prn, prn / 1000000000.0);
}
