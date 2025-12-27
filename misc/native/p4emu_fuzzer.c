#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <arpa/inet.h>

void err(char*buf) {
    printf("%s\n", buf);
    _exit(1);
}

#include "utils.h"
#include "table.h"
#include "tree.h"
#include "types.h"

#include "p4emu_hdr.h"
#include "p4emu_tab.h"
#include "p4emu_fwd.h"
#include "p4emu_msg.h"



void sendPack(unsigned char *bufD, int bufS, int port) {
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



struct packetContext ctx;


int LLVMFuzzerTestOneInput(unsigned char *data, size_t size) {
    memcpy(&ctx.bufD[preBuff], &data[0], size);
    processDataPacket(&ctx, size, 0);
    return 0;
}


int LLVMFuzzerInitialize(int *argc, char ***argv) {
    if (*argc <= 1) err("usage: --<commands>");
    dataPorts = 1;
    cpuPort = 1;
    initIface(0, "bench");
    initTables();
    ctx.stat = ifaceStat[0];
    initContext(&ctx);
    FILE* fil = fopen(&(*argv)[1][2], "r");
    if (fil == NULL) err("error opening commands");
    for (;;) {
        char* lin = NULL;
        size_t len = 0;
        if (getline(&lin, &len, fil) < 0) break;
        doOneCommand(&ctx, (unsigned char*) lin);
        free(lin);
    }
    fclose(fil);
    return 0;
}
