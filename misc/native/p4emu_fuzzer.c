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
    if (dataPorts > 0) {
        memcpy(&ctx.bufD[preBuff], &data[0], size);
        processDataPacket(&ctx, size, 0);
        return 0;
    }
    dataPorts = 1;
    cpuPort = 1;
    initIface(0, "bench");
    initTables();
    ctx.port = 0;
    ctx.stat = ifaceStat[0];
    if (initContext(&ctx) != 0) err("error initializing context");
    FILE* fil = fopen("p4emu_bench_cmds.txt", "r");
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
