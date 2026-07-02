#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <arpa/inet.h>
#include <sys/socket.h>
#include <sys/utsname.h>
#include <netinet/in.h>
#include <time.h>
#include <profile/instr_prof_interface.h>


void err(char*buf) {
    printf("%s\n", buf);
    _exit(1);
}

#undef HAVE_DEBUG
#undef HAVE_NOCRYPTO
#undef HAVE_NOCACHE
#define HAVE_NOHASH

#include "utils.h"
#include "table.h"
#include "tree.h"
#include "types.h"

#include "p4emu_hdr.h"
#include "p4emu_tab.h"
#include "p4emu_fwd.h"
#include "p4emu_msg.h"
#include "p4emu_tester.h"

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



int main(int argc, char **argv) {
    unsigned char origD[16384];
    dataPorts = 1;
    cpuPort = 1;
    initIface(0, "bench");
    initTables();
    struct packetContext ctx;
    ctx.stat = ifaceStat[0];
    initContext(&ctx);
    fflush(stdout);
    int origS = 0;
    if (argc < 3) err("usage: <commands> <bytes> <result>");
    readTestCommands(argv[1], &ctx);
    origS = readTestBytes(argv[2], origD);
    memcpy(&ctx.bufD[preBuff], &origD[0], origS);
    __llvm_profile_reset_counters();
    processDataPacket(&ctx, origS, 0);
    __llvm_profile_set_filename(argv[3]);
}
