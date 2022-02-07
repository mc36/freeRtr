#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <pthread.h>
#include <arpa/inet.h>
#include <time.h>

#undef debugging
#undef basicLoop

#include "p4cns.h"

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

void err(char*buf) {
    printf("%s\n", buf);
    exit(1);
}

#include "p4tab.h"
#include "p4fwd.h"
#include "dump.h"

int main(int argc, char **argv) {
    unsigned char bufA[16384];
    unsigned char bufB[16384];
    unsigned char bufC[16384];
    unsigned char bufD[16384];
    unsigned char origD[16384];
    int origS = 0;
    EVP_CIPHER_CTX *encrCtx = EVP_CIPHER_CTX_new();
    EVP_MD_CTX *hashCtx = EVP_MD_CTX_new();
    processCpuPack(&bufA[0], &bufB[0], &bufC[0], &bufD[0], origS, encrCtx, hashCtx);
    if (argc < 2) err("usage: <count> <pack>");
    int count = atoi(argv[1]);
    for (int i = 2; i < argc; i++) {
        sscanf(argv[i], "%hhx", &origD[origS]);
        origS++;
    }
    printf("codesize: %i\n", (int)((char*)&processCpuPack - (char*)&processDataPacket));
    printf("rounds: %i\n", count);
    hexdump(origD, 0, origS);
    ports = 1;
    cpuport = 1;
    initTables();
    struct portvrf_entry portvrf_ntry;
    memset(&portvrf_ntry, 0, sizeof(portvrf_ntry));
    struct vrf2rib_entry vrf2rib_ntry;
    memset(&vrf2rib_ntry, 0, sizeof(vrf2rib_ntry));
    struct route4_entry route4_ntry;
    memset(&route4_ntry, 0, sizeof(route4_ntry));
    struct route6_entry route6_ntry;
    memset(&route6_ntry, 0, sizeof(route6_ntry));
    struct mpls_entry mpls_ntry;
    memset(&mpls_ntry, 0, sizeof(mpls_ntry));
    struct neigh_entry neigh_ntry;
    memset(&neigh_ntry, 0, sizeof(neigh_ntry));
    portvrf_ntry.command = 1;
    portvrf_ntry.port = 0;
    portvrf_ntry.vrf = 1;
    portvrf_ntry.mpls = 1;
    table_add(&portvrf_table, &portvrf_ntry);
    vrf2rib_ntry.vrf = 1;
    struct vrf2rib_entry *route4_table = vrf2rib_init4;
    struct vrf2rib_entry *route6_table = vrf2rib_init6;
    route4_ntry.addr = 0x01010100;
    route4_ntry.mask = 24;
    route4_ntry.nexthop = 1234;
    route4_ntry.command = 1;
    tree_add(&route4_table->rou, &route4_ntry);
    route6_ntry.addr1 = 0x01010101;
    route6_ntry.addr2 = 0x0;
    route6_ntry.addr3 = 0x0;
    route6_ntry.addr4 = 0x0;
    route6_ntry.mask = 64;
    route6_ntry.nexthop = 1234;
    route6_ntry.command = 1;
    tree_add(&route6_table->rou, &route6_ntry);
    mpls_ntry.label = 12345;
    mpls_ntry.nexthop = 1234;
    mpls_ntry.swap = 54321;
    mpls_ntry.ver = 4;
    mpls_ntry.command = 3;
    table_add(&mpls_table, &mpls_ntry);
    neigh_ntry.id = 1234;
    neigh_ntry.vrf = 1;
    neigh_ntry.aclport = neigh_ntry.port = 0;
    neigh_ntry.command = 1;
    neigh_ntry.dmac[1] = 0xab;
    neigh_ntry.smac[1] = 0xcd;
    table_add(&neigh_table, &neigh_ntry);
    clock_t begin = clock();
    for (int i = 0; i < count; i++) {
        memcpy(&bufD[preBuff], &origD[0], origS);
        processDataPacket(&bufA[0], &bufB[0], &bufC[0], &bufD[0], origS, 0, 0, encrCtx, hashCtx);
    }
    clock_t end = clock();
    double spent = (double)(end - begin) / CLOCKS_PER_SEC;
    if (spent <= 0) spent = 1;
    hexdump(lastB, 0, lastS);
    printf("packets: %li\n", packs);
    printf("bytes: %li\n", bytes);
    printf("time: %f\n", spent);
    double prn = (double)packs / spent;
    printf("pps: %f, %f mpps\n", prn, prn / 1000000);
    prn = (double)bytes * 8.0 / spent;
    printf("bps: %f, %f gbps\n", prn, prn / 1000000000);
}
