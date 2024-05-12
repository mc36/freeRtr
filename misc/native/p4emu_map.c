#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <pthread.h>
#include <arpa/inet.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <poll.h>
#include <linux/if_ether.h>
#include <linux/if.h>
#include <linux/if_packet.h>
#include <sys/ioctl.h>
#include <sys/mman.h>


#include "p4emu_hdr.h"
#include "utils.h"

#define blocksMax 64

char *ifaceName[maxPorts];
int ifaceIndex[maxPorts];
int ifaceSock[maxPorts];
uint8_t *ifaceMem[maxPorts];
struct iovec *ifaceIov[maxPorts];
struct pollfd ifacePfd[maxPorts];
struct sockaddr_ll addrIfc[maxPorts];

void sendPack(unsigned char *bufD, int bufS, int port) {
    sendto(ifaceSock[port], bufD, bufS, 0, (struct sockaddr *) &addrIfc[port], sizeof (addrIfc[port]));
}

void setMtu(int port, int mtu) {
}

void setState(int port, int sta) {
}

int getState(int port) {
    struct ifreq ifr;
    memset(&ifr, 0, sizeof (ifr));
    strcpy(ifr.ifr_name, ifaceName[port]);
    if (ioctl(ifaceSock[port], SIOCGIFFLAGS, &ifr) < 0) return 0;
    int needed = IFF_RUNNING | IFF_UP;
    if ((ifr.ifr_flags & needed) == needed) return 1;
    return 0;
}


void getStats(int port, unsigned char*buf, unsigned char*pre, int*len) {
}



void err(char*buf) {
    printf("%s\n", buf);
    _exit(1);
}




pthread_t threadRaw[maxPorts];
int commandSock;
int ifaceId[maxPorts];



#define packBeg                                                                                     \
    pbd = (struct tpacket_block_desc *) ifaceIov[port][blockNum].iov_base;                          \
    if ((pbd->hdr.bh1.block_status & TP_STATUS_USER) == 0) {                                        \
        poll(&ifacePfd[port], 1, 1);                                                                \
        continue;                                                                                   \
    }                                                                                               \
    int pkts = pbd->hdr.bh1.num_pkts;                                                               \
    ppd = (struct tpacket3_hdr *) ((uint8_t *) pbd + pbd->hdr.bh1.offset_to_first_pkt);             \


#define packOne                                                                                     \
    bufS = ppd->tp_snaplen;                                                                         \
    pack = (unsigned char *) ppd + ppd->tp_mac;                                                     \
    if ((ppd->tp_status & TP_STATUS_VLAN_VALID) == 0) memcpy(&bufD[preBuff], pack, bufS); else {    \
        if ((ppd->tp_status & TP_STATUS_VLAN_TPID_VALID) == 0) ppd->hv1.tp_vlan_tpid = ETH_P_8021Q; \
        memcpy(&bufD[preBuff], pack, 12);                                                           \
        put16msb(bufD, preBuff + 12, ppd->hv1.tp_vlan_tpid);                                        \
        put16msb(bufD, preBuff + 14, ppd->hv1.tp_vlan_tci);                                         \
        memcpy(&bufD[preBuff + 16], pack + 12, bufS - 12);                                          \
        bufS += 4;                                                                                  \
    }                                                                                               \
    ppd = (struct tpacket3_hdr *) ((uint8_t *) ppd + ppd->tp_next_offset);                          \


#define packEnd                                                                                     \
    pbd->hdr.bh1.block_status = TP_STATUS_KERNEL;                                                   \
    blockNum = (blockNum + 1) % blocksMax;                                                          \



void doIfaceLoop(int * param) {
    int port = *param;
    unsigned char bufA[16384];
    unsigned char bufB[16384];
    unsigned char bufC[16384];
    unsigned char bufD[16384];
    const unsigned char *pack;
    int bufS;
    int blockNum = 0;
    struct tpacket_block_desc *pbd;
    struct tpacket3_hdr *ppd;
    EVP_CIPHER_CTX *encrCtx = EVP_CIPHER_CTX_new();
#ifndef HAVE_NOCRYPTO
    if (encrCtx == NULL) err("error getting encr context");
#endif
    EVP_MD_CTX *hashCtx = EVP_MD_CTX_new();
#ifndef HAVE_NOCRYPTO
    if (hashCtx == NULL) err("error getting hash context");
#endif
    if (port == cpuPort) {
        for (;;) {
            packBeg;
            for (int i=0; i<pkts; i++) {
                packOne;
                processCpuPack(&bufA[0], &bufB[0], &bufC[0], &bufD[0], bufS, encrCtx, hashCtx);
            }
            packEnd;
        }
    } else {
        for (;;) {
            packBeg;
            for (int i=0; i<pkts; i++) {
                packOne;
                processDataPacket(&bufA[0], &bufB[0], &bufC[0], &bufD[0], bufS, port, port, encrCtx, hashCtx);
            }
            packEnd;
        }
    }
    err("port thread exited");
}



void doSockLoop() {
    FILE *commands = fdopen(commandSock, "r");
    if (commands == NULL) err("failed to open file");
    EVP_CIPHER_CTX *encrCtx = EVP_CIPHER_CTX_new();
#ifndef HAVE_NOCRYPTO
    if (encrCtx == NULL) err("error getting encr context");
#endif
    EVP_MD_CTX *hashCtx = EVP_MD_CTX_new();
#ifndef HAVE_NOCRYPTO
    if (hashCtx == NULL) err("error getting hash context");
#endif
    unsigned char buf[16384];
    for (;;) {
        memset(&buf, 0, sizeof(buf));
        if (fgets((char*)&buf[0], sizeof(buf), commands) == NULL) break;
        if (doOneCommand(&buf[0], encrCtx, hashCtx) != 0) break;
    }
    err("command thread exited");
}



void doStatLoop() {
    FILE *commands = fdopen(commandSock, "w");
    if (commands == NULL) err("failed to open file");
    fprintf(commands, "platform %spcap\r\n", platformBase);
    fprintf(commands, "capabilities %s\r\n", getCapas());
    for (int i = 0; i < dataPorts; i++) fprintf(commands, "portname %i %s\r\n", i, ifaceName[i]);
    fprintf(commands, "cpuport %i\r\n", cpuPort);
    fprintf(commands, "dynrange %i 65535\r\n", maxPorts);
    fprintf(commands, "vrfrange 1 65535\r\n");
    fprintf(commands, "nomore\r\n");
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

    dataPorts = 0;
    for (int i = 4; i < argc; i++) {
        initIface(dataPorts, argv[i]);
        dataPorts++;
    }
    if (dataPorts < 2) err("using: dp <addr> <port> <cpuport> <ifc0> <ifc1> [ifcN]");
    if (dataPorts > maxPorts) dataPorts = maxPorts;
    if (initTables() != 0) err("error initializing tables");
    int port = atoi(argv[2]);
    struct sockaddr_in addr;
    memset(&addr, 0, sizeof (addr));
    if (inet_aton(argv[1], &addr.sin_addr) == 0) err("bad addr address");
    addr.sin_family = AF_INET;
    addr.sin_port = htons(port);
    printf("connecting %s %i.\n", inet_ntoa(addr.sin_addr), port);
    commandSock = socket(AF_INET, SOCK_STREAM, 0);
    if (commandSock < 0) err("unable to open socket");
    if (connect(commandSock, (struct sockaddr*)&addr, sizeof(addr)) < 0) err("failed to connect socket");
    cpuPort = atoi(argv[3]);
    printf("cpu port is #%i of %i...\n", cpuPort, dataPorts);

    for (int o = 0; o < dataPorts; o++) {
        printf("opening interface %s\n", ifaceName[o]);
        if ((ifaceSock[o] = socket(PF_PACKET, SOCK_RAW, htons(ETH_P_ALL))) < 0) err("unable to open socket");
        struct ifreq ifr;
        memset(&ifr, 0, sizeof (ifr));
        strcpy(ifr.ifr_name, ifaceName[o]);
        if (ioctl(ifaceSock[o], SIOCGIFINDEX, &ifr) < 0) {
          if (o < (dataPorts-1)) err("unable to get ifcidx");
          dataPorts--;
          break;
        }
        ifaceIndex[o] = ifr.ifr_ifindex;
        memset(&addrIfc[o], 0, sizeof (addrIfc[o]));
        addrIfc[o].sll_family = AF_PACKET;
        addrIfc[o].sll_ifindex = ifaceIndex[o];
        addrIfc[o].sll_protocol = htons(ETH_P_ALL);
        if (bind(ifaceSock[o], (struct sockaddr *) &addrIfc[o], sizeof (addrIfc[o])) < 0) err("failed to bind socket");
        addrIfc[o].sll_pkttype = PACKET_OUTGOING;
        struct packet_mreq pmr;
        memset(&pmr, 0, sizeof (pmr));
        pmr.mr_ifindex = ifaceIndex[o];
        pmr.mr_type = PACKET_MR_PROMISC;
        if (setsockopt(ifaceSock[o], SOL_PACKET, PACKET_ADD_MEMBERSHIP, &pmr, sizeof (pmr)) < 0) err("failed to set promisc");
        int ver = TPACKET_V3;
        if (setsockopt(ifaceSock[o], SOL_PACKET, PACKET_VERSION, &ver, sizeof (ver)) < 0) err("failed to set version");
        struct tpacket_req3 rrq;
        memset(&rrq, 0, sizeof (rrq));
        rrq.tp_block_size = 1 << 22;
        rrq.tp_frame_size = 16384;
        rrq.tp_block_nr = blocksMax;
        rrq.tp_frame_nr = (rrq.tp_block_size * rrq.tp_block_nr) / rrq.tp_frame_size;
        rrq.tp_retire_blk_tov = 1;
        if (setsockopt(ifaceSock[o], SOL_PACKET, PACKET_RX_RING, &rrq, sizeof (rrq)) < 0) err("failed enable ring buffer");
        ifaceMem[o] = mmap(NULL, (size_t)rrq.tp_block_size * rrq.tp_block_nr, PROT_READ | PROT_WRITE, MAP_SHARED, ifaceSock[o], 0);
        if (ifaceMem[o] == MAP_FAILED) err("failed to mmap ring buffer");
        ifaceIov[o] = malloc(rrq.tp_block_nr * sizeof (*ifaceIov[o]));
        if (ifaceIov[o] == NULL) err("failed to allocate iovec memory");
        for (int i = 0; i < rrq.tp_block_nr; i++) {
            ifaceIov[o][i].iov_base = ifaceMem[o] + (i * rrq.tp_block_size);
            ifaceIov[o][i].iov_len = rrq.tp_block_size;
        }
        memset(&ifacePfd[o], 0, sizeof (ifacePfd[o]));
        ifacePfd[o].fd = ifaceSock[o];
        ifacePfd[o].events = POLLIN | POLLERR;
        ifaceId[o] = o;
    }

    pthread_t threadSock;
    if (pthread_create(&threadSock, NULL, (void*) & doSockLoop, NULL)) err("error creating socket thread");
    pthread_t threadStat;
    if (pthread_create(&threadStat, NULL, (void*) & doStatLoop, NULL)) err("error creating status thread");

    for (int i=0; i < dataPorts; i++) {
        if (pthread_create(&threadRaw[i], NULL, (void*) & doIfaceLoop, &ifaceId[i])) err("error creating port thread");
    }

    doMainLoop();
}
