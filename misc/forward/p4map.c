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

pthread_mutex_t ifaceLock[maxPorts];
int ifaceIndex[maxPorts];
int ifaceSock[maxPorts];
unsigned char *ifaceMem[maxPorts];
struct iovec *ifaceRiv[maxPorts];
struct iovec *ifaceTiv[maxPorts];
struct pollfd ifacePfd[maxPorts];
struct sockaddr_ll addrIfc[maxPorts];
int blockNxt[maxPorts];

void sendPack(unsigned char *bufD, int bufS, int port) {
    pthread_mutex_lock(&ifaceLock[port]);
    struct tpacket2_hdr *ppd;
    ppd = (struct tpacket2_hdr *) ifaceTiv[port][blockNxt[port]].iov_base;
    if (ppd->tp_status != TP_STATUS_AVAILABLE) {
        pthread_mutex_unlock(&ifaceLock[port]);
        return;
    }
    memcpy(ifaceTiv[port][blockNxt[port]].iov_base + TPACKET_ALIGN(sizeof(struct tpacket2_hdr)), bufD, bufS);
    ppd->tp_len = bufS;
    ppd->tp_status = TP_STATUS_SEND_REQUEST;
    blockNxt[port] = (blockNxt[port] + 1) % blocksMax;
    pthread_mutex_unlock(&ifaceLock[port]);
    sendto(ifaceSock[port], NULL, 0, 0, (struct sockaddr *) &addrIfc[port], sizeof (addrIfc[port]));
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




int ifaceId[maxPorts];



void doIfaceLoop(int * param) {
    int port = *param;
    const unsigned char *pack;
    int bufS;
    int blockNum = 0;
    struct tpacket2_hdr *ppd;
    struct packetContext ctx;
    if (initContext(&ctx) != 0) err("error initializing context");
    unsigned char *bufD = ctx.bufD;
    ctx.stat = ifaceStat[port];
    for (;;) {
        ppd = (struct tpacket2_hdr *) ifaceRiv[port][blockNum].iov_base;
        if ((ppd->tp_status & TP_STATUS_USER) == 0) {
            poll(&ifacePfd[port], 1, 1);
            continue;
        }
        bufS = ppd->tp_snaplen;
        pack = (unsigned char *) ppd + ppd->tp_mac;
        if ((ppd->tp_status & TP_STATUS_VLAN_VALID) == 0) memcpy(&bufD[preBuff], pack, bufS);
        else {
            if ((ppd->tp_status & TP_STATUS_VLAN_TPID_VALID) == 0) ppd->tp_vlan_tpid = ETH_P_8021Q;
            memcpy(&bufD[preBuff], pack, 12);
            put16msb(bufD, preBuff + 12, ppd->tp_vlan_tpid);
            put16msb(bufD, preBuff + 14, ppd->tp_vlan_tci);
            memcpy(&bufD[preBuff + 16], pack + 12, bufS - 12);
            bufS += 4;
        }
        ppd->tp_status = TP_STATUS_KERNEL;
        blockNum = (blockNum + 1) % blocksMax;
        processDataPacket(&ctx, bufS, port);
    }
    err("port thread exited");
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
        int ver = TPACKET_V2;
        if (setsockopt(ifaceSock[o], SOL_PACKET, PACKET_VERSION, &ver, sizeof (ver)) < 0) err("failed to set version");
        struct tpacket_req3 rrq;
        memset(&rrq, 0, sizeof (rrq));
        rrq.tp_block_size = totBuff;
        rrq.tp_frame_size = totBuff;
        rrq.tp_block_nr = blocksMax;
        rrq.tp_frame_nr = (rrq.tp_block_size * rrq.tp_block_nr) / rrq.tp_frame_size;
        rrq.tp_retire_blk_tov = 1;
        if (setsockopt(ifaceSock[o], SOL_PACKET, PACKET_RX_RING, &rrq, sizeof (rrq)) < 0) err("failed enable rx ring buffer");
        if (setsockopt(ifaceSock[o], SOL_PACKET, PACKET_TX_RING, &rrq, sizeof (rrq)) < 0) err("failed enable tx ring buffer");
        ifaceMem[o] = mmap(NULL, (size_t)rrq.tp_block_size * rrq.tp_block_nr * 2, PROT_READ | PROT_WRITE, MAP_SHARED, ifaceSock[o], 0);
        if (ifaceMem[o] == MAP_FAILED) err("failed to mmap ring buffer");
        ifaceRiv[o] = malloc(rrq.tp_block_nr * sizeof (*ifaceRiv[o]));
        if (ifaceRiv[o] == NULL) err("failed to allocate rx iovec memory");
        ifaceTiv[o] = malloc(rrq.tp_block_nr * sizeof (*ifaceRiv[o]));
        if (ifaceTiv[o] == NULL) err("failed to allocate rx iovec memory");
        for (int i = 0; i < rrq.tp_block_nr; i++) {
            ifaceRiv[o][i].iov_base = ifaceMem[o] + (i * rrq.tp_block_size);
            ifaceRiv[o][i].iov_len = rrq.tp_block_size;
            ifaceTiv[o][i].iov_base = ifaceMem[o] + ((i + blocksMax) * rrq.tp_block_size);
            ifaceTiv[o][i].iov_len = rrq.tp_block_size;
        }
        blockNxt[o] = 0;
        memset(&ifacePfd[o], 0, sizeof (ifacePfd[o]));
        ifacePfd[o].fd = ifaceSock[o];
        ifacePfd[o].events = POLLIN | POLLERR;
        pthread_mutex_init(&ifaceLock[o], NULL);
        ifaceId[o] = o;
    }

    doNegotiate("map");
    pthread_t threadSock;
    if (pthread_create(&threadSock, NULL, (void*) & doSockLoop, NULL)) err("error creating socket thread");
    pthread_t threadStat;
    if (pthread_create(&threadStat, NULL, (void*) & doStatLoop, NULL)) err("error creating status thread");
    pthread_t threadRaw[maxPorts];
    for (int i=0; i < dataPorts; i++) {
        if (pthread_create(&threadRaw[i], NULL, (void*) & doIfaceLoop, &ifaceId[i])) err("error creating port thread");
    }

    doMainLoop();
}
