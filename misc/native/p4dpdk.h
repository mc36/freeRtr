#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <pthread.h>
#include <arpa/inet.h>
#include <openssl/conf.h>
//#include <openssl/provider.h>
#include <openssl/evp.h>
#include <openssl/rand.h>
#include <openssl/err.h>
#include <dpdk/rte_config.h>
#include <dpdk/rte_common.h>
#include <dpdk/rte_version.h>
#include <dpdk/rte_eal.h>
#include <dpdk/rte_ethdev.h>
#include <dpdk/rte_mbuf.h>
#include <dpdk/rte_ring.h>



#include "p4cns.h"


struct rte_mempool *mbuf_pool;

struct rte_ring *tx_ring[RTE_MAX_ETHPORTS];

void sendPack(unsigned char *bufD, int bufS, int port) {
    struct rte_mbuf *mbuf = rte_pktmbuf_alloc(mbuf_pool);
    if (mbuf == NULL) return;
    char * pack = rte_pktmbuf_append(mbuf, bufS);
    if (pack == NULL) return;
    memcpy(pack, bufD, bufS);
    if (rte_ring_mp_enqueue(tx_ring[port], mbuf) == 0) return;
    rte_pktmbuf_free(mbuf);
}


void setMtu(int port, int mtu) {
    rte_eth_dev_set_mtu(port, mtu);
}


void setState(int port, int sta) {
    if (sta == 1) rte_eth_dev_set_link_up(port);
    else rte_eth_dev_set_link_down(port);
}


int getState(int port) {
    struct rte_eth_link link;
    if (rte_eth_link_get_nowait(port, &link) != 0) return 1;
    if (link.link_status == ETH_LINK_UP) return 1;
    return 0;
}


void err(char*buf) {
    rte_exit(EXIT_FAILURE, "%s\n", buf);
    exit(1);
}




#include "p4tab.h"
#include "p4msg.h"
#include "p4fwd.h"


int commandSock;


#define RX_RING_SIZE 1024
#define TX_RING_SIZE 1024

#define NUM_MBUFS 8191
#define MBUF_CACHE_SIZE 250
#define BURST_SIZE 32
#define BURST_PAUSE 100
#define RING_SIZE 512




struct lcore_conf {
    int rx_num;
    int rx_list[RTE_MAX_ETHPORTS];
    int tx_num;
    int tx_list[RTE_MAX_ETHPORTS];
    int justProcessor;
} __rte_cache_aligned;
struct lcore_conf lcore_conf[RTE_MAX_LCORE];
struct rte_ring *lcore_ring[RTE_MAX_LCORE];
int lcore_procs;








void doSockLoop() {
    FILE *commands = fdopen(commandSock, "r");
    if (commands == NULL) err("failed to open file");
    unsigned char buf[1024];
    for (;;) {
        memset(&buf, 0, sizeof(buf));
        if (fgets((char*)&buf[0], sizeof(buf), commands) == NULL) break;
        if (doOneCommand(&buf[0]) != 0) break;
    }
    err("command thread exited");
}



void doStatLoop() {
    FILE *commands = fdopen(commandSock, "w");
    if (commands == NULL) err("failed to open file");
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
        int i = scanf("%s", buf);
        if (i < 1) {
            sleep(1);
            continue;
        }
        if (doConsoleCommand(&buf[0]) != 0) break;
        printf("\n");
    }
    err("main thread exited");
}


#define mbuf2mybuf(mbuf)                                        \
    bufS = rte_pktmbuf_pkt_len(mbuf);                           \
    bufP = rte_pktmbuf_mtod(mbuf, void *);                      \
    if ((mbuf->ol_flags & PKT_RX_VLAN_STRIPPED) != 0) {         \
        memcpy(&bufD[preBuff], bufP, 12);                       \
        put16msb(bufD, preBuff + 12, ETHERTYPE_VLAN);           \
        put16msb(bufD, preBuff + 14, mbuf->vlan_tci);           \
        memcpy(&bufD[preBuff + 16], bufP + 12, bufS - 12);      \
        bufS += 4;                                              \
    } else {                                                    \
        memcpy(&bufD[preBuff], bufP, bufS);                     \
    }                                                           \
    rte_pktmbuf_free(mbuf);




static int doPacketLoop(__rte_unused void *arg) {
    unsigned char bufA[16384];
    unsigned char bufB[16384];
    unsigned char bufC[16384];
    unsigned char bufD[16384];
    unsigned char * bufP;
    int bufS;
    int port;
    int seq;
    int num;
    int i;
    struct rte_mbuf *mbufs[BURST_SIZE];
    EVP_CIPHER_CTX *encrCtx = EVP_CIPHER_CTX_new();
    if (encrCtx == NULL) err("error getting encr context");
    EVP_MD_CTX *hashCtx = EVP_MD_CTX_new();
    if (hashCtx == NULL) err("error getting hash context");

    int lcore = rte_lcore_id();
    struct lcore_conf *myconf = &lcore_conf[lcore];

    printf("lcore %i started with %i rx and %i tx ports and %i processing!\n", lcore, myconf->rx_num, myconf->tx_num, myconf->justProcessor);
    if ((myconf->rx_num + myconf->tx_num + myconf->justProcessor) < 1) return 0;

    if (lcore_procs < 1) {
        int pkts;
        for (;;) {
            pkts = 0;
            for (seq = 0; seq < myconf->tx_num; seq++) {
                port = myconf->tx_list[seq];
                num = rte_ring_count(tx_ring[port]);
                if (num > BURST_SIZE) num = BURST_SIZE;
                num = rte_ring_sc_dequeue_bulk(tx_ring[port], (void**)mbufs, num, NULL);
                rte_eth_tx_burst(port, 0, mbufs, num);
                pkts += num;
            }
            for (seq = 0; seq < myconf->rx_num; seq++) {
                port = myconf->rx_list[seq];
                num = rte_eth_rx_burst(port, 0, mbufs, BURST_SIZE);
                pkts += num;
                if (port == cpuport) {
                    for (i = 0; i < num; i++) {
                        mbuf2mybuf(mbufs[i]);
                        processCpuPack(&bufA[0], &bufB[0], &bufC[0], &bufD[0], bufS, encrCtx, hashCtx);
                    }
                    continue;
                }
                for (i = 0; i < num; i++) {
                    mbuf2mybuf(mbufs[i]);
                    processDataPacket(&bufA[0], &bufB[0], &bufC[0], &bufD[0], bufS, port, port, encrCtx, hashCtx);
                }
            }
            if (pkts < 1) usleep(BURST_PAUSE);
        }
    } else if (myconf->justProcessor > 0) {
        seq = myconf->justProcessor - 1;
        for (;;) {
            if (rte_ring_sc_dequeue(lcore_ring[seq], (void**)mbufs) != 0) continue;
            port = mbufs[0]->port;
            mbuf2mybuf(mbufs[0]);
            processDataPacket(&bufA[0], &bufB[0], &bufC[0], &bufD[0], bufS, port, port, encrCtx, hashCtx);
        }
    } else {
        for (;;) {
            for (seq = 0; seq < myconf->tx_num; seq++) {
                port = myconf->tx_list[seq];
                num = rte_ring_count(tx_ring[port]);
                if (num > BURST_SIZE) num = BURST_SIZE;
                num = rte_ring_sc_dequeue_bulk(tx_ring[port], (void**)mbufs, num, NULL);
                rte_eth_tx_burst(port, 0, mbufs, num);
            }
            for (seq = 0; seq < myconf->rx_num; seq++) {
                port = myconf->rx_list[seq];
                num = rte_eth_rx_burst(port, 0, mbufs, BURST_SIZE);
                if (port == cpuport) {
                    for (i = 0; i < num; i++) {
                        mbuf2mybuf(mbufs[i]);
                        processCpuPack(&bufA[0], &bufB[0], &bufC[0], &bufD[0], bufS, encrCtx, hashCtx);
                    }
                    continue;
                }
                for (i = 0; i < num; i++) {
                    bufP = rte_pktmbuf_mtod(mbufs[i], void *);
                    port = hashDataPacket(bufP) % lcore_procs;
                    if (rte_ring_mp_enqueue(lcore_ring[port], mbufs[i]) != 0) rte_pktmbuf_free(mbufs[i]);
                }
            }
        }
    }
    err("packet thread exited");
    return 0;
}







int main(int argc, char **argv) {

    int ret = rte_eal_init(argc, argv);
    if (ret < 0) err("error with eal initialization");

    argc -= ret;
    argv += ret;

    ports = rte_eth_dev_count_avail();
    if (ports < 2) err("at least 2 ports needed");

    if (argc < 4) err("using: dp [dpdk options] -- <host> <rport> <cpuport> [port rxcore txcore] [-1 fwdcore fwdcore]...");
    printf("dpdk version: %s\n", rte_version());
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
    if(connect(commandSock, (struct sockaddr*)&addr, sizeof(addr)) < 0) err("failed to connect socket");
    cpuport = atoi(argv[3]);
    printf("cpu port is #%i of %i...\n", cpuport, ports);

    int port2rx[RTE_MAX_ETHPORTS];
    int port2tx[RTE_MAX_ETHPORTS];
    memset(&port2rx, 0, sizeof(port2rx));
    memset(&port2tx, 0, sizeof(port2tx));
    memset(&lcore_conf, 0, sizeof(lcore_conf));
    for (int i = 4; i< argc; i += 3) {
        int p = atoi(argv[i + 0]);
        int r = atoi(argv[i + 1]);
        int t = atoi(argv[i + 2]);
        if (p < 0) {
            lcore_conf[r].justProcessor++;
            lcore_conf[t].justProcessor++;
            continue;
        }
        if (p > ports) continue;
        port2rx[p] = r;
        port2tx[p] = t;
    }
    lcore_procs = 0;
    for (int i = 0; i < RTE_MAX_LCORE; i++) {
        if (lcore_conf[i].justProcessor < 1) continue;
        lcore_procs++;
        lcore_conf[i].justProcessor = lcore_procs;
    }
    for (int i = 0; i < ports; i++) {
        int r = port2rx[i];
        int t = port2tx[i];
        lcore_conf[r].rx_list[lcore_conf[r].rx_num] = i;
        lcore_conf[r].rx_num++;
        lcore_conf[t].tx_list[lcore_conf[t].tx_num] = i;
        lcore_conf[t].tx_num++;
    }

    mbuf_pool = rte_pktmbuf_pool_create("mbufs", NUM_MBUFS * ports, MBUF_CACHE_SIZE, 0, RTE_MBUF_DEFAULT_BUF_SIZE, rte_socket_id());
    if (mbuf_pool == NULL) err("cannot create mbuf pool");

    for (int i = 0; i < RTE_MAX_LCORE; i++) {
        if (lcore_conf[i].justProcessor < 1) continue;
        int o = lcore_conf[i].justProcessor - 1;
        int sock = rte_lcore_to_socket_id(i);
        printf("opening forwarder %i on lcore %i on socket %i...\n", o, i, sock);
        unsigned char buf[128];
        sprintf((char*)&buf[0], "dpdk-pack%i", i);
        lcore_ring[o] = rte_ring_create((char*)&buf[0], RING_SIZE, sock, RING_F_SP_ENQ | RING_F_SC_DEQ);
        if (lcore_ring[o] == NULL) err("error allocating pack ring");
    }

    RTE_ETH_FOREACH_DEV(port) {
        unsigned char buf[128];
        sprintf((char*)&buf[0], "dpdk-port%i", port);
        int sock = rte_eth_dev_socket_id(port);
        printf("opening port %i on lcore (rx %i tx %i) on socket %i...\n", port, port2rx[port], port2tx[port], sock);
        initIface(port, (char*)&buf[0]);

        struct rte_eth_conf port_conf;
        memset(&port_conf, 0, sizeof(port_conf));
        uint16_t nb_rxd = RX_RING_SIZE;
        uint16_t nb_txd = TX_RING_SIZE;
        struct rte_eth_dev_info dev_info;
        memset(&dev_info, 0, sizeof(dev_info));
        struct rte_eth_txconf txconf;
        memset(&txconf, 0, sizeof(txconf));
        struct rte_ether_addr macaddr;
        memset(&macaddr, 0, sizeof(macaddr));

        if (!rte_eth_dev_is_valid_port(port)) err("not valid port");

        ret = rte_eth_dev_info_get(port, &dev_info);
        if (ret != 0) err("error getting device info");

        if (dev_info.tx_offload_capa & DEV_TX_OFFLOAD_MBUF_FAST_FREE) {
            port_conf.txmode.offloads |= DEV_TX_OFFLOAD_MBUF_FAST_FREE;
        }

        if (dev_info.rx_offload_capa & DEV_RX_OFFLOAD_JUMBO_FRAME) {
            port_conf.rxmode.offloads |= DEV_RX_OFFLOAD_JUMBO_FRAME;
            port_conf.rxmode.max_rx_pkt_len = RTE_MBUF_DEFAULT_DATAROOM;
        } else {
            port_conf.rxmode.max_rx_pkt_len = RTE_ETHER_MAX_LEN;
        }

        ret = rte_eth_dev_configure(port, 1, 1, &port_conf);
        if (ret != 0) err("error configuring port");

        ret = rte_eth_dev_adjust_nb_rx_tx_desc(port, &nb_rxd, &nb_txd);
        if (ret != 0) err("error adjusting descriptors");

        ret = rte_eth_rx_queue_setup(port, 0, nb_rxd, sock, NULL, mbuf_pool);
        if (ret != 0) err("error setting up rx queue");

        txconf = dev_info.default_txconf;
        txconf.offloads = port_conf.txmode.offloads;
        ret = rte_eth_tx_queue_setup(port, 0, nb_txd, sock, &txconf);
        if (ret != 0) err("error setting up tx queue");

        tx_ring[port] = rte_ring_create((char*)&buf[0], RING_SIZE, sock, RING_F_SP_ENQ | RING_F_SC_DEQ);
        if (tx_ring[port] == NULL) err("error allocating tx ring");

        ret = rte_eth_dev_start(port);
        if (ret != 0) err("error starting port");

        ret = rte_eth_macaddr_get(port, &macaddr);
        if (ret != 0) err("error getting mac");

        ret = rte_eth_promiscuous_enable(port);
        if (ret != 0) err("error setting promiscuous mode");
    }

    pthread_t threadSock;
    if (pthread_create(&threadSock, NULL, (void*) & doSockLoop, NULL)) err("error creating socket thread");
    pthread_t threadStat;
    if (pthread_create(&threadStat, NULL, (void*) & doStatLoop, NULL)) err("error creating status thread");

    rte_eal_mp_remote_launch(&doPacketLoop, NULL, CALL_MAIN);

    doMainLoop();

}
