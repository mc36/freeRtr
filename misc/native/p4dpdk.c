#include <arpa/inet.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <unistd.h>
#include <pthread.h>
#include <inttypes.h>
#include <rte_config.h>
#include <rte_common.h>
#include <rte_malloc.h>
#include <rte_memory.h>
#include <rte_eal.h>
#include <rte_ethdev.h>
#include <rte_mbuf.h>
#include <rte_ring.h>


#undef basicLoop


#include "p4core.h"


struct rte_mempool *mbuf_pool;

struct rte_ring *tx_ring[RTE_MAX_ETHPORTS];

void sendpack(unsigned char *bufD, int bufS, int port) {
    struct rte_mbuf *mbuf = rte_pktmbuf_alloc(mbuf_pool);
    if (mbuf == NULL) rte_exit(EXIT_FAILURE, "error allocating bmuf\n");
    unsigned char * pack = rte_pktmbuf_append(mbuf, bufS);
    memmove(pack, bufD, bufS);
    rte_ring_mp_enqueue(tx_ring[port], mbuf);
}


#include "p4core.c"


int commandSock;


#define RX_RING_SIZE 1024
#define TX_RING_SIZE 1024

#define NUM_MBUFS 8191
#define MBUF_CACHE_SIZE 250
#define BURST_SIZE 32
#define RING_SIZE 512


static const struct rte_eth_conf port_conf_default = {
        .rxmode = {
                .max_rx_pkt_len = RTE_ETHER_MAX_LEN,
        },
        .txmode = {
        },
};



struct lcore_conf {
        int num_port;
        int port_list[RTE_MAX_ETHPORTS];
} __rte_cache_aligned;
struct lcore_conf lcore_conf[RTE_MAX_LCORE];







void doSockLoop() {
    FILE *commands = fdopen(commandSock, "rw");
    if (commands == NULL) rte_exit(EXIT_FAILURE, "failed to open file\n");
    unsigned char buf[1024];
    for (;;) {
        strcpy(buf, "quit\r\n");
        fgets(buf, sizeof(buf), commands);
        if (doOneCommand(&buf[0]) != 0) break;
    }
    rte_exit(EXIT_FAILURE, "command thread exited\n");
}



void doStatLoop() {
    FILE *commands = fdopen(commandSock, "w");
    if (commands == NULL) rte_exit(EXIT_FAILURE, "failed to open file\n");
    for (;;) {
        doStatRount(commands);
        sleep(1);
    }
    rte_exit(EXIT_FAILURE, "stat thread exited\n");
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
    rte_exit(EXIT_FAILURE, "main thread exited\n");
}




static int doPacketLoop(__rte_unused void *arg) {
    unsigned char bufD[16384];
    int bufS;
    struct rte_mbuf *bufs[BURST_SIZE];
    int port;
    int seq;
    int num;
    int pkts;
    int i;

    int lcore = rte_lcore_id();
    struct lcore_conf *myconf = &lcore_conf[lcore];

    printf("lcore %i started with %i ports!\n", lcore, myconf->num_port);
    if (myconf->num_port < 1) return 0;

    for (;;) {
        pkts = 0;
        for (seq = 0; seq < myconf->num_port; seq++) {
            port = myconf->port_list[seq];
            num = rte_ring_count(tx_ring[port]);
            if (num > BURST_SIZE) num = BURST_SIZE;
            num = rte_ring_sc_dequeue_bulk(tx_ring[port], (void**)bufs, num, NULL);
            rte_eth_tx_burst(port, 0, bufs, num);
            pkts += num;
            num = rte_eth_rx_burst(port, 0, bufs, BURST_SIZE);
            for (i = 0; i < num; i++) {
                bufS = rte_pktmbuf_pkt_len(bufs[i]);
                memmove(&bufD[preBuff], rte_pktmbuf_mtod(bufs[i], void *), bufS);
                rte_pktmbuf_free(bufs[i]);
                if (port == cpuport) processCpuPack(&bufD[0], bufS); else processDataPacket(&bufD[0], bufS, port);
            }
            pkts += num;
        }
        if (pkts < 1) usleep(100);
    }
    rte_exit(EXIT_FAILURE, "packet thread exited\n");
    return 0;
}







int main(int argc, char **argv) {

    int ret = rte_eal_init(argc, argv);
    if (ret < 0) rte_exit(EXIT_FAILURE, "error with eal initialization\n");

    argc -= ret;
    argv += ret;

    ports = rte_eth_dev_count_avail();
    if (ports < 2) rte_exit(EXIT_FAILURE, "at least 2 ports needed\n");

    if (argc < 4) rte_exit(EXIT_FAILURE, "using: dp <host> <rport> <cpuport> [port lcore] ...\n");
    initTables();
    int port = atoi(argv[2]);
    struct sockaddr_in addr;
    memset(&addr, 0, sizeof (addr));
    if (inet_aton(argv[1], &addr.sin_addr) == 0) rte_exit(EXIT_FAILURE, "bad addr address\n");
    addr.sin_family = AF_INET;
    addr.sin_port = htons(port);
    printf("connecting %s %i.\n", inet_ntoa(addr.sin_addr), port);
    commandSock = socket(AF_INET, SOCK_STREAM, 0);
    if (commandSock < 0) rte_exit(EXIT_FAILURE, "unable to open socket\n");
    if(connect(commandSock, (struct sockaddr*)&addr, sizeof(addr)) < 0) rte_exit(EXIT_FAILURE, "failed to connect socket\n");
    cpuport = atoi(argv[3]);
    printf("cpu port is #%i of %i...\n", cpuport, ports);

    int port2lcore[maxPorts];
    memset(&port2lcore, 0, sizeof(port2lcore));
    for (int i = 4; i< argc; i += 2) {
        int p = atoi(argv[i + 0]);
        int c = atoi(argv[i + 1]);
        port2lcore[p] = c;
    }
    memset(&lcore_conf, 0, sizeof(lcore_conf));
    for (int i = 0; i < ports; i++) {
        int c = port2lcore[i];
        lcore_conf[c].port_list[lcore_conf[c].num_port] = i;
        lcore_conf[c].num_port++;
    }

    mbuf_pool = rte_pktmbuf_pool_create("mbufs", NUM_MBUFS * ports, MBUF_CACHE_SIZE, 0, RTE_MBUF_DEFAULT_BUF_SIZE, rte_socket_id());
    if (mbuf_pool == NULL) rte_exit(EXIT_FAILURE, "cannot create mbuf pool\n");

    RTE_ETH_FOREACH_DEV(port) {
        unsigned char buf[128];
        sprintf(buf, "dpdk-port%i", port);
        int sock = rte_eth_dev_socket_id(port);
        printf("opening port %i on lcore %i on socket %i...\n", port, port2lcore[port], sock);
        initIface(port, buf);

        struct rte_eth_conf port_conf = port_conf_default;
        uint16_t nb_rxd = RX_RING_SIZE;
        uint16_t nb_txd = TX_RING_SIZE;
        int retval;
        struct rte_eth_dev_info dev_info;
        struct rte_eth_txconf txconf;

        if (!rte_eth_dev_is_valid_port(port)) rte_exit(EXIT_FAILURE, "not valid port\n");

        retval = rte_eth_dev_info_get(port, &dev_info);
        if (retval != 0) rte_exit(EXIT_FAILURE, "error getting device info\n");

        if (dev_info.tx_offload_capa & DEV_TX_OFFLOAD_MBUF_FAST_FREE) {
            port_conf.txmode.offloads |= DEV_TX_OFFLOAD_MBUF_FAST_FREE;
        }

        if (dev_info.rx_offload_capa & DEV_RX_OFFLOAD_JUMBO_FRAME) {
            port_conf.rxmode.offloads |= DEV_RX_OFFLOAD_JUMBO_FRAME;
            port_conf.rxmode.max_rx_pkt_len = RTE_MBUF_DEFAULT_DATAROOM;
        }

        retval = rte_eth_dev_configure(port, 1, 1, &port_conf);
        if (retval != 0) rte_exit(EXIT_FAILURE, "error configuring port\n");

        retval = rte_eth_dev_adjust_nb_rx_tx_desc(port, &nb_rxd, &nb_txd);
        if (retval != 0) rte_exit(EXIT_FAILURE, "error adjusting descriptors\n");

        retval = rte_eth_rx_queue_setup(port, 0, nb_rxd, sock, NULL, mbuf_pool);
        if (retval < 0) rte_exit(EXIT_FAILURE, "error setting up rx queue\n");

        txconf = dev_info.default_txconf;
        txconf.offloads = port_conf.txmode.offloads;
        retval = rte_eth_tx_queue_setup(port, 0, nb_txd, sock, &txconf);
        if (retval < 0) rte_exit(EXIT_FAILURE, "error setting up tx queue\n");

        tx_ring[port] = rte_ring_create(buf, RING_SIZE, sock, RING_F_SP_ENQ | RING_F_SC_DEQ);
        if (tx_ring[port] == NULL) rte_exit(EXIT_FAILURE, "error allocating tx ring\n");

        retval = rte_eth_dev_start(port);
        if (retval < 0) rte_exit(EXIT_FAILURE, "error starting port\n");

        struct rte_ether_addr addr;
        retval = rte_eth_macaddr_get(port, &addr);
        if (retval != 0) rte_exit(EXIT_FAILURE, "error getting mac\n");

        retval = rte_eth_promiscuous_enable(port);
        if (retval != 0) rte_exit(EXIT_FAILURE, "error setting promiscuous mode\n");
    }

    pthread_t threadSock;
    if (pthread_create(&threadSock, NULL, (void*) & doSockLoop, NULL)) rte_exit(EXIT_FAILURE, "error creating socket thread\n");
    pthread_t threadStat;
    if (pthread_create(&threadStat, NULL, (void*) & doStatLoop, NULL)) rte_exit(EXIT_FAILURE, "error creating status thread\n");

    rte_eal_mp_remote_launch(&doPacketLoop, NULL, CALL_MASTER);

    doMainLoop();

}
