#include <arpa/inet.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <unistd.h>
#include <pthread.h>
#include <inttypes.h>
#include <rte_eal.h>
#include <rte_ethdev.h>
#include <rte_mbuf.h>


#undef basicLoop


#include "p4core.h"


struct rte_mempool *mbuf_pool;

void sendpack(unsigned char *bufD, int bufS, int port) {
    struct rte_mbuf *buf = rte_pktmbuf_alloc(mbuf_pool);
    unsigned char * pack = rte_pktmbuf_append(buf, bufS);
    memmove(pack, bufD, bufS);
    rte_eth_tx_burst(port, 0, &buf, 1);
}


#include "p4core.c"


int commandSock;



#define RX_RING_SIZE 1024
#define TX_RING_SIZE 1024

#define NUM_MBUFS 8191
#define MBUF_CACHE_SIZE 250
#define BURST_SIZE 32

static const struct rte_eth_conf port_conf_default = {
        .rxmode = {
                .max_rx_pkt_len = RTE_ETHER_MAX_LEN,
        },
        .txmode = {
        },
};







void doSockLoop() {
    FILE *commands = fdopen(commandSock, "rw");
    if (commands == NULL) rte_exit(EXIT_FAILURE, "failed to open file");
    unsigned char buf[1024];
    for (;;) {
        strcpy(buf, "quit\r\n");
        fgets(buf, sizeof(buf), commands);
        if (doOneCommand(&buf[0]) != 0) break;
    }
    rte_exit(EXIT_FAILURE, "command thread exited");
}



void doStatLoop() {
    FILE *commands = fdopen(commandSock, "w");
    if (commands == NULL) rte_exit(EXIT_FAILURE, "failed to open file");
    for (;;) {
        doStatRount(commands);
        sleep(1);
    }
    rte_exit(EXIT_FAILURE, "stat thread exited");
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
    rte_exit(EXIT_FAILURE, "main thread exited");
}



void doPacketLoop() {
    unsigned char bufD[16384];
    int bufS;
    struct rte_mbuf *bufs[BURST_SIZE];
    uint16_t portid;
    uint16_t nb_rx;
    int pkts;
    int i;

    for (;;) {
        pkts = 0;
        RTE_ETH_FOREACH_DEV(portid) {
            nb_rx = rte_eth_rx_burst(portid, 0, bufs, BURST_SIZE);
            pkts += nb_rx;
            for (i = 0; i < nb_rx; i++) {
                bufS = rte_pktmbuf_pkt_len(bufs[i]);
                memmove(&bufD[preBuff], rte_pktmbuf_mtod(bufs[i], void *), bufS);
                rte_pktmbuf_free(bufs[i]);
                if (portid == cpuport) processCpuPack(&bufD[0], bufS); else processDataPacket(&bufD[0], bufS, portid);
            }
        }
        if (pkts < 1) usleep(100);
    }
    rte_exit(EXIT_FAILURE, "packet thread exited");
}







int main(int argc, char **argv) {

        int ret = rte_eal_init(argc, argv);
        if (ret < 0) rte_exit(EXIT_FAILURE, "error with eal initialization\n");

        argc -= ret;
        argv += ret;

        ports = rte_eth_dev_count_avail();
        if (ports < 2) rte_exit(EXIT_FAILURE, "at least 2 ports needed\n");

        if (argc < 4) rte_exit(EXIT_FAILURE, "using: dp <host> <rport> <cpuport>\n");
        initTables();
        int port = atoi(argv[2]);
        struct sockaddr_in addr;
        memset(&addr, 0, sizeof (addr));
        if (inet_aton(argv[1], &addr.sin_addr) == 0) rte_exit(EXIT_FAILURE, "bad addr address");
        addr.sin_family = AF_INET;
        addr.sin_port = htons(port);
        printf("connecting %s %i.\n", inet_ntoa(addr.sin_addr), port);
        commandSock = socket(AF_INET, SOCK_STREAM, 0);
        if (commandSock < 0) rte_exit(EXIT_FAILURE, "unable to open socket");
        if(connect(commandSock, (struct sockaddr*)&addr, sizeof(addr)) < 0) rte_exit(EXIT_FAILURE, "failed to connect socket");
        cpuport = atoi(argv[3]);
        printf("cpu port is #%i of %i...\n", cpuport, ports);


        mbuf_pool = rte_pktmbuf_pool_create("MBUF_POOL", NUM_MBUFS * ports, MBUF_CACHE_SIZE, 0, RTE_MBUF_DEFAULT_BUF_SIZE, rte_socket_id());
        if (mbuf_pool == NULL) rte_exit(EXIT_FAILURE, "cannot create mbuf pool\n");

        uint16_t portid;
        RTE_ETH_FOREACH_DEV(portid) {
            port = portid;
            unsigned char buf[128];
            sprintf(buf, "dpdk-port%u", portid);
            printf("opening port %u...\n", portid);
            initIface(portid, buf);

            struct rte_eth_conf port_conf = port_conf_default;
            const uint16_t rx_rings = 1, tx_rings = 1;
            uint16_t nb_rxd = RX_RING_SIZE;
            uint16_t nb_txd = TX_RING_SIZE;
            int retval;
            uint16_t q;
            struct rte_eth_dev_info dev_info;
            struct rte_eth_txconf txconf;

            if (!rte_eth_dev_is_valid_port(port)) rte_exit(EXIT_FAILURE, "not valid port");

            retval = rte_eth_dev_info_get(port, &dev_info);
            if (retval != 0) rte_exit(EXIT_FAILURE, "error getting device info\n");

            if (dev_info.tx_offload_capa & DEV_TX_OFFLOAD_MBUF_FAST_FREE) {
                port_conf.txmode.offloads |= DEV_TX_OFFLOAD_MBUF_FAST_FREE;
            }

            if (dev_info.rx_offload_capa & DEV_RX_OFFLOAD_JUMBO_FRAME) {
                port_conf.rxmode.offloads |= DEV_RX_OFFLOAD_JUMBO_FRAME;
                port_conf.rxmode.max_rx_pkt_len = RTE_MBUF_DEFAULT_DATAROOM;
            }

            retval = rte_eth_dev_configure(port, rx_rings, tx_rings, &port_conf);
            if (retval != 0) rte_exit(EXIT_FAILURE, "error configuring port\n");

            retval = rte_eth_dev_adjust_nb_rx_tx_desc(port, &nb_rxd, &nb_txd);
            if (retval != 0) rte_exit(EXIT_FAILURE, "error adjusting descriptors\n");

            for (q = 0; q < rx_rings; q++) {
                retval = rte_eth_rx_queue_setup(port, q, nb_rxd, rte_eth_dev_socket_id(port), NULL, mbuf_pool);
                if (retval < 0) rte_exit(EXIT_FAILURE, "error setting up rx queue\n");
            }

            txconf = dev_info.default_txconf;
            txconf.offloads = port_conf.txmode.offloads;
            for (q = 0; q < tx_rings; q++) {
                retval = rte_eth_tx_queue_setup(port, q, nb_txd, rte_eth_dev_socket_id(port), &txconf);
                if (retval < 0) rte_exit(EXIT_FAILURE, "error setting up tx queue\n");
            }

            retval = rte_eth_dev_start(port);
            if (retval < 0) rte_exit(EXIT_FAILURE, "error starting port\n");

            struct rte_ether_addr addr;
            retval = rte_eth_macaddr_get(port, &addr);
            if (retval != 0) rte_exit(EXIT_FAILURE, "error getting mac\n");

            retval = rte_eth_promiscuous_enable(port);
            if (retval != 0) rte_exit(EXIT_FAILURE, "error setting promiscuous mode\n");
        }

    pthread_t threadSock;
    if (pthread_create(&threadSock, NULL, (void*) & doSockLoop, NULL)) rte_exit(EXIT_FAILURE, "error creating socket thread");
    pthread_t threadStat;
    if (pthread_create(&threadStat, NULL, (void*) & doStatLoop, NULL)) rte_exit(EXIT_FAILURE, "error creating status thread");
    pthread_t threadPackk;
    if (pthread_create(&threadPackk, NULL, (void*) & doPacketLoop, NULL)) rte_exit(EXIT_FAILURE, "error creating packet thread");


    doMainLoop();

}
