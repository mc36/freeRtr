#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <pthread.h>
#include <arpa/inet.h>
#include <net/if.h>
#include <linux/bpf.h>
#include <linux/if_link.h>
#include <bpf/bpf.h>
#include <bpf/libbpf.h>
#include "p4xdp_tab.h"


void err(char*buf) {
    printf("%s\n", buf);
    exit(1);
}

void warn(char*buf) {
    printf("warning: %s\n", buf);
}


int commandSock;
int ifaces[maxPorts];
int ports;
int prog_fd;
int cpu_port_fd;
int rx_ports_fd;
int tx_ports_fd;
int vrf_port_fd;
int neighs_fd;
int route4_fd;
int route6_fd;
int labels_fd;
int bundles_fd;
int vlan_in_fd;
int vlan_out_fd;
int pppoes_fd;
int bridges_fd;
int cpuport;


#include "p4xdp_msg.h"


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
    fprintf(commands, "platform p4xdp\r\n");
    fprintf(commands, "capabilities route mpls bundle vlan pppoe eompls bridge vpls evpn\r\n");
    for (int i = 0; i < ports; i++) fprintf(commands, "portname %i xdp-port%i\r\n", i, ifaces[i]);
    fprintf(commands, "cpuport %i\r\n", cpuport);
    fprintf(commands, "dynrange 32768 65535\r\n");
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
        int i = scanf("%1024s", buf);
        if (i < 1) {
            sleep(1);
            continue;
        }
        switch (buf[0]) {
        case 0:
            break;
        case 'H':
        case 'h':
        case '?':
            printf("commands:\n");
            printf("h - this help\n");
            printf("x - exit process\n");
            printf("\n");
            break;
        case 'x':
        case 'X':
            err("exit requested");
            break;
        }
    }
    err("main thread exited");
}




int main(int argc, char **argv) {
    ports = 0;
    for (int i = 4; i < argc; i++) {
        printf("opening %s...", argv[i]);
        ifaces[ports] = if_nametoindex(argv[i]);
        if (ifaces[ports] == 0) err("error getting interface index");
        printf(" idx=%i\n", ifaces[ports]);
        ports++;
    }
    if (ports < 2) err("using: dp <addr> <port> <cpuport> <ifc0> <ifc1> [ifcN] ...");
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

    strcpy(argv[0] + strlen(argv[0]) - 8, "kern.bin");
    printf("loading %s...\n", argv[0]);
    struct bpf_prog_load_attr prog_load_attr = {
        .prog_type = BPF_PROG_TYPE_XDP,
        .file = argv[0],
    };
    struct bpf_object *bpf_obj;
    if (bpf_prog_load_xattr(&prog_load_attr, &bpf_obj, &prog_fd)) err("error loading code");

    cpu_port_fd = bpf_object__find_map_fd_by_name(bpf_obj, "cpu_port");
    if (cpu_port_fd < 0) err("error finding table");
    rx_ports_fd = bpf_object__find_map_fd_by_name(bpf_obj, "rx_ports");
    if (rx_ports_fd < 0) err("error finding table");
    tx_ports_fd = bpf_object__find_map_fd_by_name(bpf_obj, "tx_ports");
    if (tx_ports_fd < 0) err("error finding table");
    vrf_port_fd = bpf_object__find_map_fd_by_name(bpf_obj, "vrf_port");
    if (vrf_port_fd < 0) err("error finding table");
    neighs_fd = bpf_object__find_map_fd_by_name(bpf_obj, "neighs");
    if (neighs_fd < 0) err("error finding table");
    route4_fd = bpf_object__find_map_fd_by_name(bpf_obj, "routes4");
    if (vrf_port_fd < 0) err("error finding table");
    route6_fd = bpf_object__find_map_fd_by_name(bpf_obj, "routes6");
    if (route6_fd < 0) err("error finding table");
    labels_fd = bpf_object__find_map_fd_by_name(bpf_obj, "labels");
    if (labels_fd < 0) err("error finding table");
    bundles_fd = bpf_object__find_map_fd_by_name(bpf_obj, "bundles");
    if (bundles_fd < 0) err("error finding table");
    vlan_in_fd = bpf_object__find_map_fd_by_name(bpf_obj, "vlan_in");
    if (vlan_in_fd < 0) err("error finding table");
    vlan_out_fd = bpf_object__find_map_fd_by_name(bpf_obj, "vlan_out");
    if (vlan_out_fd < 0) err("error finding table");
    pppoes_fd = bpf_object__find_map_fd_by_name(bpf_obj, "pppoes");
    if (pppoes_fd < 0) err("error finding table");
    bridges_fd = bpf_object__find_map_fd_by_name(bpf_obj, "bridges");
    if (bridges_fd < 0) err("error finding table");

    for (int i = 0; i < ports; i++) {
        printf("opening index %i...\n", ifaces[i]);
        bpf_set_link_xdp_fd(ifaces[i], -1, XDP_FLAGS_DRV_MODE);
        bpf_set_link_xdp_fd(ifaces[i], -1, XDP_FLAGS_SKB_MODE);
        if (bpf_set_link_xdp_fd(ifaces[i], prog_fd, XDP_FLAGS_DRV_MODE) < 0) err("error attaching code");
    }

    int o = 0;
    int p = ifaces[cpuport];
    if (bpf_map_update_elem(cpu_port_fd, &o, &p, BPF_ANY) != 0) err("error setting cpuport");
    for (int i = 0; i < ports; i++) {
        printf("initializing index %i...\n", ifaces[i]);
        struct port_res ntry;
        memset(&ntry, 0, sizeof(ntry));
        ntry.idx = ifaces[i];
        if (bpf_map_update_elem(tx_ports_fd, &i, &ntry, BPF_ANY) != 0) err("error setting txport");
        o = ifaces[i];
        ntry.idx = i;
        if (bpf_map_update_elem(rx_ports_fd, &o, &ntry, BPF_ANY) != 0) err("error setting rxport");
    }

    pthread_t threadSock;
    if (pthread_create(&threadSock, NULL, (void*) & doSockLoop, NULL)) err("error creating socket thread");
    pthread_t threadStat;
    if (pthread_create(&threadStat, NULL, (void*) & doStatLoop, NULL)) err("error creating status thread");

    doMainLoop();
}
