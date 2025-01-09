#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <pthread.h>
#include <arpa/inet.h>
#include <linux/version.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <net/if.h>
#include <linux/bpf.h>
#include <linux/if_link.h>
#include <bpf/bpf.h>
#include <bpf/libbpf.h>
#include "types.h"
#include "p4xdp_tab.h"

// hack
#ifndef __LIBBPF_CURRENT_VERSION_GEQ
#define __LIBBPF_CURRENT_VERSION_GEQ(major, minor) 0
#endif


void err(char*buf) {
    printf("%s\n", buf);
    _exit(1);
}

void warn(char*buf) {
    printf("warning: %s\n", buf);
}


int commandSock;
int ifaceId[maxPorts];
char *ifaceName[maxPorts];
int dataPorts;
int cpu_port_fd;
int rx_ports_fd;
int tx_ports_fd;
int vrf_port_fd;
int neighs_fd;
int route4_fd;
int route6_fd;
int tunnel4_fd;
int tunnel6_fd;
int labels_fd;
int bundles_fd;
int vlan_in_fd;
int vlan_out_fd;
int pppoes_fd;
int polpol_fd;
int polidx_fd;
int nsh_fd;
int bridges_fd;
int cpuPort;


#include "p4xdp_msg.h"


void doSockLoop() {
    printCmds = getenv("p4emuNOCMDS") == NULL;
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
    fprintf(commands, "capabilities punting route mpls bundle vlan pppoe eompls bridge vpls evpn hairpin sgt loconn pmtud vrfysrc gre l2tp l3tp gtp nsh polka gretap pppoetap l2tptap l3tptap pckoudp vxlan pwhe\r\n");
    for (int i = 0; i < dataPorts; i++) fprintf(commands, "portname %i %s\r\n", i, ifaceName[i]);
    fprintf(commands, "cpuport %i\r\n", cpuPort);
    fprintf(commands, "dynrange 32768 1073741823\r\n");
    fprintf(commands, "vrfrange 1 1073741823\r\n");
    fprintf(commands, "neirange 4096 1073741823\r\n");
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
    if (getenv("p4emuNOCONS") != NULL) for (;;) sleep(1);
    unsigned char buf[1024];
    for (;;) {
        printf("> ");
        buf[0] = 0;
        int i = scanf("%1023s", buf);
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
    dataPorts = 0;
    for (int i = 5; i < argc; i++) {
        char*name = argv[i];
        printf("opening %s...", name);
        ifaceName[dataPorts] = malloc(strlen(name)+1);
        if (ifaceName[dataPorts] == NULL) err("error allocating memory");
        strcpy(ifaceName[dataPorts], name);
        ifaceId[dataPorts] = if_nametoindex(name);
        if (ifaceId[dataPorts] == 0) err("error getting interface index");
        printf(" idx=%i\n", ifaceId[dataPorts]);
        dataPorts++;
    }
    if (dataPorts < 2) err("using: dp <addr> <port> <cpuport> <skb/drv/hw> <ifc0> <ifc1> [ifcN]");
#if __LIBBPF_CURRENT_VERSION_GEQ(0, 7)
    printf("libbpf version: %s\n", libbpf_version_string());
#endif
    int bpf_flag = 0;
    if (strcmp(argv[4],"skb") == 0) {
        bpf_flag = XDP_FLAGS_SKB_MODE;
    }
    if (strcmp(argv[4],"drv") == 0) {
        bpf_flag = XDP_FLAGS_DRV_MODE;
    }
    if (strcmp(argv[4],"hw") == 0) {
        bpf_flag = XDP_FLAGS_HW_MODE;
    }
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

    if (LINUX_VERSION_CODE >= KERNEL_VERSION(6,5,0)) {
        strcpy(argv[0] + strlen(argv[0]) - 8, "kern.bin");
    } else {
        strcpy(argv[0] + strlen(argv[0]) - 8, "krno.bin");
    }
    printf("loading %s...\n", argv[0]);
#if __LIBBPF_CURRENT_VERSION_GEQ(0, 7)
    struct bpf_object *bpf_obj = bpf_object__open_file(argv[0], NULL);
    if (bpf_obj == NULL) err("error opening code");
    struct bpf_program *bpf_prog = bpf_object__next_program(bpf_obj, NULL);
    bpf_program__set_type(bpf_prog, BPF_PROG_TYPE_XDP);
    if (bpf_object__load(bpf_obj)) err("error loading code");
    int prog_fd = bpf_program__fd(bpf_prog);
#else
    struct bpf_prog_load_attr prog_load_attr = {
        .prog_type = BPF_PROG_TYPE_XDP,
        .file = argv[0],
    };
    struct bpf_object *bpf_obj;
    int prog_fd;
    if (bpf_prog_load_xattr(&prog_load_attr, &bpf_obj, &prog_fd)) err("error loading code");
#endif

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
    if (route4_fd < 0) err("error finding table");
    route6_fd = bpf_object__find_map_fd_by_name(bpf_obj, "routes6");
    if (route6_fd < 0) err("error finding table");
    tunnel4_fd = bpf_object__find_map_fd_by_name(bpf_obj, "tunnels4");
    if (tunnel4_fd < 0) err("error finding table");
    tunnel6_fd = bpf_object__find_map_fd_by_name(bpf_obj, "tunnels6");
    if (tunnel6_fd < 0) err("error finding table");
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
    polpol_fd = bpf_object__find_map_fd_by_name(bpf_obj, "polpols");
    if (polpol_fd < 0) err("error finding table");
    polidx_fd = bpf_object__find_map_fd_by_name(bpf_obj, "polidxs");
    if (polidx_fd < 0) err("error finding table");
    nsh_fd = bpf_object__find_map_fd_by_name(bpf_obj, "nshs");
    if (nsh_fd < 0) err("error finding table");

    for (int i = 0; i < dataPorts; i++) {
        printf("attaching iface %i, prog %i, flag %i...\n", ifaceId[i], prog_fd, bpf_flag);
#if __LIBBPF_CURRENT_VERSION_GEQ(0, 7)
        if (bpf_xdp_attach(ifaceId[i], prog_fd, bpf_flag, NULL) < 0) err("error attaching code");
#else
        if (bpf_set_link_xdp_fd(ifaceId[i], prog_fd, bpf_flag) < 0) err("error attaching code");
#endif
    }

    __u32 o = 0;
    __u32 p = ifaceId[cpuPort];
    if (bpf_map_update_elem(cpu_port_fd, &o, &p, BPF_ANY) != 0) err("error setting cpuport");
    for (__u32 i = 0; i < dataPorts; i++) {
        printf("initializing index %i...\n", ifaceId[i]);
        struct port_res ntry;
        memset(&ntry, 0, sizeof(ntry));
        ntry.idx = ifaceId[i];
        if (bpf_map_update_elem(tx_ports_fd, &i, &ntry, BPF_ANY) != 0) err("error setting txport");
        o = ifaceId[i];
        ntry.idx = i;
        if (bpf_map_update_elem(rx_ports_fd, &o, &ntry, BPF_ANY) != 0) err("error setting rxport");
    }

    setgid(1);
    setuid(1);
    pthread_t threadSock;
    if (pthread_create(&threadSock, NULL, (void*) & doSockLoop, NULL)) err("error creating socket thread");
    pthread_t threadStat;
    if (pthread_create(&threadStat, NULL, (void*) & doStatLoop, NULL)) err("error creating status thread");

    doMainLoop();
}
