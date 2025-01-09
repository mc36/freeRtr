#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <pthread.h>
#include <arpa/inet.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <net/if.h>
#include <linux/bpf.h>
#include <linux/if_link.h>
#include <linux/rtnetlink.h>
#include <bpf/bpf.h>
#include <bpf/libbpf.h>
#include <libmnl/libmnl.h>
#include "p4mnl_tab.h"

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
struct mnl_socket *mnlSocket;
int mnlPortid;
int mnlSequence;
int ifaceId[maxPorts];
char *ifaceName[maxPorts];
int dataPorts;
int cpu_port_fd;
int rx_ports_fd;
int tx_ports_fd;
int cpuPort;


#include "p4mnl_msg.h"


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
    fprintf(commands, "platform p4mnl\r\n");
    fprintf(commands, "capabilities route\r\n");
    for (int i = 0; i < dataPorts; i++) fprintf(commands, "portname %i %s\r\n", i, ifaceName[i]);
    fprintf(commands, "cpuport %i\r\n", cpuPort);
    fprintf(commands, "dynrange 32768 1073741823\r\n");
    fprintf(commands, "vrfrange 1 255\r\n");
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
    for (int i = 4; i < argc; i++) {
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
    if (dataPorts < 2) err("using: dp <addr> <port> <cpuport> <ifc0> <ifc1> [ifcN]");
#if __LIBBPF_CURRENT_VERSION_GEQ(0, 7)
    printf("libbpf version: %s\n", libbpf_version_string());
#endif
    mnlSocket = mnl_socket_open(NETLINK_ROUTE);
    if (mnlSocket == NULL) err("error opening netlink");
    if (mnl_socket_bind(mnlSocket, 0, MNL_SOCKET_AUTOPID) < 0) err("error binding netlink");
    mnlPortid = mnl_socket_get_portid(mnlSocket);
    mnlSequence = 1;

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

    strcpy(argv[0] + strlen(argv[0]) - 8, "kern.bin");
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


    for (int i = 0; i < dataPorts; i++) {
        printf("attaching iface %i, prog %i...\n", ifaceId[i], prog_fd);
#if __LIBBPF_CURRENT_VERSION_GEQ(0, 7)
        if (bpf_xdp_attach(ifaceId[i], prog_fd, XDP_FLAGS_SKB_MODE, NULL) < 0) err("error attaching code");
#else
        if (bpf_set_link_xdp_fd(ifaceId[i], prog_fd, XDP_FLAGS_SKB_MODE) < 0) err("error attaching code");
#endif
    }

    __u32 o = 0;
    __u32 p = ifaceId[cpuPort];
    if (bpf_map_update_elem(cpu_port_fd, &o, &p, BPF_ANY) != 0) err("error setting cpuport");
    for (__u32 i = 0; i < dataPorts; i++) {
        printf("initializing index %i...\n", ifaceId[i]);
        __u32 o = ifaceId[i];
        if (bpf_map_update_elem(tx_ports_fd, &i, &o, BPF_ANY) != 0) err("error setting txport");
        if (bpf_map_update_elem(rx_ports_fd, &o, &i, BPF_ANY) != 0) err("error setting rxport");
    }

    pthread_t threadSock;
    if (pthread_create(&threadSock, NULL, (void*) & doSockLoop, NULL)) err("error creating socket thread");
    pthread_t threadStat;
    if (pthread_create(&threadStat, NULL, (void*) & doStatLoop, NULL)) err("error creating status thread");

    doMainLoop();
}
