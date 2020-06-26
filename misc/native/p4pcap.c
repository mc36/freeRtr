#include <arpa/inet.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <netinet/in.h>
#include <pthread.h>
#include <pcap.h>
#include <unistd.h>


#include "p4core.h"

pcap_t *ifacePcap[maxPorts];

void sendpack(unsigned char *bufD, int bufS, int port) {
    pcap_sendpacket(ifacePcap[port], bufD, bufS);
}


#include "p4core.c"


pthread_t threadRaw[maxPorts];
int commandSock;
int ifaceId[maxPorts];



void err(char*buf) {
    printf("%s\n", buf);
    exit(1);
}




void doHostLoop() {
    struct pcap_pkthdr head;
    const unsigned char *pack;
    unsigned char bufD[16384];
    int bufS;
    unsigned int addrLen;
    for (;;) {
        pack = pcap_next(ifacePcap[0], &head);
        bufS = head.caplen;
        if (bufS < 0) break;
        memmove(&bufD[preBuff], pack, bufS);
        processCpuPack(&bufD[0], bufS);
    }
    err("host thread exited");
}



void doDataLoop(int * param) {
    int port = *param;
    unsigned char bufD[16384];
    struct pcap_pkthdr head;
    const unsigned char *pack;
    int bufS;
    unsigned int addrLen;
    for (;;) {
        pack = pcap_next(ifacePcap[port], &head);
        bufS = head.caplen;
        if (bufS < 0) break;
        memmove(&bufD[preBuff], pack, bufS);
        processDataPacket(&bufD[0], bufS, port);
    }
    err("port thread exited");
}



void doSockLoop() {
    FILE *commands = fdopen(commandSock, "rw");
    if (commands == NULL) err("failed to open file");
    unsigned char buf[1024];
    for (;;) {
        strcpy(buf, "quit\r\n");
        fgets(buf, sizeof(buf), commands);
        if (doOneCommand(&buf[0]) != 0) break;
    }
    err("command thread exited");
}



void doStatLoop() {
    FILE *commands = fdopen(commandSock, "w");
    if (commands == NULL) err("failed to open file");
    for (;;) {
        doStatRount(commands);
        sleep(1);
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




int main(int argc, char **argv) {
    unsigned char errbuf[PCAP_ERRBUF_SIZE + 1];

    ports = 0;
    for (int i = 4; i < argc; i++) {
        initIface(ports, argv[i]);
        ports++;
    }
    cpuport = atoi(argv[3]);
    printf("cpu port is #%i of %i...\n", cpuport, ports);

    if (ports < 2) {
        err("using: dp <addr> <port> <cpuport> <ifc0> <ifc1> [ifcN] ...");
    }

    initTables();

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

    for (int i = 0; i < ports; i++) {
        printf("opening interface %s.\n", ifaceName[i]);
        ifacePcap[i] = pcap_create(ifaceName[i], errbuf);
        if (ifacePcap[i] == NULL) err("unable to open interface");
        if (pcap_set_snaplen(ifacePcap[i], 65536) < 0) err("unable to set snaplen");
        if (pcap_set_promisc(ifacePcap[i], 1) < 0) err("unable to set promisc");
        if (pcap_set_immediate_mode(ifacePcap[i], 1) < 0) err("unable to set immediate");
        if (pcap_activate(ifacePcap[i]) < 0) err("activation failed");
        if (pcap_setdirection(ifacePcap[i], PCAP_D_IN) < 0) err("unable to set direction");
        ifaceId[i] = i;
    }

    pthread_t threadSock;
    if (pthread_create(&threadSock, NULL, (void*) & doSockLoop, NULL)) err("error creating socket thread");
    pthread_t threadStat;
    if (pthread_create(&threadStat, NULL, (void*) & doStatLoop, NULL)) err("error creating status thread");

    if (pthread_create(&threadRaw[0], NULL, (void*) & doHostLoop, NULL)) err("error creating host thread");
    for (int i=1; i < ports; i++) {
        if (pthread_create(&threadRaw[i], NULL, (void*) & doDataLoop, &ifaceId[i])) err("error creating port thread");
    }

    doMainLoop();
}
