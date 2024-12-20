#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <poll.h>
#include <arpa/inet.h>
#include <sys/socket.h>
#include <netinet/in.h>


#include "p4emu_hdr.h"


struct sockaddr_in peers[maxPorts];
int sockets[maxPorts];
int commandSock;
int ifaceId[maxPorts];


void sendPack(unsigned char *bufD, int bufS, int port) {
    sendto(sockets[port], bufD, bufS, 0, (struct sockaddr *) &peers[port], sizeof(peers[port]));
}


void setMtu(int port, int mtu) {
}

void setState(int port, int sta) {
}

int getState(int port) {
    return 1;
}


void getStats(int port, unsigned char*buf, unsigned char*pre, int*len) {
}

void err(char*buf) {
    printf("%s\n", buf);
    _exit(1);
}



int main(int argc, char **argv) {
    dataPorts = (argc - 6) / 2;
    if (dataPorts < 2) err("using: dp <addr> <port> <cpuport> <laddr> <raddr> <lport1> <rport1> [lportN] [rportN]");
    if (dataPorts > maxPorts) dataPorts = maxPorts;
    struct sockaddr_in addrLoc;
    memset(&addrLoc, 0, sizeof(addrLoc));
    if (inet_aton(argv[4], &addrLoc.sin_addr) == 0) err("bad laddr address");
    addrLoc.sin_family = AF_INET;
    for (int i = 0; i < dataPorts; i++) {
        unsigned char buf[1024];
        sprintf((char*)&buf[0], "udport-%i", i);
        initIface(i, (char*)&buf[0]);
        memset(&peers[i], 0, sizeof(peers[i]));
        if (inet_aton(argv[5], &peers[i].sin_addr) == 0) err("bad raddr address");
        addrLoc.sin_port = htons(atoi(argv[(i*2)+6]));
        peers[i].sin_family = AF_INET;
        peers[i].sin_port = htons(atoi(argv[(i*2)+7]));
        if ((sockets[i] = socket(AF_INET, SOCK_DGRAM, IPPROTO_UDP)) < 0) err("unable to open socket");
        if (bind(sockets[i], (struct sockaddr *) &addrLoc, sizeof(addrLoc)) < 0) err("failed to bind socket");
        ifaceId[i] = i;
    }
    if (initTables() != 0) err("error initializing tables");
    int port = atoi(argv[2]);
    memset(&addrLoc, 0, sizeof(addrLoc));
    if (inet_aton(argv[1], &addrLoc.sin_addr) == 0) err("bad addr address");
    addrLoc.sin_family = AF_INET;
    addrLoc.sin_port = htons(port);
    printf("connecting %s %i.\n", inet_ntoa(addrLoc.sin_addr), port);
    commandSock = socket(AF_INET, SOCK_STREAM, 0);
    if (commandSock < 0) err("unable to open socket");
    if (connect(commandSock, (struct sockaddr*)&addrLoc, sizeof(addrLoc)) < 0) err("failed to connect socket");
    cpuPort = atoi(argv[3]);
    printf("cpu port is #%i of %i...\n", cpuPort, dataPorts);

    struct packetContext ctx;
    if (initContext(&ctx) != 0) err("error initializing context");
    struct sockaddr_in addrTmp;
    unsigned int addrLen;
    int bufS;
    struct pollfd fds[maxPorts+2];
    FILE *commandr = fdopen(commandSock, "r");
    if (commandr == NULL) err("failed to open file");
    FILE *commands = fdopen(commandSock, "w");
    if (commands == NULL) err("failed to open file");
    fprintf(commands, "platform %sudp\r\n", platformBase);
    fprintf(commands, "capabilities %s\r\n", getCapas());
    for (int i = 0; i < dataPorts; i++) fprintf(commands, "portname %i %s\r\n", i, ifaceName[i]);
    fprintf(commands, "cpuport %i\r\n", cpuPort);
    fprintf(commands, "dynrange %i 1073741823\r\n", maxPorts);
    fprintf(commands, "vrfrange 1 1073741823\r\n");
    fprintf(commands, "neirange 4096 1073741823\r\n");
    fprintf(commands, "nomore\r\n");
    fflush(commands);

    int rnd = 0;
    for (;;) {
        for (int i = 0; i < dataPorts; i++) {
            fds[i].fd = sockets[i];
            fds[i].events = POLLIN;
            fds[i].revents = 0;
        }
        fds[dataPorts].fd = commandSock;
        fds[dataPorts].events = POLLIN;
        fds[dataPorts].revents = 0;
        fds[dataPorts+1].fd = STDIN_FILENO;
        fds[dataPorts+1].events = POLLIN;
        fds[dataPorts+1].revents = 0;
        if (poll(fds, dataPorts+2, 100) < 0) err("error in poll");
        int don = 0;
        for (int i = 0; i < dataPorts; i++) {
            if ((fds[i].revents&POLLERR) != 0) err("error on socket");
            if ((fds[i].revents&POLLHUP) != 0) err("hangup on socket");
            if ((fds[i].revents&POLLIN) == 0) continue;
            don++;
            addrLen = sizeof(addrTmp);
            bufS = totBuff - preBuff;
            bufS = recvfrom(sockets[i], &ctx.bufD[preBuff], bufS, 0, (struct sockaddr *) &addrTmp, &addrLen);
            if (bufS < 0) err("error reading socket");
            ctx.port = i;
            if (i == cpuPort) {
                processCpuPack(&ctx, bufS);
            } else {
                processDataPacket(&ctx, bufS, i);
            }
        }
        if (don == 0) {
            doStatRound(commands, rnd);
            rnd++;
        }
        if ((fds[dataPorts].revents&POLLERR) != 0) err("error on socket");
        if ((fds[dataPorts].revents&POLLHUP) != 0) err("hangup on socket");
        if ((fds[dataPorts].revents&POLLIN) != 0) {
            memset(ctx.bufD, 0, totBuff);
            if (fgets((char*)&ctx.bufD[0], totBuff, commandr) == NULL) err("error reading socket");
            if (doOneCommand(&ctx, &ctx.bufD[0]) != 0) err("command exited");
        }
        if ((fds[dataPorts+1].revents&POLLERR) != 0) err("error on socket");
        if ((fds[dataPorts+1].revents&POLLHUP) != 0) err("hangup on socket");
        if ((fds[dataPorts+1].revents&POLLIN) != 0) {
            memset(ctx.bufD, 0, totBuff);
            scanf("%1023s", &ctx.bufD[0]);
            if (doConsoleCommand(&ctx.bufD[0]) != 0) err("console exited");
        }
    }
}
