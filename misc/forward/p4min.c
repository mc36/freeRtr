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


#include "p4emu_hdr.h"
#include "utils.h"


#include "p4raw1.h"


int getState(int port) {
    if (port == 0) return 1;
#include "p4raw2.h"
}








int main(int argc, char **argv) {
    dataPorts = 1;
    initIface(0, "cpudprt");
    for (int i = 7; i < argc; i++) {
        initIface(dataPorts, argv[i]);
        dataPorts++;
    }
    if (dataPorts < 2) err("using: dp <laddr> <lport> <raddr> <rport> <addr> <port> <ifc0> <ifc1> [ifcN]");
    if (dataPorts > maxPorts) dataPorts = maxPorts;
    if (initTables() != 0) err("error initializing tables");
    int portLoc = atoi(argv[2]);
    int portRem = atoi(argv[4]);
    struct sockaddr_in addrLoc;
    struct sockaddr_in addrRem;
    memset(&addrLoc, 0, sizeof (addrLoc));
    memset(&addrRem, 0, sizeof (addrRem));
    if (inet_aton(argv[1], &addrLoc.sin_addr) == 0) err("bad laddr address");
    if (inet_aton(argv[3], &addrRem.sin_addr) == 0) err("bad raddr address");
    addrLoc.sin_family = AF_INET;
    addrLoc.sin_port = htons(portLoc);
    addrRem.sin_family = AF_INET;
    addrRem.sin_port = htons(portRem);
    if ((ifaceSock[0] = socket(AF_INET, SOCK_DGRAM, IPPROTO_UDP)) < 0) err("unable to udp open socket");
    if (bind(ifaceSock[0], (struct sockaddr *) &addrLoc, sizeof (addrLoc)) < 0) err("failed to bind socket");
    printf("binded to local port %s %i.\n", inet_ntoa(addrLoc.sin_addr), portLoc);
    if (connect(ifaceSock[0], (struct sockaddr *) &addrRem, sizeof (addrRem)) < 0) err("failed to connect socket");
    printf("will send to %s %i.\n", inet_ntoa(addrRem.sin_addr), portRem);
    portRem = atoi(argv[6]);
    memset(&addrRem, 0, sizeof (addrRem));
    if (inet_aton(argv[5], &addrRem.sin_addr) == 0) err("bad addr address");
    addrRem.sin_family = AF_INET;
    addrRem.sin_port = htons(portRem);
    printf("connecting %s %i.\n", inet_ntoa(addrRem.sin_addr), portRem);
    commandSock = socket(AF_INET, SOCK_STREAM, 0);
    if (commandSock < 0) err("unable to open socket");
    if (connect(commandSock, (struct sockaddr*)&addrRem, sizeof(addrRem)) < 0) err("failed to connect socket");
    cpuPort = 0;
    for (int o = 1; o < dataPorts; o++) {
#include "p4raw3.h"
    }
    doNegotiate("min");
#include "p4raw4.h"
}
