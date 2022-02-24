#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <pthread.h>
#include <arpa/inet.h>
#include <pcap.h>



char *iface1name;
char *iface2name;
pcap_t *iface1pcap;
pcap_t *iface2pcap;
pthread_t threadIfc1;
pthread_t threadIfc2;
pthread_t threadPrint;
long int byteRx;
long int packRx;
long int byteTx;
long int packTx;

void err(char*buf) {
    printf("%s\n", buf);
    exit(1);
}

void gotIfc1pack(unsigned char*dummyparameter, const struct pcap_pkthdr *hdr, unsigned char *dat) {
    int len = hdr->caplen;
    packRx++;
    byteRx += len;
    pcap_sendpacket(iface2pcap, dat, len);
}

void gotIfc2pack(unsigned char*dummyparameter, const struct pcap_pkthdr *hdr, unsigned char *dat) {
    int len = hdr->caplen;
    packTx++;
    byteTx += len;
    pcap_sendpacket(iface1pcap, dat, len);
}

void doIfc1loop() {
    pcap_loop(iface1pcap, 0, (pcap_handler) gotIfc1pack, NULL);
    err("iface1 thread exited");
}

void doIfc2loop() {
    pcap_loop(iface2pcap, 0, (pcap_handler) gotIfc2pack, NULL);
    err("iface2 thread exited");
}


void doMainLoop() {
    unsigned char buf[1024];

doer:
    printf("> ");
    buf[0] = 0;
    int i = scanf("%1024s", buf);
    if (i < 1) {
        sleep(1);
        goto doer;
    }
    switch (buf[0]) {
    case 0:
        goto doer;
        break;
    case 'H':
    case 'h':
    case '?':
        printf("commands:\n");
        printf("h - this help\n");
        printf("q - exit process\n");
        printf("d - display counters\n");
        printf("c - clear counters\n");
        break;
    case 'Q':
    case 'q':
        err("exiting");
        break;
    case 'D':
    case 'd':
        printf("iface counters:\n");
        printf("                      packets                bytes\n");
        printf("received %20li %20li\n", packRx, byteRx);
        printf("sent     %20li %20li\n", packTx, byteTx);
        break;
    case 'C':
    case 'c':
        printf("counters cleared.\n");
        byteRx = 0;
        packRx = 0;
        byteTx = 0;
        packTx = 0;
        break;
    default:
        printf("unknown command '%s', try ?\n", buf);
        break;
    }
    printf("\n");

    goto doer;
}

int main(int argc, char **argv) {
    char errbuf[PCAP_ERRBUF_SIZE + 1];

    if (argc < 3) {
        if (argc <= 1) goto help;
        char*curr = argv[1];
        pcap_if_t *pcifcs;
        if ((curr[0] == '-') || (curr[0] == '/')) curr++;
        switch (curr[0]) {
        case 'V':
        case 'v':
            err("pcap interface connector v1.0");
            break;
        case 'L':
        case 'l':
            if (pcap_findalldevs(&pcifcs, errbuf) == -1) err("unable to find all interfaces");
            printf("list of interfaces:\n");
            int i = 0;
            for (; pcifcs; pcifcs = pcifcs->next) {
                i++;
                printf("interface #%i:\n", i);
                printf("  name: %s\n", pcifcs->name);
                if (pcifcs->description != NULL) printf("  desc: %s\n", pcifcs->description);
            }
            if (i < 1) err("no interfaces found");
            break;
        case '?':
        case 'h':
        case 'H':
help :
            curr = argv[0];
            printf("using: %s <iface1> <iface2>\n", curr);
            printf("   or: %s <command>\n", curr);
            printf("commands: l=list interfaces\n");
            printf("          v=version\n");
            printf("          h=this help\n");
            exit(1);
            break;
        default:
            err("unknown command, try -h");
            break;
        }
        exit(1);
    }


    printf("pcap version: %s\n", pcap_lib_version());

    iface1name = malloc(strlen(argv[1]) + 1);
    if (iface1name == NULL) err("error allocating memory");
    strcpy(iface1name, argv[1]);
    printf("opening interface %s\n", iface1name);
    iface1pcap = pcap_create(iface1name, errbuf);
    if (iface1pcap == NULL) err("unable to open interface");
    if (pcap_set_snaplen(iface1pcap, 65536) < 0) err("unable to set snaplen");
    if (pcap_set_promisc(iface1pcap, 1) < 0) err("unable to set promisc");
    if (pcap_set_immediate_mode(iface1pcap, 1) < 0) err("unable to set immediate");
    if (pcap_activate(iface1pcap) < 0) err("activation failed");
    if (pcap_setdirection(iface1pcap, PCAP_D_IN) < 0) err("unable to set direction");

    iface2name = malloc(strlen(argv[2]) + 1);
    if (iface2name == NULL) err("error allocating memory");
    strcpy(iface2name, argv[2]);
    printf("opening interface %s\n", iface2name);
    iface2pcap = pcap_create(iface2name, errbuf);
    if (iface2pcap == NULL) err("unable to open interface");
    if (pcap_set_snaplen(iface2pcap, 65536) < 0) err("unable to set snaplen");
    if (pcap_set_promisc(iface2pcap, 1) < 0) err("unable to set promisc");
    if (pcap_set_immediate_mode(iface2pcap, 1) < 0) err("unable to set immediate");
    if (pcap_activate(iface2pcap) < 0) err("activation failed");
    if (pcap_setdirection(iface2pcap, PCAP_D_IN) < 0) err("unable to set direction");

    printf("serving others\n");

    byteRx = 0;
    packRx = 0;
    byteTx = 0;
    packTx = 0;
    if (pthread_create(&threadIfc1, NULL, (void*) & doIfc1loop, NULL)) err("error creating iface1 thread");
    if (pthread_create(&threadIfc2, NULL, (void*) & doIfc2loop, NULL)) err("error creating iface2 thread");

    doMainLoop();
}
