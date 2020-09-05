#include "utils.h"
#include "table.h"
#include "types.h"


#define preBuff 64

int ports = 0;
int cpuport = 0;
int punts = 0;
int ipids = 0;

unsigned char *ifaceName[maxPorts];
long int byteRx[maxPorts];
long int packRx[maxPorts];
long int byteTx[maxPorts];
long int packTx[maxPorts];
long int byteDr[maxPorts];
long int packDr[maxPorts];




struct mpls_entry {
    int label;
    int command;    // 1=vrf, 2=pop, 3=swap, 4=xconn, 5=vpls, 6=punt
    int nexthop;
    int port;
    int bridge;
    int vrf;
    int ver;
    int swap;
    long pack;
    long byte;
};

struct table_head mpls_table;

int mpls_compare(void *ptr1, void *ptr2) {
    struct mpls_entry *ntry1 = ptr1;
    struct mpls_entry *ntry2 = ptr2;
    if (ntry1->label < ntry2->label) return -1;
    if (ntry1->label > ntry2->label) return +1;
    return 0;
}


struct portvrf_entry {
    int port;
    int command;    // 1=vrf, 2=bridge, 3=xconn
    int vrf;
    int bridge;
    int nexthop;
    int label1;
    int label2;
};

struct table_head portvrf_table;

int portvrf_compare(void *ptr1, void *ptr2) {
    struct portvrf_entry *ntry1 = ptr1;
    struct portvrf_entry *ntry2 = ptr2;
    if (ntry1->port < ntry2->port) return -1;
    if (ntry1->port > ntry2->port) return +1;
    return 0;
}


struct route4_entry {
    int vrf;
    int mask;
    int addr;
    int command;    // 1=route, 2=punt, 3=mpls1, 4=mpls2
    int nexthop;
    int label1;
    int label2;
    long pack;
    long byte;
};

struct table_head route4_table;

int route4_compare(void *ptr1, void *ptr2) {
    struct route4_entry *ntry1 = ptr1;
    struct route4_entry *ntry2 = ptr2;
    if (ntry1->vrf < ntry2->vrf) return -1;
    if (ntry1->vrf > ntry2->vrf) return +1;
    if (ntry1->mask < ntry2->mask) return -1;
    if (ntry1->mask > ntry2->mask) return +1;
    if (ntry1->addr < ntry2->addr) return -1;
    if (ntry1->addr > ntry2->addr) return +1;
    return 0;
}



struct route6_entry {
    int vrf;
    int mask;
    int addr1;
    int addr2;
    int addr3;
    int addr4;
    int command;    // 1=route, 2=punt, 3=mpls1, 4=mpls2
    int nexthop;
    int label1;
    int label2;
    long pack;
    long byte;
};

struct table_head route6_table;

int route6_compare(void *ptr1, void *ptr2) {
    struct route6_entry *ntry1 = ptr1;
    struct route6_entry *ntry2 = ptr2;
    if (ntry1->vrf < ntry2->vrf) return -1;
    if (ntry1->vrf > ntry2->vrf) return +1;
    if (ntry1->mask < ntry2->mask) return -1;
    if (ntry1->mask > ntry2->mask) return +1;
    if (ntry1->addr1 < ntry2->addr1) return -1;
    if (ntry1->addr1 > ntry2->addr1) return +1;
    if (ntry1->addr2 < ntry2->addr2) return -1;
    if (ntry1->addr2 > ntry2->addr2) return +1;
    if (ntry1->addr3 < ntry2->addr3) return -1;
    if (ntry1->addr3 > ntry2->addr3) return +1;
    if (ntry1->addr4 < ntry2->addr4) return -1;
    if (ntry1->addr4 > ntry2->addr4) return +1;
    return 0;
}


struct neigh_entry {
    int id;
    int vrf;
    int command;    // 1=rawip, 2=pppoe, 3=gre4, 4=gre6, 5=l2tp4, 6=l2tp6, 7=ipip4, 8=ipip6
    int port;
    int aclport;
    int session;
    unsigned char smac[6];
    unsigned char dmac[6];
    int sip1;
    int sip2;
    int sip3;
    int sip4;
    int dip1;
    int dip2;
    int dip3;
    int dip4;
    int sprt;
    int dprt;
    int tid;
    long pack;
    long byte;
};

struct table_head neigh_table;

int neigh_compare(void *ptr1, void *ptr2) {
    struct neigh_entry *ntry1 = ptr1;
    struct neigh_entry *ntry2 = ptr2;
    if (ntry1->id < ntry2->id) return -1;
    if (ntry1->id > ntry2->id) return +1;
    return 0;
}


struct bridge_entry {
    int id;
    int mac1;
    int mac2;
    int command;    // 1=port, 2=vpls, 3=route, 4=vxlan4, 5=vxlan6
    int port;
    int nexthop;
    int label1;
    int label2;
    int srcAddr1;
    int srcAddr2;
    int srcAddr3;
    int srcAddr4;
    int trgAddr1;
    int trgAddr2;
    int trgAddr3;
    int trgAddr4;
    int instance;
    long pack;
    long byte;
};

struct table_head bridge_table;

int bridge_compare(void *ptr1, void *ptr2) {
    struct bridge_entry *ntry1 = ptr1;
    struct bridge_entry *ntry2 = ptr2;
    if (ntry1->id < ntry2->id) return -1;
    if (ntry1->id > ntry2->id) return +1;
    if (ntry1->mac2 < ntry2->mac2) return -1;
    if (ntry1->mac2 > ntry2->mac2) return +1;
    if (ntry1->mac1 < ntry2->mac1) return -1;
    if (ntry1->mac1 > ntry2->mac1) return +1;
    return 0;
}


struct vlan_entry {
    int id;
    int vlan;
    int port;
    long pack;
    long byte;
};

struct table_head vlanin_table;

struct table_head vlanout_table;

int vlanin_compare(void *ptr1, void *ptr2) {
    struct vlan_entry *ntry1 = ptr1;
    struct vlan_entry *ntry2 = ptr2;
    if (ntry1->port < ntry2->port) return -1;
    if (ntry1->port > ntry2->port) return +1;
    if (ntry1->vlan < ntry2->vlan) return -1;
    if (ntry1->vlan > ntry2->vlan) return +1;
    return 0;
}

int vlanout_compare(void *ptr1, void *ptr2) {
    struct vlan_entry *ntry1 = ptr1;
    struct vlan_entry *ntry2 = ptr2;
    if (ntry1->id < ntry2->id) return -1;
    if (ntry1->id > ntry2->id) return +1;
    return 0;
}


struct acls_entry {
    int ver;
    int dir; // 1=in, 2=out, 3=nat, 4=copp
    int port;
    struct table_head aces;
};

struct table_head acls_table;

int acls_compare(void *ptr1, void *ptr2) {
    struct acls_entry *ntry1 = ptr1;
    struct acls_entry *ntry2 = ptr2;
    if (ntry1->ver < ntry2->ver) return -1;
    if (ntry1->ver > ntry2->ver) return +1;
    if (ntry1->dir < ntry2->dir) return -1;
    if (ntry1->dir > ntry2->dir) return +1;
    if (ntry1->port < ntry2->port) return -1;
    if (ntry1->port > ntry2->port) return +1;
    return 0;
}


struct acl4_entry {
    int pri;
    int act;
    int srcAddr;
    int srcMask;
    int trgAddr;
    int trgMask;
    int protV;
    int protM;
    int srcPortV;
    int srcPortM;
    int trgPortV;
    int trgPortM;
};

int acl4_compare(void *ptr1, void *ptr2) {
    struct acl4_entry *ntry1 = ptr1;
    struct acl4_entry *ntry2 = ptr2;
    if (ntry1->pri < ntry2->pri) return -1;
    if (ntry1->pri > ntry2->pri) return +1;
    return 0;
}

int acl4_matcher(void *ptr1, void *ptr2) {
    struct acl4_entry *ntry1 = ptr1;
    struct acl4_entry *ntry2 = ptr2;
    if ((ntry1->protV & ntry2->protM) != ntry2->protV) return 1;
    if ((ntry1->srcPortV & ntry2->srcPortM) != ntry2->srcPortV) return 1;
    if ((ntry1->trgPortV & ntry2->trgPortM) != ntry2->trgPortV) return 1;
    if ((ntry1->srcAddr & ntry2->srcMask) != ntry2->srcAddr) return 1;
    if ((ntry1->trgAddr & ntry2->trgMask) != ntry2->trgAddr) return 1;
    return 0;
}


struct acl6_entry {
    int pri;
    int act;
    int srcAddr1;
    int srcAddr2;
    int srcAddr3;
    int srcAddr4;
    int srcMask1;
    int srcMask2;
    int srcMask3;
    int srcMask4;
    int trgAddr1;
    int trgAddr2;
    int trgAddr3;
    int trgAddr4;
    int trgMask1;
    int trgMask2;
    int trgMask3;
    int trgMask4;
    int protV;
    int protM;
    int srcPortV;
    int srcPortM;
    int trgPortV;
    int trgPortM;
};

int acl6_compare(void *ptr1, void *ptr2) {
    struct acl6_entry *ntry1 = ptr1;
    struct acl6_entry *ntry2 = ptr2;
    if (ntry1->pri < ntry2->pri) return -1;
    if (ntry1->pri > ntry2->pri) return +1;
    return 0;
}

int acl6_matcher(void *ptr1, void *ptr2) {
    struct acl6_entry *ntry1 = ptr1;
    struct acl6_entry *ntry2 = ptr2;
    if ((ntry1->protV & ntry2->protM) != ntry2->protV) return 1;
    if ((ntry1->srcPortV & ntry2->srcPortM) != ntry2->srcPortV) return 1;
    if ((ntry1->trgPortV & ntry2->trgPortM) != ntry2->trgPortV) return 1;
    if ((ntry1->srcAddr1 & ntry2->srcMask1) != ntry2->srcAddr1) return 1;
    if ((ntry1->srcAddr2 & ntry2->srcMask2) != ntry2->srcAddr2) return 1;
    if ((ntry1->srcAddr3 & ntry2->srcMask3) != ntry2->srcAddr3) return 1;
    if ((ntry1->srcAddr4 & ntry2->srcMask4) != ntry2->srcAddr4) return 1;
    if ((ntry1->trgAddr1 & ntry2->trgMask1) != ntry2->trgAddr1) return 1;
    if ((ntry1->trgAddr2 & ntry2->trgMask2) != ntry2->trgAddr2) return 1;
    if ((ntry1->trgAddr3 & ntry2->trgMask3) != ntry2->trgAddr3) return 1;
    if ((ntry1->trgAddr4 & ntry2->trgMask4) != ntry2->trgAddr4) return 1;
    return 0;
}


struct aclH_entry {
    int pri;
    int act;
};

int apply_acl(struct table_head *tab, void *ntry, int matcher(void *, void *)) {
    for (int i=tab->size-1; i>=0; i--) {
        struct aclH_entry *res = table_get(tab, i);
        if (matcher(ntry, res) != 0) continue;
        return res->act;
    }
    return 1;
}


struct nat4_entry {
    int vrf;
    int prot;
    int oSrcAddr;
    int oTrgAddr;
    int oSrcPort;
    int oTrgPort;
    int nSrcAddr;
    int nTrgAddr;
    int nSrcPort;
    int nTrgPort;
    int sum3;
    int sum4;
    long pack;
    long byte;
};

struct table_head nat4_table;

int nat4_compare(void *ptr1, void *ptr2) {
    struct nat4_entry *ntry1 = ptr1;
    struct nat4_entry *ntry2 = ptr2;
    if (ntry1->vrf < ntry2->vrf) return -1;
    if (ntry1->vrf > ntry2->vrf) return +1;
    if (ntry1->prot < ntry2->prot) return -1;
    if (ntry1->prot > ntry2->prot) return +1;
    if (ntry1->oSrcPort < ntry2->oSrcPort) return -1;
    if (ntry1->oSrcPort > ntry2->oSrcPort) return +1;
    if (ntry1->oTrgPort < ntry2->oTrgPort) return -1;
    if (ntry1->oTrgPort > ntry2->oTrgPort) return +1;
    if (ntry1->oSrcAddr < ntry2->oSrcAddr) return -1;
    if (ntry1->oSrcAddr > ntry2->oSrcAddr) return +1;
    if (ntry1->oTrgAddr < ntry2->oTrgAddr) return -1;
    if (ntry1->oTrgAddr > ntry2->oTrgAddr) return +1;
    return 0;
}


struct nat6_entry {
    int vrf;
    int prot;
    int oSrcAddr1;
    int oSrcAddr2;
    int oSrcAddr3;
    int oSrcAddr4;
    int oTrgAddr1;
    int oTrgAddr2;
    int oTrgAddr3;
    int oTrgAddr4;
    int oSrcPort;
    int oTrgPort;
    int nSrcAddr1;
    int nSrcAddr2;
    int nSrcAddr3;
    int nSrcAddr4;
    int nTrgAddr1;
    int nTrgAddr2;
    int nTrgAddr3;
    int nTrgAddr4;
    int nSrcPort;
    int nTrgPort;
    int sum3;
    int sum4;
    long pack;
    long byte;
};

struct table_head nat6_table;

int nat6_compare(void *ptr1, void *ptr2) {
    struct nat6_entry *ntry1 = ptr1;
    struct nat6_entry *ntry2 = ptr2;
    if (ntry1->vrf < ntry2->vrf) return -1;
    if (ntry1->vrf > ntry2->vrf) return +1;
    if (ntry1->prot < ntry2->prot) return -1;
    if (ntry1->prot > ntry2->prot) return +1;
    if (ntry1->oSrcPort < ntry2->oSrcPort) return -1;
    if (ntry1->oSrcPort > ntry2->oSrcPort) return +1;
    if (ntry1->oTrgPort < ntry2->oTrgPort) return -1;
    if (ntry1->oTrgPort > ntry2->oTrgPort) return +1;
    if (ntry1->oSrcAddr1 < ntry2->oSrcAddr1) return -1;
    if (ntry1->oSrcAddr1 > ntry2->oSrcAddr1) return +1;
    if (ntry1->oSrcAddr2 < ntry2->oSrcAddr2) return -1;
    if (ntry1->oSrcAddr2 > ntry2->oSrcAddr2) return +1;
    if (ntry1->oSrcAddr3 < ntry2->oSrcAddr3) return -1;
    if (ntry1->oSrcAddr3 > ntry2->oSrcAddr3) return +1;
    if (ntry1->oSrcAddr4 < ntry2->oSrcAddr4) return -1;
    if (ntry1->oSrcAddr4 > ntry2->oSrcAddr4) return +1;
    if (ntry1->oTrgAddr1 < ntry2->oTrgAddr1) return -1;
    if (ntry1->oTrgAddr1 > ntry2->oTrgAddr1) return +1;
    if (ntry1->oTrgAddr2 < ntry2->oTrgAddr2) return -1;
    if (ntry1->oTrgAddr2 > ntry2->oTrgAddr2) return +1;
    if (ntry1->oTrgAddr3 < ntry2->oTrgAddr3) return -1;
    if (ntry1->oTrgAddr3 > ntry2->oTrgAddr3) return +1;
    if (ntry1->oTrgAddr4 < ntry2->oTrgAddr4) return -1;
    if (ntry1->oTrgAddr4 > ntry2->oTrgAddr4) return +1;
    return 0;
}


struct bundle_entry {
    int id;
    int command;    // 1=port, 2=hairpin
    int out[16];
    long pack;
    long byte;
};

struct table_head bundle_table;

int bundle_compare(void *ptr1, void *ptr2) {
    struct bundle_entry *ntry1 = ptr1;
    struct bundle_entry *ntry2 = ptr2;
    if (ntry1->id < ntry2->id) return -1;
    if (ntry1->id > ntry2->id) return +1;
    return 0;
}


struct pppoe_entry {
    int port;
    int session;
    int aclport;
    long pack;
    long byte;
};

struct table_head pppoe_table;

int pppoe_compare(void *ptr1, void *ptr2) {
    struct pppoe_entry *ntry1 = ptr1;
    struct pppoe_entry *ntry2 = ptr2;
    if (ntry1->port < ntry2->port) return -1;
    if (ntry1->port > ntry2->port) return +1;
    if (ntry1->session < ntry2->session) return -1;
    if (ntry1->session > ntry2->session) return +1;
    return 0;
}


struct tun4_entry {
    int vrf;
    int prot;
    int srcAddr;
    int trgAddr;
    int srcPort;
    int trgPort;
    int command;    // 1=gre, 2=l2tp, 3=vxlan, 4=ip4ip, 5=ip6ip
    int aclport;
    long pack;
    long byte;
};

struct table_head tun4_table;

int tun4_compare(void *ptr1, void *ptr2) {
    struct tun4_entry *ntry1 = ptr1;
    struct tun4_entry *ntry2 = ptr2;
    if (ntry1->vrf < ntry2->vrf) return -1;
    if (ntry1->vrf > ntry2->vrf) return +1;
    if (ntry1->prot < ntry2->prot) return -1;
    if (ntry1->prot > ntry2->prot) return +1;
    if (ntry1->srcPort < ntry2->srcPort) return -1;
    if (ntry1->srcPort > ntry2->srcPort) return +1;
    if (ntry1->trgPort < ntry2->trgPort) return -1;
    if (ntry1->trgPort > ntry2->trgPort) return +1;
    if (ntry1->srcAddr < ntry2->srcAddr) return -1;
    if (ntry1->srcAddr > ntry2->srcAddr) return +1;
    if (ntry1->trgAddr < ntry2->trgAddr) return -1;
    if (ntry1->trgAddr > ntry2->trgAddr) return +1;
    return 0;
}


struct tun6_entry {
    int vrf;
    int prot;
    int srcAddr1;
    int srcAddr2;
    int srcAddr3;
    int srcAddr4;
    int trgAddr1;
    int trgAddr2;
    int trgAddr3;
    int trgAddr4;
    int srcPort;
    int trgPort;
    int command;    // 1=gre, 2=l2tp, 3=vxlan, 4=ip4ip, 5=ip6ip
    int aclport;
    long pack;
    long byte;
};

struct table_head tun6_table;

int tun6_compare(void *ptr1, void *ptr2) {
    struct tun6_entry *ntry1 = ptr1;
    struct tun6_entry *ntry2 = ptr2;
    if (ntry1->vrf < ntry2->vrf) return -1;
    if (ntry1->vrf > ntry2->vrf) return +1;
    if (ntry1->prot < ntry2->prot) return -1;
    if (ntry1->prot > ntry2->prot) return +1;
    if (ntry1->srcPort < ntry2->srcPort) return -1;
    if (ntry1->srcPort > ntry2->srcPort) return +1;
    if (ntry1->trgPort < ntry2->trgPort) return -1;
    if (ntry1->trgPort > ntry2->trgPort) return +1;
    if (ntry1->srcAddr1 < ntry2->srcAddr1) return -1;
    if (ntry1->srcAddr1 > ntry2->srcAddr1) return +1;
    if (ntry1->srcAddr2 < ntry2->srcAddr2) return -1;
    if (ntry1->srcAddr2 > ntry2->srcAddr2) return +1;
    if (ntry1->srcAddr3 < ntry2->srcAddr3) return -1;
    if (ntry1->srcAddr3 > ntry2->srcAddr3) return +1;
    if (ntry1->srcAddr4 < ntry2->srcAddr4) return -1;
    if (ntry1->srcAddr4 > ntry2->srcAddr4) return +1;
    if (ntry1->trgAddr1 < ntry2->trgAddr1) return -1;
    if (ntry1->trgAddr1 > ntry2->trgAddr1) return +1;
    if (ntry1->trgAddr2 < ntry2->trgAddr2) return -1;
    if (ntry1->trgAddr2 > ntry2->trgAddr2) return +1;
    if (ntry1->trgAddr3 < ntry2->trgAddr3) return -1;
    if (ntry1->trgAddr3 > ntry2->trgAddr3) return +1;
    if (ntry1->trgAddr4 < ntry2->trgAddr4) return -1;
    if (ntry1->trgAddr4 > ntry2->trgAddr4) return +1;
    return 0;
}


struct macsec_entry {
    int port;
    int ethtyp;
    unsigned char encrKeyDat[256];
    unsigned char hashKeyDat[256];
    int encrKeyLen;
    int hashKeyLen;
    int encrBlkLen;
    int hashBlkLen;
    int needMacs;
    int seqTx;
    int seqRx;
    const EVP_CIPHER *encrAlg;
    const EVP_MD *hashAlg;
    EVP_PKEY *hashPkey;
};

struct table_head macsec_table;

int macsec_compare(void *ptr1, void *ptr2) {
    struct pppoe_entry *ntry1 = ptr1;
    struct pppoe_entry *ntry2 = ptr2;
    if (ntry1->port < ntry2->port) return -1;
    if (ntry1->port > ntry2->port) return +1;
    return 0;
}








void initIface(int port, unsigned char *name) {
    ifaceName[port] = malloc(1024);
    strcpy(ifaceName[port], name);
    byteRx[port] = 0;
    packRx[port] = 0;
    byteTx[port] = 0;
    packTx[port] = 0;
    byteDr[port] = 0;
    packDr[port] = 0;
}


void initTables() {
    table_init(&mpls_table, sizeof(struct mpls_entry), &mpls_compare);
    table_init(&portvrf_table, sizeof(struct portvrf_entry), &portvrf_compare);
    table_init(&route4_table, sizeof(struct route4_entry), &route4_compare);
    table_init(&route6_table, sizeof(struct route6_entry), &route6_compare);
    table_init(&neigh_table, sizeof(struct neigh_entry), &neigh_compare);
    table_init(&vlanin_table, sizeof(struct vlan_entry), &vlanin_compare);
    table_init(&vlanout_table, sizeof(struct vlan_entry), &vlanout_compare);
    table_init(&bridge_table, sizeof(struct bridge_entry), &bridge_compare);
    table_init(&acls_table, sizeof(struct acls_entry), &acls_compare);
    table_init(&nat4_table, sizeof(struct nat4_entry), &nat4_compare);
    table_init(&nat6_table, sizeof(struct nat6_entry), &nat6_compare);
    table_init(&bundle_table, sizeof(struct bundle_entry), &bundle_compare);
    table_init(&pppoe_table, sizeof(struct pppoe_entry), &pppoe_compare);
    table_init(&tun4_table, sizeof(struct tun4_entry), &tun4_compare);
    table_init(&tun6_table, sizeof(struct tun6_entry), &tun6_compare);
    table_init(&macsec_table, sizeof(struct macsec_entry), &macsec_compare);
    printf("openssl version: %s\n", OpenSSL_version(OPENSSL_VERSION));
    OpenSSL_add_all_ciphers();
    OpenSSL_add_all_digests();
    OpenSSL_add_all_algorithms();
    RAND_get_rand_method();
    RAND_poll();
}
