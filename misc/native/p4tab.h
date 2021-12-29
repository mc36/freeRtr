#include "utils.h"
#include "table.h"
#include "tree.h"
#include "types.h"


#define preBuff 256

int ports = 0;
int cpuport = 0;
int punts = 0;
int ipids = 0;

char *ifaceName[maxPorts];
long int byteRx[maxPorts];
long int packRx[maxPorts];
long int byteTx[maxPorts];
long int packTx[maxPorts];
long int byteDr[maxPorts];
long int packDr[maxPorts];
long int byteMpls[maxPorts];
long int packMpls[maxPorts];
long int byteVlan[maxPorts];
long int packVlan[maxPorts];
long int byteIpv4[maxPorts];
long int packIpv4[maxPorts];
long int byteIpv6[maxPorts];
long int packIpv6[maxPorts];
long int bytePppoe[maxPorts];
long int packPppoe[maxPorts];
long int byteBridge[maxPorts];
long int packBridge[maxPorts];
long int bytePolka[maxPorts];
long int packPolka[maxPorts];
long int byteNsh[maxPorts];
long int packNsh[maxPorts];




struct polkaIdx_entry {
    int vrf;
    int index;
    int nexthop;
    long pack;
    long byte;
};

struct table_head polkaIdx_table;

int polkaIdx_compare(void *ptr1, void *ptr2) {
    struct polkaIdx_entry *ntry1 = ptr1;
    struct polkaIdx_entry *ntry2 = ptr2;
    if (ntry1->vrf < ntry2->vrf) return -1;
    if (ntry1->vrf > ntry2->vrf) return +1;
    if (ntry1->index < ntry2->index) return -1;
    if (ntry1->index > ntry2->index) return +1;
    return 0;
}


struct polkaPoly_entry {
    int port;
    int tab[256];
    long pack;
    long byte;
};

struct table_head polkaPoly_table;

int polkaPoly_compare(void *ptr1, void *ptr2) {
    struct polkaPoly_entry *ntry1 = ptr1;
    struct polkaPoly_entry *ntry2 = ptr2;
    if (ntry1->port < ntry2->port) return -1;
    if (ntry1->port > ntry2->port) return +1;
    return 0;
}


struct nsh_entry {
    int sp;
    int si;
    int command;    // 1=swap, 2=vrf
    int port;
    int vrf;
    int trg;
    unsigned char smac[6];
    unsigned char dmac[6];
    long pack;
    long byte;
};

struct table_head nsh_table;

int nsh_compare(void *ptr1, void *ptr2) {
    struct nsh_entry *ntry1 = ptr1;
    struct nsh_entry *ntry2 = ptr2;
    if (ntry1->sp < ntry2->sp) return -1;
    if (ntry1->sp > ntry2->sp) return +1;
    if (ntry1->si < ntry2->si) return -1;
    if (ntry1->si > ntry2->si) return +1;
    return 0;
}


struct mpls_entry {
    int label;
    int command;    // 1=vrf, 2=pop, 3=swap, 4=xconn, 5=vpls, 6=punt, 7=dup, 8=bier
    int nexthop;
    int port;
    int bridge;
    int vrf;
    int ver;
    int swap;
    int bier[8];
    struct table_head flood;
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
    int tcpmss4;
    int tcpmss6;
    int mpls;
    int nsh;
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
    int command;    // 1=route, 2=punt, 3=mpls1, 4=mpls2, 5=srv6, 6=mysrv4, 7=mysrv6, 8=brsrv, 9=polka
    int nexthop;
    int label1;
    int label2;
    int srv1;
    int srv2;
    int srv3;
    int srv4;
    unsigned char polka[16];
    long pack;
    long byte;
};

struct tree_head route4_table;

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

int route4_masker(void *ptr) {
    struct route4_entry *ntry = ptr;
    return 16 + ntry->mask;
}

int route4_bitter(void *ptr, int pos) {
    struct route4_entry *ntry = ptr;
    if (pos < 16) {
        return ntry->mask & bitVals[16 + pos];
    }
    pos -= 16;
    return ntry->addr & bitVals[pos];
}



struct route6_entry {
    int vrf;
    int mask;
    int addr1;
    int addr2;
    int addr3;
    int addr4;
    int command;    // 1=route, 2=punt, 3=mpls1, 4=mpls2, 5=srv6, 6=mysrv4, 7=mysrv6, 8=brsrv, 9=polka
    int nexthop;
    int label1;
    int label2;
    int srv1;
    int srv2;
    int srv3;
    int srv4;
    unsigned char polka[16];
    long pack;
    long byte;
};

struct tree_head route6_table;

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

int route6_masker(void *ptr) {
    struct route6_entry *ntry = ptr;
    return 16 + ntry->mask;
}

int route6_bitter(void *ptr, int pos) {
    struct route6_entry *ntry = ptr;
    if (pos < 16) {
        return ntry->mask & bitVals[16 + pos];
    }
    pos -= 16;
    if (pos < 32) return ntry->addr1 & bitVals[pos];
    pos -= 32;
    if (pos < 32) return ntry->addr2 & bitVals[pos];
    pos -= 32;
    if (pos < 32) return ntry->addr3 & bitVals[pos];
    pos -= 32;
    return ntry->addr4 & bitVals[pos];
}


struct neigh_entry {
    int id;
    int vrf;
    int command;    // 1=rawip, 2=pppoe, 3=gre4, 4=gre6, 5=l2tp4, 6=l2tp6, 7=ipip4, 8=ipip6, 9=esp4, 10=esp6, 11=ovpn4, 12=ovpn6, 13=wg4. 14=wg6, 15=amt4, 16=amt6
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
    int spi;
    unsigned char encrKeyDat[256];
    unsigned char hashKeyDat[256];
    int encrKeyLen;
    int hashKeyLen;
    int encrBlkLen;
    int hashBlkLen;
    int seq;
    const EVP_CIPHER *encrAlg;
    const EVP_MD *hashAlg;
    EVP_PKEY *hashPkey;
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
    int command;    // 1=port, 2=vpls, 3=route, 4=vxlan4, 5=vxlan6, 6=pckoudp4, 7=pckoudp6, 8=srv4, 9=srv6
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
    int srcPort;
    int trgPort;
    int instance;
    long packRx;
    long byteRx;
    long packTx;
    long byteTx;
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
    int dir; // 1=inacl, 2=outacl, 3=nat, 4=copp, 5=pbr, 6=inqos, 7=outqos, 8=flwspc
    int port;
    struct table_head aces;
    int cmd; // 1=normal, 2=setvrf, 3=sethop, 4=setlab
    int vrf;
    int hop;
    int label;
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
    long pack;
    long byte;
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
    int tosV;
    int tosM;
    int flowV;
    int flowM;
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
    if ((ntry1->tosV & ntry2->tosM) != ntry2->tosV) return 1;
    if ((ntry1->flowV & ntry2->flowM) != ntry2->flowV) return 1;
    if ((ntry1->srcAddr & ntry2->srcMask) != ntry2->srcAddr) return 1;
    if ((ntry1->trgAddr & ntry2->trgMask) != ntry2->trgAddr) return 1;
    return 0;
}


struct acl6_entry {
    int pri;
    int act;
    long pack;
    long byte;
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
    int tosV;
    int tosM;
    int flowV;
    int flowM;
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
    if ((ntry1->tosV & ntry2->tosM) != ntry2->tosV) return 1;
    if ((ntry1->flowV & ntry2->flowM) != ntry2->flowV) return 1;
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
    int act;        // 0=permit, 1=deny
    long pack;
    long byte;
};

struct aclH_entry* search_ace(struct table_head *tab, void *ntry, int matcher(void *, void *),int siz) {
    for (int i=0; i<tab->size; i++) {
        struct aclH_entry *res = table_get(tab, i);
        if (matcher(ntry, res) != 0) continue;
        res->pack++;
        res->byte += siz;
        return res;
    }
    return NULL;
}

int apply_acl(struct table_head *tab, void *ntry, int matcher(void *, void *),int siz) {
    struct aclH_entry *res = search_ace(tab, ntry, matcher, siz);
    if (res == NULL) return 1;
    return res->act;
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
    int command;    // 1=gre, 2=l2tp, 3=vxlan, 4=ip4ip, 5=ip6ip, 6=esp, 7=pckoudp, 8=openvpn, 9=wireguard, 10=amt
    int aclport;
    int spi;
    unsigned char encrKeyDat[256];
    unsigned char hashKeyDat[256];
    int encrKeyLen;
    int hashKeyLen;
    int encrBlkLen;
    int hashBlkLen;
    int seq;
    const EVP_CIPHER *encrAlg;
    const EVP_MD *hashAlg;
    EVP_PKEY *hashPkey;
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
    int command;    // 1=gre, 2=l2tp, 3=vxlan, 4=ip4ip, 5=ip6ip, 6=esp, 7=pckoudp, 8=openvpn, 9=wireguard, 10=amt
    int aclport;
    int spi;
    unsigned char encrKeyDat[256];
    unsigned char hashKeyDat[256];
    int encrKeyLen;
    int hashKeyLen;
    int encrBlkLen;
    int hashBlkLen;
    int seq;
    const EVP_CIPHER *encrAlg;
    const EVP_MD *hashAlg;
    EVP_PKEY *hashPkey;
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
    long packRx;
    long byteRx;
    long packTx;
    long byteTx;
};

struct table_head macsec_table;

int macsec_compare(void *ptr1, void *ptr2) {
    struct pppoe_entry *ntry1 = ptr1;
    struct pppoe_entry *ntry2 = ptr2;
    if (ntry1->port < ntry2->port) return -1;
    if (ntry1->port > ntry2->port) return +1;
    return 0;
}


struct policer_entry {
    int vrf;
    int meter;
    int dir; // 1=in, 2=out, 3=flwspc4, 4=flwspc6
    long allow;
    long avail;
};

struct table_head policer_table;

int policer_compare(void *ptr1, void *ptr2) {
    struct policer_entry *ntry1 = ptr1;
    struct policer_entry *ntry2 = ptr2;
    if (ntry1->vrf < ntry2->vrf) return -1;
    if (ntry1->vrf > ntry2->vrf) return +1;
    if (ntry1->dir < ntry2->dir) return -1;
    if (ntry1->dir > ntry2->dir) return +1;
    if (ntry1->meter < ntry2->meter) return -1;
    if (ntry1->meter > ntry2->meter) return +1;
    return 0;
}


struct monitor_entry {
    int port;
    int target;
    int sample;
    int truncate;
    int packets;
};

struct table_head monitor_table;

int monitor_compare(void *ptr1, void *ptr2) {
    struct monitor_entry *ntry1 = ptr1;
    struct monitor_entry *ntry2 = ptr2;
    if (ntry1->port < ntry2->port) return -1;
    if (ntry1->port > ntry2->port) return +1;
    return 0;
}


struct flood_entry {
    int trg;
    int id;
    int command; // 1=iface, 2=mpls, 3=biermsk, 4=bierset
    int lab;
    int src;
    unsigned char smac[6];
    unsigned char dmac[6];
    int bier[8];
};

int flood_compare(void *ptr1, void *ptr2) {
    struct flood_entry *ntry1 = ptr1;
    struct flood_entry *ntry2 = ptr2;
    if (ntry1->command < ntry2->command) return -1;
    if (ntry1->command > ntry2->command) return +1;
    if (ntry1->trg < ntry2->trg) return -1;
    if (ntry1->trg > ntry2->trg) return +1;
    if (ntry1->id < ntry2->id) return -1;
    if (ntry1->id > ntry2->id) return +1;
    return 0;
}


struct mroute4_entry {
    int vrf;
    int grp;
    int src;
    int ingr;
    int local;
    struct table_head flood;
    long pack;
    long byte;
};

struct table_head mroute4_table;

int mroute4_compare(void *ptr1, void *ptr2) {
    struct mroute4_entry *ntry1 = ptr1;
    struct mroute4_entry *ntry2 = ptr2;
    if (ntry1->vrf < ntry2->vrf) return -1;
    if (ntry1->vrf > ntry2->vrf) return +1;
    if (ntry1->grp < ntry2->grp) return -1;
    if (ntry1->grp > ntry2->grp) return +1;
    if (ntry1->src < ntry2->src) return -1;
    if (ntry1->src > ntry2->src) return +1;
    return 0;
}



struct mroute6_entry {
    int vrf;
    int grp1;
    int grp2;
    int grp3;
    int grp4;
    int src1;
    int src2;
    int src3;
    int src4;
    int ingr;
    int local;
    struct table_head flood;
    long pack;
    long byte;
};

struct table_head mroute6_table;

int mroute6_compare(void *ptr1, void *ptr2) {
    struct mroute6_entry *ntry1 = ptr1;
    struct mroute6_entry *ntry2 = ptr2;
    if (ntry1->vrf < ntry2->vrf) return -1;
    if (ntry1->vrf > ntry2->vrf) return +1;
    if (ntry1->grp1 < ntry2->grp1) return -1;
    if (ntry1->grp1 > ntry2->grp1) return +1;
    if (ntry1->grp2 < ntry2->grp2) return -1;
    if (ntry1->grp2 > ntry2->grp2) return +1;
    if (ntry1->grp3 < ntry2->grp3) return -1;
    if (ntry1->grp3 > ntry2->grp3) return +1;
    if (ntry1->grp4 < ntry2->grp4) return -1;
    if (ntry1->grp4 > ntry2->grp4) return +1;
    if (ntry1->src1 < ntry2->src1) return -1;
    if (ntry1->src1 > ntry2->src1) return +1;
    if (ntry1->src2 < ntry2->src2) return -1;
    if (ntry1->src2 > ntry2->src2) return +1;
    if (ntry1->src3 < ntry2->src3) return -1;
    if (ntry1->src3 > ntry2->src3) return +1;
    if (ntry1->src4 < ntry2->src4) return -1;
    if (ntry1->src4 > ntry2->src4) return +1;
    return 0;
}








void initIface(int port, char *name) {
    ifaceName[port] = malloc(1024);
    strcpy(ifaceName[port], name);
    byteRx[port] = 0;
    packRx[port] = 0;
    byteTx[port] = 0;
    packTx[port] = 0;
    byteDr[port] = 0;
    packDr[port] = 0;
}


int initTables() {
    table_init(&polkaIdx_table, sizeof(struct polkaIdx_entry), &polkaIdx_compare);
    table_init(&polkaPoly_table, sizeof(struct polkaPoly_entry), &polkaPoly_compare);
    table_init(&nsh_table, sizeof(struct nsh_entry), &nsh_compare);
    table_init(&mpls_table, sizeof(struct mpls_entry), &mpls_compare);
    table_init(&portvrf_table, sizeof(struct portvrf_entry), &portvrf_compare);
    tree_init(&route4_table, sizeof(struct route4_entry), &route4_masker, &route4_bitter);
    tree_init(&route6_table, sizeof(struct route6_entry), &route6_masker, &route6_bitter);
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
    table_init(&policer_table, sizeof(struct policer_entry), &policer_compare);
    table_init(&monitor_table, sizeof(struct monitor_entry), &monitor_compare);
    table_init(&mroute4_table, sizeof(struct mroute4_entry), &mroute4_compare);
    table_init(&mroute6_table, sizeof(struct mroute6_entry), &mroute6_compare);
    printf("openssl version: %s\n", OpenSSL_version(OPENSSL_VERSION));
//    if (OSSL_PROVIDER_load(NULL, "legacy") == NULL) return 1;
//    if (OSSL_PROVIDER_load(NULL, "default") == NULL) return 1;
    OpenSSL_add_all_ciphers();
    OpenSSL_add_all_digests();
    OpenSSL_add_all_algorithms();
    RAND_poll();
    return 0;
}
