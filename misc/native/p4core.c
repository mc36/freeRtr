#include "utils.h"
#include "table.h"


#define preBuff 64

int ports = 0;
int cpuport = 0;

unsigned char *ifaceName[maxPorts];
long int byteRx[maxPorts];
long int packRx[maxPorts];
long int byteTx[maxPorts];
long int packTx[maxPorts];
long int byteDr[maxPorts];
long int packDr[maxPorts];




struct mpls_entry {
    int label;
    int command;    // 1=vrf, 2=pop, 3=swap
    int nexthop;
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
    int command;    // 1=vrf, 2=bridge
    int vrf;
    int bridge;
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
    int command;    // 1=route, 2=punt, 3=mpls1, 4=mpls3
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
    int command;    // 1=route, 2=punt, 3=mpls1, 4=mpls3
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
    int port;
    unsigned char smac[6];
    unsigned char dmac[6];
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
    int port;
    unsigned char mac[6];
    long pack;
    long byte;
};

struct table_head bridge_table;

int bridge_compare(void *ptr1, void *ptr2) {
    struct bridge_entry *ntry1 = ptr1;
    struct bridge_entry *ntry2 = ptr2;
    if (ntry1->id < ntry2->id) return -1;
    if (ntry1->id > ntry2->id) return +1;
    if (ntry1->mac[5] < ntry2->mac[5]) return -1;
    if (ntry1->mac[5] > ntry2->mac[5]) return +1;
    if (ntry1->mac[4] < ntry2->mac[4]) return -1;
    if (ntry1->mac[4] > ntry2->mac[4]) return +1;
    if (ntry1->mac[3] < ntry2->mac[3]) return -1;
    if (ntry1->mac[3] > ntry2->mac[3]) return +1;
    if (ntry1->mac[2] < ntry2->mac[2]) return -1;
    if (ntry1->mac[2] > ntry2->mac[2]) return +1;
    if (ntry1->mac[1] < ntry2->mac[1]) return -1;
    if (ntry1->mac[1] > ntry2->mac[1]) return +1;
    if (ntry1->mac[0] < ntry2->mac[0]) return -1;
    if (ntry1->mac[0] > ntry2->mac[0]) return +1;
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
    int dir; // 1=in, 2=out, 3=nat
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
    return 0;
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


#define extract_layer4(ntry)                                    \
    switch (ntry.protV) {                                       \
        case 6:                                                 \
            ntry.srcPortV = get16msb(bufD, bufT + 0);           \
            ntry.trgPortV = get16msb(bufD, bufT + 2);           \
            break;                                              \
        case 17:                                                \
            ntry.srcPortV = get16msb(bufD, bufT + 0);           \
            ntry.trgPortV = get16msb(bufD, bufT + 2);           \
            break;                                              \
        default:                                                \
            ntry.srcPortV = 0;                                  \
            ntry.trgPortV = 0;                                  \
            break;                                              \
    }


#define update_chksum(ofs, val)                                 \
    sum = get16lsb(bufD, ofs);                                  \
    sum -= val;                                                 \
    sum = (sum & 0xffff) + (sum >> 16);                         \
    put16lsb(bufD, ofs, sum);



#define accumulate_sum(sum, val, mul)                           \
    put32msb(buf2, 0, val);                                     \
    sum += mul*get16lsb(buf2, 0);                               \
    sum += mul*get16lsb(buf2, 2);


#define update_layer4(ntry)                                     \
    switch (ntry->prot) {                                       \
        case 6:                                                 \
            put16msb(bufD, bufT + 0, ntry->nSrcPort);           \
            put16msb(bufD, bufT + 2, ntry->nTrgPort);           \
            update_chksum(bufT + 16, ntry->sum4);               \
            break;                                              \
        case 17:                                                \
            put16msb(bufD, bufT + 0, ntry->nSrcPort);           \
            put16msb(bufD, bufT + 2, ntry->nTrgPort);           \
            update_chksum(bufT + 6, ntry->sum4);                \
            break;                                              \
    }



void send2port(unsigned char *bufD, int bufS, int port) {
    sendpack(bufD, bufS, port);
    packTx[port]++;
    byteTx[port] += bufS;
}



void send2cpu(unsigned char *bufD, int bufS, int port) {
    put16msb(bufD, preBuff - 2, port);
    send2port(&bufD[preBuff - 2], bufS + 2, cpuport);
}


void processCpuPack(unsigned char* bufD, int bufS) {
    struct vlan_entry vlan_ntry;
    struct vlan_entry *vlan_res;
    int index;
    int prt;
    packRx[cpuport]++;
    byteRx[cpuport] += bufS;
    prt = get16msb(bufD, preBuff);
    if (prt >= ports) return;
    send2port(&bufD[preBuff + 2], bufS - 2, prt);
    if (get16msb(bufD, preBuff + 14) != 0x8100) return;
    vlan_ntry.port = prt;
    vlan_ntry.vlan = get16msb(bufD, preBuff + 16) & 0xfff;
    index = table_find(&vlanin_table, &vlan_ntry);
    if (index < 0) return;
    vlan_res = table_get(&vlanout_table, index);
    vlan_res->pack++;
    vlan_res->byte += bufS;
}


#ifdef basicLoop


void processDataPacket(unsigned char *bufD, int bufS, int port) {
    struct vlan_entry vlan_ntry;
    struct vlan_entry *vlan_res;
    int index;
    packRx[port]++;
    byteRx[port] += bufS;
    send2cpu(bufD, bufS, port);
    if (get16msb(bufD, preBuff + 12) != 0x8100) return;
    vlan_ntry.port = port;
    vlan_ntry.vlan = get16msb(bufD, preBuff + 14) & 0xfff;
    index = table_find(&vlanin_table, &vlan_ntry);
    if (index < 0) return;
    vlan_res = table_get(&vlanin_table, index);
    vlan_res->pack++;
    vlan_res->byte += bufS;
}



#else


int masks[] = {
    0x00000000,
    0x80000000, 0xc0000000, 0xe0000000, 0xf0000000,
    0xf8000000, 0xfc000000, 0xfe000000, 0xff000000,
    0xff800000, 0xffc00000, 0xffe00000, 0xfff00000,
    0xfff80000, 0xfffc0000, 0xfffe0000, 0xffff0000,
    0xffff8000, 0xffffc000, 0xffffe000, 0xfffff000,
    0xfffff800, 0xfffffc00, 0xfffffe00, 0xffffff00,
    0xffffff80, 0xffffffc0, 0xffffffe0, 0xfffffff0,
    0xfffffff8, 0xfffffffc, 0xfffffffe, 0xffffffff
};


void processDataPacket(unsigned char *bufD, int bufS, int port) {
    unsigned char buf2[preBuff];
    int bufP;
    int bufT;
    int ethtyp;
    struct mpls_entry mpls_ntry;
    struct portvrf_entry portvrf_ntry;
    struct route4_entry route4_ntry;
    struct route6_entry route6_ntry;
    struct neigh_entry neigh_ntry;
    struct vlan_entry vlan_ntry;
    struct bridge_entry bridge_ntry;
    struct acls_entry acls_ntry;
    struct acl4_entry acl4_ntry;
    struct acl6_entry acl6_ntry;
    struct nat4_entry nat4_ntry;
    struct nat6_entry nat6_ntry;
    struct mpls_entry *mpls_res;
    struct portvrf_entry *portvrf_res;
    struct route4_entry *route4_res;
    struct route6_entry *route6_res;
    struct neigh_entry *neigh_res;
    struct vlan_entry *vlan_res;
    struct bridge_entry *bridge_res;
    struct acls_entry *acls_res;
    struct nat4_entry *nat4_res;
    struct nat6_entry *nat6_res;
    int index;
    int label;
    int prt;
    int sum;
    int ttl;
    bufP = preBuff;
    packRx[port]++;
    byteRx[port] += bufS;
    bufP += 6 * 2; // dmac, smac
    prt = port;
ethtyp_rx:
    ethtyp = get16msb(bufD, bufP);
    bufP += 2;
    switch (ethtyp) {
        case 0x8847:
mpls_rx:
            label = get32msb(bufD, bufP);
            ttl = (label & 0xff) - 1;
            if (ttl <= 1) {
                packDr[port]++;
                byteDr[port] += bufS;
                return;
            }
            bufP += 4;
            mpls_ntry.label = (label >> 12) & 0xfffff;
            index = table_find(&mpls_table, &mpls_ntry);
            if (index < 0) {
                packDr[port]++;
                byteDr[port] += bufS;
                return;
            }
            mpls_res = table_get(&mpls_table, index);
            mpls_res->pack++;
            mpls_res->byte += bufS;
            switch (mpls_res->command) {
                case 1: // route
                    route4_ntry.vrf = mpls_res->vrf;
                    route6_ntry.vrf = mpls_res->vrf;
                    if ((label & 0x100) == 0) goto mpls_rx;
                    switch (mpls_res->ver) {
                        case 4:
                            ethtyp = 0x800;
                            goto ipv4_rx;
                        case 6:
                            ethtyp = 0x86dd;
                            goto ipv6_rx;
                        default:
                            ethtyp = 0;
                            break;
                    }
                    packDr[port]++;
                    byteDr[port] += bufS;
                    return;
                case 2: // pop
                    switch (mpls_res->ver) {
                        case 4:
                            ethtyp = 0x800;
                            break;
                        case 6:
                            ethtyp = 0x86dd;
                            break;
                        default:
                            ethtyp = 0;
                            break;
                    }
                    neigh_ntry.id = mpls_res->nexthop;
                    goto ethtyp_tx;
                    return;
                case 3: // swap
                    bufP -= 4;
                    label = (label & 0xf00) | ttl | (mpls_res->swap << 12);
                    put32msb(bufD, bufP, label);
                    neigh_ntry.id = mpls_res->nexthop;
ethtyp_tx:
                    bufP -= 2;
                    put16msb(bufD, bufP, ethtyp);
                    index = table_find(&neigh_table, &neigh_ntry);
                    if (index < 0) {
                        packDr[port]++;
                        byteDr[port] += bufS;
                        return;
                    }
                    neigh_res = table_get(&neigh_table, index);
                    neigh_res->pack++;
                    neigh_res->byte += bufS;
                    prt = neigh_res->port;
vlan_tx:
                    vlan_ntry.id = prt;
                    index = table_find(&vlanout_table, &vlan_ntry);
                    if (index >= 0) {
                        vlan_res = table_get(&vlanout_table, index);
                        bufP -= 2;
                        put16msb(bufD, bufP, vlan_res->vlan);
                        bufP -= 2;
                        put16msb(bufD, bufP, 0x8100);
                        prt = vlan_res->port;
                        vlan_res->pack++;
                        vlan_res->byte += bufS;
                    }
                    if (prt >= ports) {
                        packDr[port]++;
                        byteDr[port] += bufS;
                        return;
                    }
                    bufP -= 6;
                    memmove(&bufD[bufP], &neigh_res->smac, 6);
                    bufP -= 6;
                    memmove(&bufD[bufP], &neigh_res->dmac, 6);
                    send2port(&bufD[bufP], bufS - bufP + preBuff, prt);
                    return;
                 default:
                    return;
            }
            return;
        case 0x8100:
            vlan_ntry.port = prt;
            vlan_ntry.vlan = get16msb(bufD, bufP) & 0xfff;
            bufP += 2;
            index = table_find(&vlanin_table, &vlan_ntry);
            if (index < 0) {
                packDr[port]++;
                byteDr[port] += bufS;
                return;
            }
            vlan_res = table_get(&vlanin_table, index);
            prt = vlan_res->id;
            vlan_res->pack++;
            vlan_res->byte += bufS;
            goto ethtyp_rx;
            return;
        case 0x800:
            portvrf_ntry.port = prt;
            index = table_find(&portvrf_table, &portvrf_ntry);
            if (index < 0) {
                packDr[port]++;
                byteDr[port] += bufS;
                return;
            }
            portvrf_res = table_get(&portvrf_table, index);
            if (portvrf_res->command == 2) goto bridge;
            route4_ntry.vrf = portvrf_res->vrf;
ipv4_rx:
            acl4_ntry.protV = bufD[bufP + 9];
            acl4_ntry.srcAddr = get32msb(bufD, bufP + 12);
            acl4_ntry.trgAddr = route4_ntry.addr = get32msb(bufD, bufP + 16);
            ttl = bufD[bufP + 8] - 1;
            if (ttl <= 1) {
                packDr[port]++;
                byteDr[port] += bufS;
                return;
            }
            bufD[bufP + 8] = ttl;
            update_chksum(bufP + 10, -1);
            bufT = bufP + 20;
            extract_layer4(acl4_ntry);
            acls_ntry.ver = 4;
            acls_ntry.dir = 1;
            acls_ntry.port = prt;
            index = table_find(&acls_table, &acls_ntry);
            if (index >= 0) {
                acls_res = table_get(&acls_table, index);
                if (apply_acl(&acls_res->aces, &acl4_ntry, &acl4_matcher) != 0) {
                    packDr[port]++;
                    byteDr[port] += bufS;
                    return;
                }
            }            
            acls_ntry.dir = 3;
            acls_ntry.port = route4_ntry.vrf;
            index = table_find(&acls_table, &acls_ntry);
            if (index >= 0) {
                acls_res = table_get(&acls_table, index);
                nat4_ntry.vrf = route4_ntry.vrf;
                nat4_ntry.prot = acl4_ntry.protV;
                nat4_ntry.oSrcAddr = acl4_ntry.srcAddr;
                nat4_ntry.oTrgAddr = acl4_ntry.trgAddr;
                nat4_ntry.oSrcPort = acl4_ntry.srcPortV;
                nat4_ntry.oTrgPort = acl4_ntry.trgPortV;
                index = table_find(&nat4_table, &nat4_ntry);
                if (index < 0) {
                    if (apply_acl(&acls_res->aces, &acl4_ntry, &acl4_matcher) == 0) goto cpu;
                    goto ipv4_rou;
                }
                nat4_res = table_get(&nat4_table, index);
                nat4_res->pack++;
                nat4_res->byte += bufS;
                acl4_ntry.srcAddr = nat4_res->nSrcAddr;
                acl4_ntry.trgAddr = route4_ntry.addr = nat4_res->nTrgAddr;
                acl4_ntry.srcPortV = nat4_res->nSrcPort;
                acl4_ntry.trgPortV = nat4_res->nTrgPort;
                put32msb(bufD, bufP + 12, acl4_ntry.srcAddr);
                put32msb(bufD, bufP + 16, acl4_ntry.trgAddr);
                update_chksum(bufP + 10, nat4_res->sum3);
                update_layer4(nat4_res);
            }            
ipv4_rou:
            for (int i = 32; i >= 0; i--) {
                route4_ntry.mask = i;
                route4_ntry.addr &= masks[i];
                index = table_find(&route4_table, &route4_ntry);
                if (index < 0) continue;
                route4_res = table_get(&route4_table, index);
                route4_res->pack++;
                route4_res->byte += bufS;
                switch (route4_res->command) {
                    case 1: // route
                        neigh_ntry.id = route4_res->nexthop;
                        bufP -= 2;
                        put16msb(bufD, bufP, ethtyp);
                        index = table_find(&neigh_table, &neigh_ntry);
                        if (index < 0) {
                            packDr[port]++;
                            byteDr[port] += bufS;
                            return;
                        }
                        neigh_res = table_get(&neigh_table, index);
                        neigh_res->pack++;
                        neigh_res->byte += bufS;
                        prt = neigh_res->port;
                        acls_ntry.dir = 2;
                        acls_ntry.port = prt;
                        index = table_find(&acls_table, &acls_ntry);
                        if (index >= 0) {
                            acls_res = table_get(&acls_table, index);
                            if (apply_acl(&acls_res->aces, &acl4_ntry, &acl4_matcher) != 0) {
                                packDr[port]++;
                                byteDr[port] += bufS;
                                return;
                            }
                        }            
                        goto vlan_tx;
                    case 2: // punt
                        goto cpu;
                    case 3: // mpls1
                        ethtyp = 0x8847;
                        bufP -= 4;
                        label = 0x100 | ttl | (route4_res->label1 << 12);
                        put32msb(bufD, bufP, label);
                        neigh_ntry.id = route4_res->nexthop;
                        goto ethtyp_tx;
                    case 4: // mpls2
                        ethtyp = 0x8847;
                        bufP -= 4;
                        label = 0x100 | ttl | (route4_res->label2 << 12);
                        put32msb(bufD, bufP, label);
                        bufP -= 4;
                        label = ttl | (route4_res->label1 << 12);
                        put32msb(bufD, bufP, label);
                        neigh_ntry.id = route4_res->nexthop;
                        goto ethtyp_tx;
                }
            }
            packDr[port]++;
            byteDr[port] += bufS;
            return;
        case 0x86dd:
            portvrf_ntry.port = prt;
            index = table_find(&portvrf_table, &portvrf_ntry);
            if (index < 0) {
                packDr[port]++;
                byteDr[port] += bufS;
                return;
            }
            portvrf_res = table_get(&portvrf_table, index);
            if (portvrf_res->command == 2) goto bridge;
            route6_ntry.vrf = portvrf_res->vrf;
ipv6_rx:
            acl6_ntry.protV = bufD[bufP + 6];
            acl6_ntry.srcAddr1 = get32msb(bufD, bufP + 8);
            acl6_ntry.srcAddr2 = get32msb(bufD, bufP + 12);
            acl6_ntry.srcAddr3 = get32msb(bufD, bufP + 16);
            acl6_ntry.srcAddr4 = get32msb(bufD, bufP + 20);
            acl6_ntry.trgAddr1 = route6_ntry.addr1 = get32msb(bufD, bufP + 24);
            acl6_ntry.trgAddr2 = route6_ntry.addr2 = get32msb(bufD, bufP + 28);
            acl6_ntry.trgAddr3 = route6_ntry.addr3 = get32msb(bufD, bufP + 32);
            acl6_ntry.trgAddr4 = route6_ntry.addr4 = get32msb(bufD, bufP + 36);
            ttl = bufD[bufP + 7] - 1;
            if (ttl <= 1) {
                packDr[port]++;
                byteDr[port] += bufS;
                return;
            }
            bufD[bufP + 7] = ttl;
            bufT = bufP + 40;
            extract_layer4(acl6_ntry);
            acls_ntry.ver = 6;
            acls_ntry.dir = 1;
            acls_ntry.port = prt;
            index = table_find(&acls_table, &acls_ntry);
            if (index >= 0) {
                acls_res = table_get(&acls_table, index);
                if (apply_acl(&acls_res->aces, &acl6_ntry, &acl6_matcher) != 0) {
                    packDr[port]++;
                    byteDr[port] += bufS;
                    return;
                }
            }            
            acls_ntry.dir = 3;
            acls_ntry.port = route6_ntry.vrf;
            index = table_find(&acls_table, &acls_ntry);
            if (index >= 0) {
                acls_res = table_get(&acls_table, index);
                nat6_ntry.vrf = route6_ntry.vrf;
                nat6_ntry.prot = acl6_ntry.protV;
                nat6_ntry.oSrcAddr1 = acl6_ntry.srcAddr1;
                nat6_ntry.oSrcAddr2 = acl6_ntry.srcAddr2;
                nat6_ntry.oSrcAddr3 = acl6_ntry.srcAddr3;
                nat6_ntry.oSrcAddr4 = acl6_ntry.srcAddr4;
                nat6_ntry.oTrgAddr1 = acl6_ntry.trgAddr1;
                nat6_ntry.oTrgAddr2 = acl6_ntry.trgAddr2;
                nat6_ntry.oTrgAddr3 = acl6_ntry.trgAddr3;
                nat6_ntry.oTrgAddr4 = acl6_ntry.trgAddr4;
                nat6_ntry.oSrcPort = acl6_ntry.srcPortV;
                nat6_ntry.oTrgPort = acl6_ntry.trgPortV;
                index = table_find(&nat6_table, &nat6_ntry);
                if (index < 0) {
                    if (apply_acl(&acls_res->aces, &acl6_ntry, &acl6_matcher) == 0) goto cpu;
                    goto ipv6_rou;
                }
                nat6_res = table_get(&nat6_table, index);
                nat6_res->pack++;
                nat6_res->byte += bufS;
                acl6_ntry.srcAddr1 = nat6_res->nSrcAddr1;
                acl6_ntry.srcAddr2 = nat6_res->nSrcAddr2;
                acl6_ntry.srcAddr3 = nat6_res->nSrcAddr3;
                acl6_ntry.srcAddr4 = nat6_res->nSrcAddr4;
                acl6_ntry.trgAddr1 = route6_ntry.addr1 = nat6_res->nTrgAddr1;
                acl6_ntry.trgAddr2 = route6_ntry.addr2 = nat6_res->nTrgAddr2;
                acl6_ntry.trgAddr3 = route6_ntry.addr3 = nat6_res->nTrgAddr3;
                acl6_ntry.trgAddr4 = route6_ntry.addr4 = nat6_res->nTrgAddr4;
                acl6_ntry.srcPortV = nat6_res->nSrcPort;
                acl6_ntry.trgPortV = nat6_res->nTrgPort;
                put32msb(bufD, bufP + 8, acl6_ntry.srcAddr1);
                put32msb(bufD, bufP + 12, acl6_ntry.srcAddr2);
                put32msb(bufD, bufP + 16, acl6_ntry.srcAddr3);
                put32msb(bufD, bufP + 20, acl6_ntry.srcAddr4);
                put32msb(bufD, bufP + 24, acl6_ntry.trgAddr1);
                put32msb(bufD, bufP + 28, acl6_ntry.trgAddr2);
                put32msb(bufD, bufP + 32, acl6_ntry.trgAddr3);
                put32msb(bufD, bufP + 36, acl6_ntry.trgAddr4);
                update_layer4(nat6_res);
            }            
ipv6_rou:
            for (int i = 32; i >= 0; i--) {
                route6_ntry.mask = 96 + i;
                route6_ntry.addr4 &= masks[i];
                index = table_find(&route6_table, &route6_ntry);
                if (index < 0) continue;
                goto ipv6_hit;
            }
            for (int i = 32; i >= 0; i--) {
                route6_ntry.mask = 64 + i;
                route6_ntry.addr3 &= masks[i];
                index = table_find(&route6_table, &route6_ntry);
                if (index < 0) continue;
                goto ipv6_hit;
            }
            for (int i = 32; i >= 0; i--) {
                route6_ntry.mask = 32 + i;
                route6_ntry.addr2 &= masks[i];
                index = table_find(&route6_table, &route6_ntry);
                if (index < 0) continue;
                goto ipv6_hit;
            }
            for (int i = 32; i >= 0; i--) {
                route6_ntry.mask = i;
                route6_ntry.addr1 &= masks[i];
                index = table_find(&route6_table, &route6_ntry);
                if (index < 0) continue;
ipv6_hit:
                route6_res = table_get(&route6_table, index);
                route6_res->pack++;
                route6_res->byte += bufS;
                switch (route6_res->command) {
                    case 1: // route
                        neigh_ntry.id = route6_res->nexthop;
                        bufP -= 2;
                        put16msb(bufD, bufP, ethtyp);
                        index = table_find(&neigh_table, &neigh_ntry);
                        if (index < 0) {
                            packDr[port]++;
                            byteDr[port] += bufS;
                            return;
                        }
                        neigh_res = table_get(&neigh_table, index);
                        neigh_res->pack++;
                        neigh_res->byte += bufS;
                        prt = neigh_res->port;
                        acls_ntry.dir = 2;
                        acls_ntry.port = prt;
                        index = table_find(&acls_table, &acls_ntry);
                        if (index >= 0) {
                            acls_res = table_get(&acls_table, index);
                            if (apply_acl(&acls_res->aces, &acl6_ntry, &acl6_matcher) != 0) {
                                packDr[port]++;
                                byteDr[port] += bufS;
                                return;
                            }
                        }            
                        goto vlan_tx;
                    case 2: // punt
                        goto cpu;
                    case 3: // mpls1
                        ethtyp = 0x8847;
                        bufP -= 4;
                        label = 0x100 | ttl | (route6_res->label1 << 12);
                        put32msb(bufD, bufP, label);
                        neigh_ntry.id = route6_res->nexthop;
                        goto ethtyp_tx;
                    case 4: // mpls2
                        ethtyp = 0x8847;
                        bufP -= 4;
                        label = 0x100 | ttl | (route6_res->label2 << 12);
                        put32msb(bufD, bufP, label);
                        bufP -= 4;
                        label = ttl | (route6_res->label1 << 12);
                        put32msb(bufD, bufP, label);
                        neigh_ntry.id = route6_res->nexthop;
                        goto ethtyp_tx;
                }
            }
            packDr[port]++;
            byteDr[port] += bufS;
            return;
        case 0x65580000:
bridge:
            bridge_ntry.id = portvrf_res->bridge;
            memmove(&bridge_ntry.mac, &bufD[preBuff], 6);
            memmove(&buf2[0], &bufD[preBuff], 12);
            index = table_find(&bridge_table, &bridge_ntry);
            if (index < 0) goto cpu;
            bridge_res = table_get(&bridge_table, index);
            bridge_res->pack++;
            bridge_res->byte += bufS;
            bufP -= 2;
            prt = bridge_res->port;
            vlan_ntry.id = prt;
            index = table_find(&vlanout_table, &vlan_ntry);
            if (index >= 0) {
                vlan_res = table_get(&vlanout_table, index);
                bufP -= 2;
                put16msb(bufD, bufP, vlan_res->vlan);
                bufP -= 2;
                put16msb(bufD, bufP, 0x8100);
                prt = vlan_res->port;
                vlan_res->pack++;
                vlan_res->byte += bufS;
            }
            if (prt >= ports) {
                packDr[port]++;
                byteDr[port] += bufS;
                return;
            }
            bufP -= 12;
            memmove(&bufD[bufP], &buf2[0], 12);
            send2port(&bufD[bufP], bufS - bufP + preBuff, prt);
            return;
        default:
cpu:
            send2cpu(bufD, bufS, port);
            return;
    }
}

#endif




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
}




void str2mac(unsigned char *dst, unsigned char *src) {
    sscanf(src, "%hhx:%hhx:%hhx:%hhx:%hhx:%hhx", &dst[0], &dst[1], &dst[2], &dst[3], &dst[4], &dst[5]);
}

void mac2str(unsigned char *src, unsigned char *dst) {
    snprintf(dst, 128, "%02x:%02x:%02x:%02x:%02x:%02x", src[0], src[1], src[2], src[3], src[4], src[5]);
}

void readAcl4(struct acl4_entry *acl4_ntry, unsigned char**arg) {
    unsigned char buf2[1024];
    acl4_ntry->pri = atoi(arg[3]);
    acl4_ntry->act = strcmp(arg[4], "permit");
    acl4_ntry->protV = atoi(arg[5]);
    acl4_ntry->protM = atoi(arg[6]);
    inet_pton(AF_INET, arg[7], buf2);
    acl4_ntry->srcAddr = get32msb(buf2, 0);
    inet_pton(AF_INET, arg[8], buf2);
    acl4_ntry->srcMask = get32msb(buf2, 0);
    inet_pton(AF_INET, arg[9], buf2);
    acl4_ntry->trgAddr = get32msb(buf2, 0);
    inet_pton(AF_INET, arg[10], buf2);
    acl4_ntry->trgMask = get32msb(buf2, 0);
    acl4_ntry->srcPortV = atoi(arg[11]);
    acl4_ntry->srcPortM = atoi(arg[12]);
    acl4_ntry->trgPortV = atoi(arg[13]);
    acl4_ntry->trgPortM = atoi(arg[14]);
}

void readAcl6(struct acl6_entry *acl6_ntry, unsigned char**arg) {
    unsigned char buf2[1024];
    acl6_ntry->pri = atoi(arg[3]);
    acl6_ntry->act = strcmp(arg[4], "permit");
    acl6_ntry->protV = atoi(arg[5]);
    acl6_ntry->protM = atoi(arg[6]);
    inet_pton(AF_INET6, arg[7], buf2);
    acl6_ntry->srcAddr1 = get32msb(buf2, 0);
    acl6_ntry->srcAddr2 = get32msb(buf2, 4);
    acl6_ntry->srcAddr3 = get32msb(buf2, 8);
    acl6_ntry->srcAddr4 = get32msb(buf2, 12);
    inet_pton(AF_INET6, arg[8], buf2);
    acl6_ntry->srcMask1 = get32msb(buf2, 0);
    acl6_ntry->srcMask2 = get32msb(buf2, 4);
    acl6_ntry->srcMask3 = get32msb(buf2, 8);
    acl6_ntry->srcMask4 = get32msb(buf2, 12);
    inet_pton(AF_INET6, arg[9], buf2);
    acl6_ntry->trgAddr1 = get32msb(buf2, 0);
    acl6_ntry->trgAddr2 = get32msb(buf2, 4);
    acl6_ntry->trgAddr3 = get32msb(buf2, 8);
    acl6_ntry->trgAddr4 = get32msb(buf2, 12);
    inet_pton(AF_INET6, arg[10], buf2);
    acl6_ntry->trgMask1 = get32msb(buf2, 0);
    acl6_ntry->trgMask2 = get32msb(buf2, 4);
    acl6_ntry->trgMask3 = get32msb(buf2, 8);
    acl6_ntry->trgMask4 = get32msb(buf2, 12);
    acl6_ntry->srcPortV = atoi(arg[11]);
    acl6_ntry->srcPortM = atoi(arg[12]);
    acl6_ntry->trgPortV = atoi(arg[13]);
    acl6_ntry->trgPortM = atoi(arg[14]);
}

int doOneCommand(unsigned char* buf) {
    unsigned char buf2[1024];
    unsigned char* arg[128];
    int cnt;
    cnt = 0;
    arg[0] = &buf[0];
    int i = 0;
    int o = 0;
    for (;;) {
        switch (buf[i]) {
            case 0:
            case 10:
            case 13:
                o = 1;
            case ' ':
            case '/':
            case '_':
                buf[i] = 0;
                cnt++;
                arg[cnt] = &buf[i + 1];
                break;
        }
        if (o > 0) break;
        i++;
    }
    printf("rx: ");
    for (int i=0; i < cnt; i++) printf("'%s' ",arg[i]);
    printf("\n");
    int del = strcmp(arg[1], "del");
    struct mpls_entry mpls_ntry;
    memset(&mpls_ntry, 0, sizeof(mpls_ntry));
    struct portvrf_entry portvrf_ntry;
    memset(&portvrf_ntry, 0, sizeof(portvrf_ntry));
    struct route4_entry route4_ntry;
    memset(&route4_ntry, 0, sizeof(route4_ntry));
    struct route6_entry route6_ntry;
    memset(&route6_ntry, 0, sizeof(route6_ntry));
    struct neigh_entry neigh_ntry;
    memset(&neigh_ntry, 0, sizeof(neigh_ntry));
    struct vlan_entry vlan_ntry;
    memset(&vlan_ntry, 0, sizeof(vlan_ntry));
    struct bridge_entry bridge_ntry;
    memset(&bridge_ntry, 0, sizeof(bridge_ntry));
    struct acls_entry acls_ntry;
    memset(&acls_ntry, 0, sizeof(acls_ntry));
    struct acls_entry *acls_res;
    struct acl4_entry acl4_ntry;
    memset(&acl4_ntry, 0, sizeof(acl4_ntry));
    struct acl6_entry acl6_ntry;
    memset(&acl6_ntry, 0, sizeof(acl6_ntry));
    struct nat4_entry nat4_ntry;
    memset(&nat4_ntry, 0, sizeof(nat4_ntry));
    struct nat6_entry nat6_ntry;
    memset(&nat6_ntry, 0, sizeof(nat6_ntry));
    int index = 0;
    if (strcmp(arg[0], "quit") == 0) {
        return 1;
    }
    if (strcmp(arg[0], "mylabel4") == 0) {
        mpls_ntry.label = atoi(arg[2]);
        mpls_ntry.vrf = atoi(arg[3]);
        mpls_ntry.ver = 4;
        mpls_ntry.command = 1;
        if (del == 0) table_del(&mpls_table, &mpls_ntry); else table_add(&mpls_table, &mpls_ntry);
        return 0;
    }
    if (strcmp(arg[0], "mylabel6") == 0) {
        mpls_ntry.label = atoi(arg[2]);
        mpls_ntry.vrf = atoi(arg[3]);
        mpls_ntry.ver = 6;
        mpls_ntry.command = 1;
        if (del == 0) table_del(&mpls_table, &mpls_ntry); else table_add(&mpls_table, &mpls_ntry);
        return 0;
    }
    if (strcmp(arg[0], "unlabel4") == 0) {
        mpls_ntry.label = atoi(arg[2]);
        mpls_ntry.nexthop = atoi(arg[3]);
        mpls_ntry.ver = 4;
        mpls_ntry.command = 2;
        if (del == 0) table_del(&mpls_table, &mpls_ntry); else table_add(&mpls_table, &mpls_ntry);
        return 0;
    }
    if (strcmp(arg[0], "unlabel6") == 0) {
        mpls_ntry.label = atoi(arg[2]);
        mpls_ntry.nexthop = atoi(arg[3]);
        mpls_ntry.ver = 6;
        mpls_ntry.command = 2;
        if (del == 0) table_del(&mpls_table, &mpls_ntry); else table_add(&mpls_table, &mpls_ntry);
        return 0;
    }
    if (strcmp(arg[0], "label4") == 0) {
        mpls_ntry.label = atoi(arg[2]);
        mpls_ntry.nexthop = atoi(arg[3]);
        mpls_ntry.swap = atoi(arg[5]);
        mpls_ntry.ver = 4;
        mpls_ntry.command = 3;
        if (del == 0) table_del(&mpls_table, &mpls_ntry); else table_add(&mpls_table, &mpls_ntry);
        return 0;
    }
    if (strcmp(arg[0], "label6") == 0) {
        mpls_ntry.label = atoi(arg[2]);
        mpls_ntry.nexthop = atoi(arg[3]);
        mpls_ntry.swap = atoi(arg[5]);
        mpls_ntry.ver = 6;
        mpls_ntry.command = 3;
        if (del == 0) table_del(&mpls_table, &mpls_ntry); else table_add(&mpls_table, &mpls_ntry);
        return 0;
    }
    if (strcmp(arg[0], "portvrf") == 0) {
        portvrf_ntry.command = 1;
        portvrf_ntry.port = atoi(arg[2]);
        portvrf_ntry.vrf = atoi(arg[3]);
        if (del == 0) table_del(&portvrf_table, &portvrf_ntry); else table_add(&portvrf_table, &portvrf_ntry);
        return 0;
    }
    if (strcmp(arg[0], "portbridge") == 0) {
        portvrf_ntry.command = 2;
        portvrf_ntry.port = atoi(arg[2]);
        portvrf_ntry.bridge = atoi(arg[3]);
        if (del == 0) table_del(&portvrf_table, &portvrf_ntry); else table_add(&portvrf_table, &portvrf_ntry);
        return 0;
    }
    if (strcmp(arg[0], "bridgemac") == 0) {
        bridge_ntry.id = atoi(arg[2]);
        str2mac(bridge_ntry.mac, arg[3]);
        bridge_ntry.port = atoi(arg[4]);
        if (del == 0) table_del(&bridge_table, &bridge_ntry); else table_add(&bridge_table, &bridge_ntry);
        return 0;
    }
    if (strcmp(arg[0], "portvlan") == 0) {
        vlan_ntry.id= atoi(arg[2]);
        vlan_ntry.port = atoi(arg[3]);
        vlan_ntry.vlan = atoi(arg[4]);
        if (del == 0) table_del(&vlanin_table, &vlan_ntry); else table_add(&vlanin_table, &vlan_ntry);
        if (del == 0) table_del(&vlanout_table, &vlan_ntry); else table_add(&vlanout_table, &vlan_ntry);
        return 0;
    }
    if (strcmp(arg[0], "myaddr4") == 0) {
        inet_pton(AF_INET, arg[2], buf2);
        route4_ntry.addr = get32msb(buf2, 0);
        route4_ntry.mask = atoi(arg[3]);
        route4_ntry.vrf = atoi(arg[5]);
        route4_ntry.command = 2;
        if (del == 0) table_del(&route4_table, &route4_ntry); else table_add(&route4_table, &route4_ntry);
        return 0;
    }
    if (strcmp(arg[0], "route4") == 0) {
        inet_pton(AF_INET, arg[2], buf2);
        route4_ntry.addr = get32msb(buf2, 0);
        route4_ntry.mask = atoi(arg[3]);
        route4_ntry.nexthop = atoi(arg[4]);
        route4_ntry.vrf = atoi(arg[6]);
        route4_ntry.command = 1;
        if (del == 0) table_del(&route4_table, &route4_ntry); else table_add(&route4_table, &route4_ntry);
        return 0;
    }
    if (strcmp(arg[0], "labroute4") == 0) {
        inet_pton(AF_INET, arg[2], buf2);
        route4_ntry.addr = get32msb(buf2, 0);
        route4_ntry.mask = atoi(arg[3]);
        route4_ntry.nexthop = atoi(arg[4]);
        route4_ntry.vrf = atoi(arg[6]);
        route4_ntry.label1 = atoi(arg[7]);
        route4_ntry.command = 3;
        if (del == 0) table_del(&route4_table, &route4_ntry); else table_add(&route4_table, &route4_ntry);
        return 0;
    }
    if (strcmp(arg[0], "vpnroute4") == 0) {
        inet_pton(AF_INET, arg[2], buf2);
        route4_ntry.addr = get32msb(buf2, 0);
        route4_ntry.mask = atoi(arg[3]);
        route4_ntry.nexthop = atoi(arg[4]);
        route4_ntry.vrf = atoi(arg[6]);
        route4_ntry.label1 = atoi(arg[7]);
        route4_ntry.label2 = atoi(arg[8]);
        route4_ntry.command = 4;
        if (del == 0) table_del(&route4_table, &route4_ntry); else table_add(&route4_table, &route4_ntry);
        return 0;
    }
    if (strcmp(arg[0], "neigh4") == 0) {
        route4_ntry.nexthop = atoi(arg[2]);
        inet_pton(AF_INET, arg[3], buf2);
        route4_ntry.addr = get32msb(buf2, 0);
        route4_ntry.mask = 32;
        route4_ntry.vrf = atoi(arg[5]);
        route4_ntry.command = 1;
        neigh_ntry.id = route4_ntry.nexthop;
        neigh_ntry.vrf = route4_ntry.vrf;
        neigh_ntry.port = atoi(arg[7]);
        str2mac(neigh_ntry.dmac, arg[4]);
        str2mac(neigh_ntry.smac, arg[6]);
        if (del == 0) table_del(&route4_table, &route4_ntry); else table_add(&route4_table, &route4_ntry);
        if (del == 0) table_del(&neigh_table, &neigh_ntry); else table_add(&neigh_table, &neigh_ntry);
        return 0;
    }
    if (strcmp(arg[0], "myaddr6") == 0) {
        inet_pton(AF_INET6, arg[2], buf);
        route6_ntry.addr1 = get32msb(buf, 0);
        route6_ntry.addr2 = get32msb(buf, 4);
        route6_ntry.addr3 = get32msb(buf, 8);
        route6_ntry.addr4 = get32msb(buf, 12);
        route6_ntry.mask = atoi(arg[3]);
        route6_ntry.vrf = atoi(arg[5]);
        route6_ntry.command = 2;
        if (del == 0) table_del(&route6_table, &route6_ntry); else table_add(&route6_table, &route6_ntry);
        return 0;
    }
    if (strcmp(arg[0], "route6") == 0) {
        inet_pton(AF_INET6, arg[2], buf2);
        route6_ntry.addr1 = get32msb(buf2, 0);
        route6_ntry.addr2 = get32msb(buf2, 4);
        route6_ntry.addr3 = get32msb(buf2, 8);
        route6_ntry.addr4 = get32msb(buf2, 12);
        route6_ntry.mask = atoi(arg[3]);
        route6_ntry.nexthop = atoi(arg[4]);
        route6_ntry.vrf = atoi(arg[6]);
        route6_ntry.command = 1;
        if (del == 0) table_del(&route6_table, &route6_ntry); else table_add(&route6_table, &route6_ntry);
        return 0;
    }
    if (strcmp(arg[0], "labroute6") == 0) {
        inet_pton(AF_INET6, arg[2], buf2);
        route6_ntry.addr1 = get32msb(buf2, 0);
        route6_ntry.addr2 = get32msb(buf2, 4);
        route6_ntry.addr3 = get32msb(buf2, 8);
        route6_ntry.addr4 = get32msb(buf2, 12);
        route6_ntry.mask = atoi(arg[3]);
        route6_ntry.nexthop = atoi(arg[4]);
        route6_ntry.vrf = atoi(arg[6]);
        route6_ntry.label1 = atoi(arg[7]);
        route6_ntry.command = 3;
        if (del == 0) table_del(&route6_table, &route6_ntry); else table_add(&route6_table, &route6_ntry);
        return 0;
    }
    if (strcmp(arg[0], "vpnroute6") == 0) {
        inet_pton(AF_INET6, arg[2], buf2);
        route6_ntry.addr1 = get32msb(buf2, 0);
        route6_ntry.addr2 = get32msb(buf2, 4);
        route6_ntry.addr3 = get32msb(buf2, 8);
        route6_ntry.addr4 = get32msb(buf2, 12);
        route6_ntry.mask = atoi(arg[3]);
        route6_ntry.nexthop = atoi(arg[4]);
        route6_ntry.vrf = atoi(arg[6]);
        route6_ntry.label1 = atoi(arg[7]);
        route6_ntry.label2 = atoi(arg[8]);
        route6_ntry.command = 4;
        if (del == 0) table_del(&route6_table, &route6_ntry); else table_add(&route6_table, &route6_ntry);
        return 0;
    }
    if (strcmp(arg[0], "neigh6") == 0) {
        route6_ntry.nexthop = atoi(arg[2]);
        inet_pton(AF_INET6, arg[3], buf2);
        route6_ntry.addr1 = get32msb(buf2, 0);
        route6_ntry.addr2 = get32msb(buf2, 4);
        route6_ntry.addr3 = get32msb(buf2, 8);
        route6_ntry.addr4 = get32msb(buf2, 12);
        route6_ntry.mask = 128;
        route6_ntry.vrf = atoi(arg[5]);
        route6_ntry.command = 1;
        neigh_ntry.id = route6_ntry.nexthop;
        neigh_ntry.vrf = route6_ntry.vrf;
        neigh_ntry.port = atoi(arg[7]);
        str2mac(neigh_ntry.dmac, arg[4]);
        str2mac(neigh_ntry.smac, arg[6]);
        if (del == 0) table_del(&route6_table, &route6_ntry); else table_add(&route6_table, &route6_ntry);
        if (del == 0) table_del(&neigh_table, &neigh_ntry); else table_add(&neigh_table, &neigh_ntry);
        return 0;
    }
    if (strcmp(arg[0], "inacl4") == 0) {
        acls_ntry.dir = 1;
        acls_ntry.ver = 4;
        acls_ntry.port = atoi(arg[2]);
        index = table_find(&acls_table, &acls_ntry);
        if (index < 0) {
            table_init(&acls_ntry.aces, sizeof(struct acl4_entry), &acl4_compare);
            table_add(&acls_table, &acls_ntry);
            acls_res = table_get(&acls_table, table_find(&acls_table, &acls_ntry));
        } else {
            acls_res = table_get(&acls_table, index);
        }
        readAcl4(&acl4_ntry, &arg[0]);
        if (del == 0) table_del(&acls_res->aces, &acl4_ntry); else table_add(&acls_res->aces, &acl4_ntry);
        return 0;
    }
    if (strcmp(arg[0], "outacl4") == 0) {
        acls_ntry.dir = 2;
        acls_ntry.ver = 4;
        acls_ntry.port = atoi(arg[2]);
        index = table_find(&acls_table, &acls_ntry);
        if (index < 0) {
            table_init(&acls_ntry.aces, sizeof(struct acl4_entry), &acl4_compare);
            table_add(&acls_table, &acls_ntry);
            acls_res = table_get(&acls_table, table_find(&acls_table, &acls_ntry));
        } else {
            acls_res = table_get(&acls_table, index);
        }
        readAcl4(&acl4_ntry, &arg[0]);
        if (del == 0) table_del(&acls_res->aces, &acl4_ntry); else table_add(&acls_res->aces, &acl4_ntry);
        return 0;
    }
    if (strcmp(arg[0], "natcfg4") == 0) {
        acls_ntry.dir = 3;
        acls_ntry.ver = 4;
        acls_ntry.port = atoi(arg[2]);
        index = table_find(&acls_table, &acls_ntry);
        if (index < 0) {
            table_init(&acls_ntry.aces, sizeof(struct acl4_entry), &acl4_compare);
            table_add(&acls_table, &acls_ntry);
            acls_res = table_get(&acls_table, table_find(&acls_table, &acls_ntry));
        } else {
            acls_res = table_get(&acls_table, index);
        }
        readAcl4(&acl4_ntry, &arg[0]);
        if (del == 0) table_del(&acls_res->aces, &acl4_ntry); else table_add(&acls_res->aces, &acl4_ntry);
        return 0;
    }
    if (strcmp(arg[0], "inacl6") == 0) {
        acls_ntry.dir = 1;
        acls_ntry.ver = 6;
        acls_ntry.port = atoi(arg[2]);
        index = table_find(&acls_table, &acls_ntry);
        if (index < 0) {
            table_init(&acls_ntry.aces, sizeof(struct acl6_entry), &acl6_compare);
            table_add(&acls_table, &acls_ntry);
            acls_res = table_get(&acls_table, table_find(&acls_table, &acls_ntry));
        } else {
            acls_res = table_get(&acls_table, index);
        }
        readAcl6(&acl6_ntry, &arg[0]);
        if (del == 0) table_del(&acls_res->aces, &acl6_ntry); else table_add(&acls_res->aces, &acl6_ntry);
        return 0;
    }
    if (strcmp(arg[0], "outacl6") == 0) {
        acls_ntry.dir = 2;
        acls_ntry.ver = 6;
        acls_ntry.port = atoi(arg[2]);
        index = table_find(&acls_table, &acls_ntry);
        if (index < 0) {
            table_init(&acls_ntry.aces, sizeof(struct acl6_entry), &acl6_compare);
            table_add(&acls_table, &acls_ntry);
            acls_res = table_get(&acls_table, table_find(&acls_table, &acls_ntry));
        } else {
            acls_res = table_get(&acls_table, index);
        }
        readAcl6(&acl6_ntry, &arg[0]);
        if (del == 0) table_del(&acls_res->aces, &acl6_ntry); else table_add(&acls_res->aces, &acl6_ntry);
        return 0;
    }
    if (strcmp(arg[0], "natcfg6") == 0) {
        acls_ntry.dir = 3;
        acls_ntry.ver = 6;
        acls_ntry.port = atoi(arg[2]);
        index = table_find(&acls_table, &acls_ntry);
        if (index < 0) {
            table_init(&acls_ntry.aces, sizeof(struct acl6_entry), &acl6_compare);
            table_add(&acls_table, &acls_ntry);
            acls_res = table_get(&acls_table, table_find(&acls_table, &acls_ntry));
        } else {
            acls_res = table_get(&acls_table, index);
        }
        readAcl6(&acl6_ntry, &arg[0]);
        if (del == 0) table_del(&acls_res->aces, &acl6_ntry); else table_add(&acls_res->aces, &acl6_ntry);
        return 0;
    }
    if (strcmp(arg[0], "nattrns4") == 0) {
        nat4_ntry.vrf = atoi(arg[2]);
        nat4_ntry.prot = atoi(arg[3]);
        inet_pton(AF_INET, arg[4], buf2);
        nat4_ntry.oSrcAddr = get32msb(buf2, 0);
        nat4_ntry.oSrcPort = atoi(arg[5]);
        inet_pton(AF_INET, arg[6], buf2);
        nat4_ntry.oTrgAddr = get32msb(buf2, 0);
        nat4_ntry.oTrgPort = atoi(arg[7]);
        inet_pton(AF_INET, arg[8], buf2);
        nat4_ntry.nSrcAddr = get32msb(buf2, 0);
        nat4_ntry.nSrcPort = atoi(arg[9]);
        inet_pton(AF_INET, arg[10], buf2);
        nat4_ntry.nTrgAddr = get32msb(buf2, 0);
        nat4_ntry.nTrgPort = atoi(arg[11]);
        nat4_ntry.sum3 = 0;
        accumulate_sum(nat4_ntry.sum3, nat4_ntry.oSrcAddr, -1);
        accumulate_sum(nat4_ntry.sum3, nat4_ntry.oTrgAddr, -1);
        accumulate_sum(nat4_ntry.sum3, nat4_ntry.nSrcAddr, +1);
        accumulate_sum(nat4_ntry.sum3, nat4_ntry.nTrgAddr, +1);
        nat4_ntry.sum4 = nat4_ntry.sum3;
        accumulate_sum(nat4_ntry.sum4, nat4_ntry.oSrcPort, -1);
        accumulate_sum(nat4_ntry.sum4, nat4_ntry.oTrgPort, -1);
        accumulate_sum(nat4_ntry.sum4, nat4_ntry.nSrcPort, +1);
        accumulate_sum(nat4_ntry.sum4, nat4_ntry.nTrgPort, +1);
        if (del == 0) table_del(&nat4_table, &nat4_ntry); else table_add(&nat4_table, &nat4_ntry);
        return 0;
    }
    if (strcmp(arg[0], "nattrns6") == 0) {
        nat6_ntry.vrf = atoi(arg[2]);
        nat6_ntry.prot = atoi(arg[3]);
        inet_pton(AF_INET6, arg[4], buf2);
        nat6_ntry.oSrcAddr1 = get32msb(buf2, 0);
        nat6_ntry.oSrcAddr2 = get32msb(buf2, 4);
        nat6_ntry.oSrcAddr3 = get32msb(buf2, 8);
        nat6_ntry.oSrcAddr4 = get32msb(buf2, 12);
        nat6_ntry.oSrcPort = atoi(arg[5]);
        inet_pton(AF_INET6, arg[6], buf2);
        nat6_ntry.oTrgAddr1 = get32msb(buf2, 0);
        nat6_ntry.oTrgAddr2 = get32msb(buf2, 4);
        nat6_ntry.oTrgAddr3 = get32msb(buf2, 8);
        nat6_ntry.oTrgAddr4 = get32msb(buf2, 12);
        nat6_ntry.oTrgPort = atoi(arg[7]);
        inet_pton(AF_INET6, arg[8], buf2);
        nat6_ntry.nSrcAddr1 = get32msb(buf2, 0);
        nat6_ntry.nSrcAddr2 = get32msb(buf2, 4);
        nat6_ntry.nSrcAddr3 = get32msb(buf2, 8);
        nat6_ntry.nSrcAddr4 = get32msb(buf2, 12);
        nat6_ntry.nSrcPort = atoi(arg[9]);
        inet_pton(AF_INET6, arg[10], buf2);
        nat6_ntry.nTrgAddr1 = get32msb(buf2, 0);
        nat6_ntry.nTrgAddr2 = get32msb(buf2, 4);
        nat6_ntry.nTrgAddr3 = get32msb(buf2, 8);
        nat6_ntry.nTrgAddr4 = get32msb(buf2, 12);
        nat6_ntry.nTrgPort = atoi(arg[11]);
        nat6_ntry.sum3 = 0;
        accumulate_sum(nat6_ntry.sum3, nat6_ntry.oSrcAddr1, -1);
        accumulate_sum(nat6_ntry.sum3, nat6_ntry.oSrcAddr2, -1);
        accumulate_sum(nat6_ntry.sum3, nat6_ntry.oSrcAddr3, -1);
        accumulate_sum(nat6_ntry.sum3, nat6_ntry.oSrcAddr4, -1);
        accumulate_sum(nat6_ntry.sum3, nat6_ntry.oTrgAddr1, -1);
        accumulate_sum(nat6_ntry.sum3, nat6_ntry.oTrgAddr2, -1);
        accumulate_sum(nat6_ntry.sum3, nat6_ntry.oTrgAddr3, -1);
        accumulate_sum(nat6_ntry.sum3, nat6_ntry.oTrgAddr4, -1);
        accumulate_sum(nat6_ntry.sum3, nat6_ntry.nSrcAddr1, +1);
        accumulate_sum(nat6_ntry.sum3, nat6_ntry.nSrcAddr2, +1);
        accumulate_sum(nat6_ntry.sum3, nat6_ntry.nSrcAddr3, +1);
        accumulate_sum(nat6_ntry.sum3, nat6_ntry.nSrcAddr4, +1);
        accumulate_sum(nat6_ntry.sum3, nat6_ntry.nTrgAddr1, +1);
        accumulate_sum(nat6_ntry.sum3, nat6_ntry.nTrgAddr2, +1);
        accumulate_sum(nat6_ntry.sum3, nat6_ntry.nTrgAddr3, +1);
        accumulate_sum(nat6_ntry.sum3, nat6_ntry.nTrgAddr4, +1);
        nat6_ntry.sum4 = nat6_ntry.sum3;
        accumulate_sum(nat6_ntry.sum4, nat6_ntry.oSrcPort, -1);
        accumulate_sum(nat6_ntry.sum4, nat6_ntry.oTrgPort, -1);
        accumulate_sum(nat6_ntry.sum4, nat6_ntry.nSrcPort, +1);
        accumulate_sum(nat6_ntry.sum4, nat6_ntry.nTrgPort, +1);
        if (del == 0) table_del(&nat6_table, &nat6_ntry); else table_add(&nat6_table, &nat6_ntry);
        return 0;
    }
    return 0;
}






void doReportRount(FILE *commands) {
    unsigned char buf[1024];
    unsigned char buf2[1024];
    unsigned char buf3[1024];
    for (int i=0; i<mpls_table.size; i++) {
        struct mpls_entry *ntry = table_get(&mpls_table, i);
        fprintf(commands, "mpls_cnt %i %li %li\n", ntry->label, ntry->pack, ntry->byte);
    }
    for (int i=0; i<neigh_table.size; i++) {
        struct neigh_entry *ntry = table_get(&neigh_table, i);
        fprintf(commands, "neigh_cnt %i %li %li\n", ntry->id, ntry->pack, ntry->byte);
    }
    for (int i=0; i<bridge_table.size; i++) {
        struct bridge_entry *ntry = table_get(&bridge_table, i);
        mac2str(ntry->mac, buf);
        fprintf(commands, "bridge_cnt %i %s %li %li\n", ntry->id, &buf, ntry->pack, ntry->byte);
    }
    for (int i=0; i<route4_table.size; i++) {
        struct route4_entry *ntry = table_get(&route4_table, i);
        put32msb(buf2, 0, ntry->addr);
        inet_ntop(AF_INET, &buf2[0], &buf[0], sizeof(buf));
        fprintf(commands, "route4_cnt %i %s %i %li %li\n", ntry->vrf, &buf, ntry->mask, ntry->pack, ntry->byte);
    }
    for (int i=0; i<route6_table.size; i++) {
        struct route6_entry *ntry = table_get(&route6_table, i);
        put32msb(buf2, 0, ntry->addr1);
        put32msb(buf2, 4, ntry->addr2);
        put32msb(buf2, 8, ntry->addr3);
        put32msb(buf2, 12, ntry->addr4);
        inet_ntop(AF_INET6, &buf2[0], &buf[0], sizeof(buf));
        fprintf(commands, "route6_cnt %i %s %i %li %li\n", ntry->vrf, &buf, ntry->mask, ntry->pack, ntry->byte);
    }
    for (int i=0; i<nat4_table.size; i++) {
        struct nat4_entry *ntry = table_get(&nat4_table, i);
        put32msb(buf, 0, ntry->oSrcAddr);
        inet_ntop(AF_INET, &buf[0], &buf2[0], sizeof(buf2));
        put32msb(buf, 0, ntry->oTrgAddr);
        inet_ntop(AF_INET, &buf[0], &buf3[0], sizeof(buf3));
        fprintf(commands, "nattrns4_cnt %i %i %s %s %i %i %li %li\n", ntry->vrf, ntry->prot, &buf2, &buf3, ntry->oSrcPort, ntry->oTrgPort, ntry->pack, ntry->byte);
    }
    for (int i=0; i<nat6_table.size; i++) {
        struct nat6_entry *ntry = table_get(&nat6_table, i);
        put32msb(buf, 0, ntry->oSrcAddr1);
        put32msb(buf, 4, ntry->oSrcAddr2);
        put32msb(buf, 8, ntry->oSrcAddr3);
        put32msb(buf, 12, ntry->oSrcAddr4);
        inet_ntop(AF_INET6, &buf[0], &buf2[0], sizeof(buf2));
        put32msb(buf, 0, ntry->oTrgAddr1);
        put32msb(buf, 4, ntry->oTrgAddr2);
        put32msb(buf, 8, ntry->oTrgAddr3);
        put32msb(buf, 12, ntry->oTrgAddr4);
        inet_ntop(AF_INET6, &buf[0], &buf3[0], sizeof(buf3));
        fprintf(commands, "nattrns6_cnt %i %i %s %s %i %i %li %li\n", ntry->vrf, ntry->prot, &buf2, &buf3, ntry->oSrcPort, ntry->oTrgPort, ntry->pack, ntry->byte);
    }
    fflush(commands);
}



void doStatRount(FILE *commands) {
    for (int i = 0; i < ports; i++) {
        fprintf(commands, "counter %i %li %li %li %li %li %li\r\n", i, packRx[i], byteRx[i], packTx[i], byteTx[i], packDr[i], byteDr[i]);
    }
    for (int i=0; i<vlanin_table.size; i++) {
        struct vlan_entry *intry = table_get(&vlanin_table, i);
        struct vlan_entry *ontry = table_get(&vlanout_table, i);
        fprintf(commands, "counter %i %li %li %li %li 0 0\r\n", intry->id, intry->pack, intry->byte, ontry->pack, ontry->byte);
    }
    fflush(commands);
}





int doConsoleCommand(unsigned char*buf) {
    unsigned char buf2[1024];
    switch (buf[0]) {
        case 0:
            break;
        case 'H':
        case 'h':
        case '?':
            printf("commands:\n");
            printf("h - this help\n");
            printf("q - exit process\n");
            printf("i - interface counters\n");
            printf("p - display portvrf table\n");
            printf("b - display bridge table\n");
            printf("m - display mpls table\n");
            printf("4 - display ipv4 table\n");
            printf("6 - display ipv6 table\n");
            printf("n - display nexthop table\n");
            printf("a - display acl table\n");
            printf("v - display vlan table\n");
            break;
        case 'Q':
        case 'q':
            return 1;
            break;
        case 'i':
        case 'I':
            printf("                           iface         rx         tx       drop         rx         tx       drop\n");
            for (int i=0; i<ports; i++) {
                printf("%32s %10li %10li %10li %10li %10li %10li\n", ifaceName[i], packRx[i], packTx[i], packDr[i], byteRx[i], byteTx[i], byteDr[i]);
            }
            break;
        case 'm':
        case 'M':
            printf("     label ip        vrf cmd       swap    nexthop\n");
            for (int i=0; i<mpls_table.size; i++) {
                struct mpls_entry *ntry = table_get(&mpls_table, i);
                printf("%10i %2i %10i %3i %10i %10i\n", ntry->label, ntry->ver, ntry->vrf, ntry->command, ntry->swap, ntry->nexthop);
            }
            break;
        case 'a':
        case 'A':
            printf("  vrf/port dir ver       aces\n");
            for (int i=0; i<acls_table.size; i++) {
                struct acls_entry *ntry = table_get(&acls_table, i);
                printf("%10i %3i %3i %10i\n", ntry->port, ntry->dir, ntry->ver, ntry->aces.size);
            }
            break;
        case 'p':
        case 'P':
            printf("      port cmd        vrf     bridge\n");
            for (int i=0; i<portvrf_table.size; i++) {
                struct portvrf_entry *ntry = table_get(&portvrf_table, i);
                printf("%10i %3i %10i %10i\n", ntry->port, ntry->command, ntry->vrf, ntry->bridge);
            }
            break;
        case 'n':
        case 'N':
            printf("        id        vrf       port              smac              dmac\n");
            for (int i=0; i<neigh_table.size; i++) {
                struct neigh_entry *ntry = table_get(&neigh_table, i);
                mac2str(ntry->smac, buf);
                mac2str(ntry->dmac, buf2);
                printf("%10i %10i %10i %s %s\n", ntry->id, ntry->vrf, ntry->port, &buf, &buf2);
            }
            break;
        case 'b':
        case 'B':
            printf("    bridge               mac       port\n");
            for (int i=0; i<bridge_table.size; i++) {
                struct bridge_entry *ntry = table_get(&bridge_table, i);
                mac2str(ntry->mac, buf);
                printf("%10i %s %10i\n", ntry->id, buf, ntry->port);
            }
            break;
        case 'v':
        case 'V':
            printf("        id       vlan       port\n");
            for (int i=0; i<vlanin_table.size; i++) {
                struct vlan_entry *ntry = table_get(&vlanin_table, i);
                printf("%10i %10i %10i\n", ntry->id, ntry->vlan, ntry->port);
            }
            break;
        case '4':
            printf("            addr msk        vrf cmd    nexthop     label1     label2\n");
            for (int i=0; i<route4_table.size; i++) {
                struct route4_entry *ntry = table_get(&route4_table, i);
                put32msb(buf2, 0, ntry->addr);
                inet_ntop(AF_INET, &buf2[0], &buf[0], sizeof(buf));
                printf("%16s %3i %10i %3i %10i %10i %10i\n", &buf, ntry->mask, ntry->vrf, ntry->command, ntry->nexthop, ntry->label1, ntry->label2);
            }
            break;
        case '6':
            printf("                                    addr msk        vrf cmd    nexthop     label1     label2\n");
            for (int i=0; i<route6_table.size; i++) {
                struct route6_entry *ntry = table_get(&route6_table, i);
                put32msb(buf2, 0, ntry->addr1);
                put32msb(buf2, 4, ntry->addr2);
                put32msb(buf2, 8, ntry->addr3);
                put32msb(buf2, 12, ntry->addr4);
                inet_ntop(AF_INET6, &buf2[0], &buf[0], sizeof(buf));
                printf("%40s %3i %10i %3i %10i %10i %10i\n", &buf, ntry->mask, ntry->vrf, ntry->command, ntry->nexthop, ntry->label1, ntry->label2);
            }
            break;
        default:
            printf("unknown command '%s', try ?\n", buf);
            break;
    }
    return 0;
}
