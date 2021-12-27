#include <linux/bpf.h>
#include <bpf/bpf_helpers.h>
#include "utils.h"
#include "types.h"
#include "p4xdp_tab.h"


struct {
    __uint(type, BPF_MAP_TYPE_ARRAY);
    __type(key, int);
    __type(value, int);
    __uint(max_entries, 2);
} cpu_port SEC(".maps");


struct {
    __uint(type, BPF_MAP_TYPE_HASH);
    __type(key, int);
    __type(value, struct port_res);
    __uint(max_entries, maxPorts);
} tx_ports SEC(".maps");


struct {
    __uint(type, BPF_MAP_TYPE_HASH);
    __type(key, int);
    __type(value, struct port_res);
    __uint(max_entries, maxPorts);
} rx_ports SEC(".maps");


struct {
    __uint(type, BPF_MAP_TYPE_HASH);
    __type(key, int);
    __type(value, struct vrfp_res);
    __uint(max_entries, maxPorts);
} vrf_port SEC(".maps");


struct {
    __uint(type, BPF_MAP_TYPE_HASH);
    __type(key, int);
    __type(value, struct neigh_res);
    __uint(max_entries, 512);
} neighs SEC(".maps");


struct {
    __uint(type, BPF_MAP_TYPE_LPM_TRIE);
    __type(key, struct route4_key);
    __type(value, struct routes_res);
    __uint(max_entries, 2048);
    __uint(map_flags, BPF_F_NO_PREALLOC);
} routes4 SEC(".maps");


struct {
    __uint(type, BPF_MAP_TYPE_LPM_TRIE);
    __type(key, struct route6_key);
    __type(value, struct routes_res);
    __uint(max_entries, 2048);
    __uint(map_flags, BPF_F_NO_PREALLOC);
} routes6 SEC(".maps");


struct {
    __uint(type, BPF_MAP_TYPE_HASH);
    __type(key, int);
    __type(value, struct label_res);
    __uint(max_entries, 1024);
} labels SEC(".maps");


struct {
    __uint(type, BPF_MAP_TYPE_HASH);
    __type(key, int);
    __type(value, struct bundle_res);
    __uint(max_entries, 256);
} bundles SEC(".maps");


struct {
    __uint(type, BPF_MAP_TYPE_HASH);
    __type(key, struct vlan_key);
    __type(value, int);
    __uint(max_entries, 512);
} vlan_in SEC(".maps");


struct {
    __uint(type, BPF_MAP_TYPE_HASH);
    __type(key, int);
    __type(value, struct vlan_res);
    __uint(max_entries, 512);
} vlan_out SEC(".maps");


struct {
    __uint(type, BPF_MAP_TYPE_HASH);
    __type(key, struct pppoe_key);
    __type(value, int);
    __uint(max_entries, 128);
} pppoes SEC(".maps");


struct {
    __uint(type, BPF_MAP_TYPE_HASH);
    __type(key, struct bridge_key);
    __type(value, struct bridge_res);
    __uint(max_entries, 1024);
} bridges SEC(".maps");




#define revalidatePacket(size)                  \
    bufE = (unsigned char*)(long)ctx->data_end; \
    bufD = (unsigned char*)(long)ctx->data;     \
    if ((size + bufD) > bufE) goto drop;




#define update_chksum(ofs, val)                                 \
    int sum = get16lsb(bufD, ofs);                              \
    sum -= val;                                                 \
    sum = (sum & 0xffff) + (sum >> 16);                         \
    put16lsb(bufD, ofs, sum);



#define doRouted(res)                                               \
    res->pack++;                                                    \
    res->byte += bufE - bufD;                                       \
    switch (res->cmd) {                                             \
    case 1:                                                         \
        neik = res->hop;                                            \
        goto ethtyp_tx;                                             \
    case 2:                                                         \
        goto cpu;                                                   \
    case 3:                                                         \
        bufP -= 4;                                                  \
        put32msb(bufD, bufP, ((res->label1 << 12) | 0x100 | ttl));  \
        ethtyp = ETHERTYPE_MPLS_UCAST;                              \
        neik = res->hop;                                            \
        goto ethtyp_tx;                                             \
    case 4:                                                         \
        bufP -= 4;                                                  \
        put32msb(bufD, bufP, ((res->label2 << 12) | 0x100 | ttl));  \
        bufP -= 4;                                                  \
        put32msb(bufD, bufP, ((res->label1 << 12) | ttl));          \
        ethtyp = ETHERTYPE_MPLS_UCAST;                              \
        neik = res->hop;                                            \
        goto ethtyp_tx;                                             \
    default:                                                        \
        goto drop;                                                  \
    }



#define readMpls()                                                  \
    label = get32msb(bufD, bufP);                                   \
    ttl = (label & 0xff) - 1;                                       \
    if (ttl <= 1) goto punt;                                        \
    tmp = label & 0xf00;                                            \
    label = (label >> 12) & 0xfffff;                                \
    hash ^= label;                                                  \
    bufP += 4;                                                      \
    resm = bpf_map_lookup_elem(&labels, &label);                    \
    if (resm == NULL) goto drop;                                    \
    resm->pack++;                                                   \
    resm->byte += bufE - bufD;



#define routeMpls()                                                 \
    vrfp->vrf = resm->vrf;                                          \
    switch (resm->ver) {                                            \
    case 4:                                                         \
        ethtyp = ETHERTYPE_IPV4;                                    \
        goto ipv4_rx;                                               \
    case 6:                                                         \
        ethtyp = ETHERTYPE_IPV6;                                    \
        goto ipv6_rx;                                               \
    default:                                                        \
        goto drop;                                                  \
    }



#define switchMpls()                                                \
    case 2:                                                         \
        neik = resm->hop;                                           \
        if ((tmp & 0x100) == 0) {                                   \
            bufD[bufP + 3] = ttl;                                   \
            goto ethtyp_tx;                                         \
        }                                                           \
        switch (resm->ver) {                                        \
        case 4:                                                     \
            ethtyp = ETHERTYPE_IPV4;                                \
            goto ethtyp_tx;                                         \
        case 6:                                                     \
            ethtyp = ETHERTYPE_IPV6;                                \
            goto ethtyp_tx;                                         \
        default:                                                    \
            goto drop;                                              \
        }                                                           \
    case 3:                                                         \
        bufP -= 4;                                                  \
        label = (tmp & 0xf00) | ttl | (resm->swap << 12);           \
        put32msb(bufD, bufP, label);                                \
        neik = resm->hop;                                           \
        goto ethtyp_tx;                                             \
    case 4:                                                         \
        revalidatePacket(bufP + 14);                                \
        __builtin_memcpy(&macaddr[0], &bufD[bufP], sizeof(macaddr));\
        bufP += 12;                                                 \
        prt = resm->port;                                           \
        goto subif_tx;                                              \
    case 5:                                                         \
        revalidatePacket(bufP + 14);                                \
        __builtin_memcpy(&macaddr[0], &bufD[bufP], sizeof(macaddr));\
        bufP += 12;                                                 \
        ethtyp = get16msb(bufD, bufP);                              \
        bufP += 2;                                                  \
        tmp = resm->brdg;                                           \
        goto bridge_rx;                                             \
    default:                                                        \
        goto drop;




SEC("p4xdp_router")
int xdp_router(struct xdp_md *ctx) {

    unsigned char* bufE;
    unsigned char* bufD;
    revalidatePacket(18);
    unsigned char macaddr[6 + 6];
    __builtin_memcpy(macaddr, &bufD[0], sizeof(macaddr));

    int tmp = 0;
    int* cpuport = bpf_map_lookup_elem(&cpu_port, &tmp);
    if (cpuport == NULL) goto drop;

    int prt = ctx->ingress_ifindex;
    struct port_res* rxport = bpf_map_lookup_elem(&rx_ports, &prt);
    if (rxport == NULL) goto drop;
    rxport->pack++;
    rxport->byte += bufE - bufD;
    if (prt == *cpuport) {
        prt = get16msb(bufD, 0);
        struct port_res* txport = bpf_map_lookup_elem(&tx_ports, &prt);
        if (txport == NULL) goto drop;
        txport->pack++;
        txport->byte += bufE - bufD;
        if (bpf_xdp_adjust_head(ctx, 2) != 0) goto drop;
        return bpf_redirect(txport->idx, 0);
    }
    prt = rxport->idx;

    int ethtyp = get16msb(bufD, sizeof(macaddr));
    int bufP = sizeof(macaddr) + 2;
    int hash = get32msb(macaddr, 0);
    hash ^= get32msb(macaddr, 4);
    hash ^= get32msb(macaddr, 8);

    if (ethtyp == ETHERTYPE_VLAN) {
        struct vlan_key vlnk;
        vlnk.port = prt;
        vlnk.vlan = get16msb(bufD, bufP) & 0xfff;
        bufP += 2;
        ethtyp = get16msb(bufD, bufP);
        bufP += 2;
        int* res = bpf_map_lookup_elem(&vlan_in, &vlnk);
        if (res == NULL) goto drop;
        prt = *res;
    }

    switch (ethtyp) {
    case ETHERTYPE_PPPOE_DATA:
        revalidatePacket(bufP + 12);
        struct pppoe_key pppk;
        pppk.port = prt;
        pppk.sess = get16msb(bufD, bufP + 2);
        int* res = bpf_map_lookup_elem(&pppoes, &pppk);
        if (res == NULL) goto drop;
        prt = *res;
        ethtyp = get16msb(bufD, bufP + 6);
        if ((ethtyp & 0x8000) != 0) goto cpu;
        bufP += 8;
        switch (ethtyp) {
        case PPPTYPE_MPLS_UCAST:
            ethtyp = ETHERTYPE_MPLS_UCAST;
            break;
        case PPPTYPE_IPV4:
            ethtyp = ETHERTYPE_IPV4;
            break;
        case PPPTYPE_IPV6:
            ethtyp = ETHERTYPE_IPV6;
            break;
        default:
            goto drop;
        }
        break;
    case ETHERTYPE_PPPOE_CTRL:
        goto cpu;
    }

    struct vrfp_res* vrfp = bpf_map_lookup_elem(&vrf_port, &prt);
    if (vrfp == NULL) goto drop;
    vrfp->pack++;
    vrfp->byte += bufE - bufD;
    int neik = 0;
    int ttl = 0;

    switch (vrfp->cmd) {
    case 1:
        break;
    case 2:
        tmp = vrfp->brdg;
        goto bridge_rx;
    case 3:
        bufP -= 2;
        bufP -= 12;
        bufP -= 2 * sizeof(macaddr);
        if (bpf_xdp_adjust_head(ctx, bufP) != 0) goto drop;
        bufP = 2 * sizeof(macaddr);
        revalidatePacket(3 * sizeof(macaddr));
        __builtin_memcpy(&bufD[bufP], &macaddr[0], sizeof(macaddr));
        ethtyp = ETHERTYPE_MPLS_UCAST;
        bufP -= 4;
        ttl = 0x1ff | (vrfp->label2 << 12);
        put32msb(bufD, bufP, ttl);
        bufP -= 4;
        ttl = 0xff | (vrfp->label1 << 12);
        put32msb(bufD, bufP, ttl);
        neik = vrfp->hop;
        goto ethtyp_tx;
    default:
        goto drop;
    }

    switch (ethtyp) {
    case ETHERTYPE_MPLS_UCAST:
        if (vrfp->mpls == 0) goto drop;
        revalidatePacket(bufP + 12);
        int label;
        struct label_res* resm;
        readMpls();
        switch (resm->cmd) {
        case 1:
            if ((tmp & 0x100) == 0) {
                bufD[bufP + 3] = ttl + 1;
                readMpls();
                switch (resm->cmd) {
                case 1:
                    routeMpls();
                    switchMpls();
                }
            }
            routeMpls();
            switchMpls();
        }
        goto drop;
    case ETHERTYPE_IPV4:
ipv4_rx:
        revalidatePacket(bufP + 20);
        if ((bufD[bufP + 0] & 0xf0) != 0x40) goto drop;
        if ((bufD[bufP + 0] & 0xf) < 5) goto drop;
        ttl = bufD[bufP + 8] - 1;
        if (ttl <= 1) goto punt;
        if (bufD[bufP + 9] == 46) goto cpu;
        bufD[bufP + 8] = ttl;
        update_chksum(bufP + 10, -1);
        struct route4_key rou4;
        rou4.bits = (sizeof(rou4) * 8) - routes_bits;
        rou4.vrf = vrfp->vrf;
        __builtin_memcpy(rou4.addr, &bufD[bufP + 16], sizeof(rou4.addr));
        struct routes_res* res4 = bpf_map_lookup_elem(&routes4, &rou4);
        if (res4 == NULL) goto punt;
        hash ^= get32msb(bufD, bufP + 12); // src
        hash ^= get32msb(bufD, bufP + 16); // dst
        doRouted(res4);
    case ETHERTYPE_IPV6:
ipv6_rx:
        revalidatePacket(bufP + 40);
        if ((bufD[bufP + 0] & 0xf0) != 0x60) goto drop;
        ttl = bufD[bufP + 7] - 1;
        if (ttl <= 1) goto punt;
        if (bufD[bufP + 6] == 0) goto cpu;
        bufD[bufP + 7] = ttl;
        struct route6_key rou6;
        rou6.bits = (sizeof(rou6) * 8) - routes_bits;
        rou6.vrf = vrfp->vrf;
        __builtin_memcpy(rou6.addr, &bufD[bufP + 24], sizeof(rou6.addr));
        struct routes_res* res6 = bpf_map_lookup_elem(&routes6, &rou6);
        if (res6 == NULL) goto punt;
        hash ^= get32msb(bufD, bufP + 8); // src
        hash ^= get32msb(bufD, bufP + 12);
        hash ^= get32msb(bufD, bufP + 16);
        hash ^= get32msb(bufD, bufP + 20);
        hash ^= get32msb(bufD, bufP + 24); // dst
        hash ^= get32msb(bufD, bufP + 28);
        hash ^= get32msb(bufD, bufP + 32);
        hash ^= get32msb(bufD, bufP + 36);
        doRouted(res6);
    case ETHERTYPE_ARP:
        goto cpu;
    case ETHERTYPE_LACP:
        goto cpu;
    case ETHERTYPE_LLDP:
        goto cpu;
    default:
        goto punt;
    }

bridge_rx:
    {}
    struct bridge_key brdk;
    __builtin_memcpy(brdk.mac, &macaddr[6], sizeof(brdk.mac));
    brdk.id = tmp;
    brdk.pad1 = 0;
    brdk.pad2 = 0;
    struct bridge_res* brdr = bpf_map_lookup_elem(&bridges, &brdk);
    if (brdr == NULL) goto cpu;
    brdr->packRx++;
    brdr->byteRx += bufE - bufD;
    __builtin_memcpy(brdk.mac, &macaddr[0], sizeof(brdk.mac));
    brdr = bpf_map_lookup_elem(&bridges, &brdk);
    if (brdr == NULL) goto cpu;
    brdr->packTx++;
    brdr->byteTx += bufE - bufD;
    switch (brdr->cmd) {
    case 1:
        bufP -= 2;
        put16msb(bufD, bufP, ethtyp);
        prt = brdr->port;
        goto subif_tx;
    case 2:
        bufP -= 2;
        put16msb(bufD, bufP, ethtyp);
        bufP -= 12;
        bufP -= 2 * sizeof(macaddr);
        if (bpf_xdp_adjust_head(ctx, bufP) != 0) goto drop;
        bufP = 2 * sizeof(macaddr);
        revalidatePacket(3 * sizeof(macaddr));
        __builtin_memcpy(&bufD[bufP], &macaddr[0], sizeof(macaddr));
        ethtyp = ETHERTYPE_MPLS_UCAST;
        bufP -= 4;
        tmp = 0x1ff | (brdr->label2 << 12);
        put32msb(bufD, bufP, tmp);
        bufP -= 4;
        tmp = 0xff | (brdr->label1 << 12);
        put32msb(bufD, bufP, tmp);
        neik = brdr->hop;
        goto ethtyp_tx;
    default:
        goto drop;
    }

ethtyp_tx:
    bufP -= 2;
    put16msb(bufD, bufP, ethtyp);

    struct neigh_res* neir = bpf_map_lookup_elem(&neighs, &neik);
    if (neir == NULL) goto punt;
    neir->pack++;
    neir->byte += bufE - bufD;
    __builtin_memcpy(&macaddr[0], neir->dmac, sizeof(neir->dmac));
    __builtin_memcpy(&macaddr[6], neir->smac, sizeof(neir->smac));
    prt = neir->port;
    switch (neir->cmd) {
    case 1:
        break;
    case 2:
        switch (ethtyp) {
        case ETHERTYPE_MPLS_UCAST:
            ethtyp = PPPTYPE_MPLS_UCAST;
            break;
        case ETHERTYPE_IPV4:
            ethtyp = PPPTYPE_IPV4;
            break;
        case ETHERTYPE_IPV6:
            ethtyp = PPPTYPE_IPV6;
            break;
        default:
            goto drop;
        }
        tmp = (bufE - bufD) - bufP;
        put16msb(bufD, bufP, ethtyp);
        bufP -= sizeof(macaddr);
        if (bpf_xdp_adjust_head(ctx, bufP) != 0) goto drop;
        bufP = sizeof(macaddr);
        revalidatePacket(sizeof(macaddr));
        bufP -= 6;
        put16msb(bufD, bufP + 0, 0x1100);
        put16msb(bufD, bufP + 2, neir->sess);
        put16msb(bufD, bufP + 4, tmp);
        ethtyp = ETHERTYPE_PPPOE_DATA;
        bufP -= 2;
        put16msb(bufD, bufP, ethtyp);
        break;
    default:
        goto drop;
    }

subif_tx:
    {}
    struct vlan_res* vlnr = bpf_map_lookup_elem(&vlan_out, &prt);
    if (vlnr != NULL) {
        hash ^= vlnr->vlan;
        bufP -= 2;
        put16msb(bufD, bufP, vlnr->vlan);
        bufP -= 2;
        put16msb(bufD, bufP, ETHERTYPE_VLAN);
        prt = vlnr->port;
        vlnr->pack++;
        vlnr->byte += bufE - bufD;
        ethtyp = ETHERTYPE_VLAN;
    }

    struct bundle_res* bunr = bpf_map_lookup_elem(&bundles, &prt);
    if (bunr != NULL) {
        hash = ((hash >> 16) ^ hash) & 0xffff;
        hash = ((hash >> 8) ^ hash) & 0xff;
        hash = ((hash >> 4) ^ hash) & 0xf;
        //prt = bunr->out[hash];
        switch (hash) {
        case 0:
            prt = bunr->out[0];
            break;
        case 1:
            prt = bunr->out[1];
            break;
        case 2:
            prt = bunr->out[2];
            break;
        case 3:
            prt = bunr->out[3];
            break;
        case 4:
            prt = bunr->out[4];
            break;
        case 5:
            prt = bunr->out[5];
            break;
        case 6:
            prt = bunr->out[6];
            break;
        case 7:
            prt = bunr->out[7];
            break;
        case 8:
            prt = bunr->out[8];
            break;
        case 9:
            prt = bunr->out[9];
            break;
        case 10:
            prt = bunr->out[10];
            break;
        case 11:
            prt = bunr->out[11];
            break;
        case 12:
            prt = bunr->out[12];
            break;
        case 13:
            prt = bunr->out[13];
            break;
        case 14:
            prt = bunr->out[14];
            break;
        case 15:
            prt = bunr->out[15];
            break;
        default:
            prt = bunr->out[0];
            break;
        }
        bunr->pack++;
        bunr->byte += bufE - bufD;
    }

    bufP -= sizeof(macaddr);
    if (bpf_xdp_adjust_head(ctx, bufP) != 0) goto drop;
    bufP = 0;
    revalidatePacket(sizeof(macaddr));
    __builtin_memcpy(bufD, &macaddr, sizeof(macaddr));

    struct port_res* txport = bpf_map_lookup_elem(&tx_ports, &prt);
    if (txport == NULL) goto drop;
    txport->pack++;
    txport->byte += bufE - bufD;
    return bpf_redirect(txport->idx, 0);

punt:
    tmp = 1;
    int* remain = bpf_map_lookup_elem(&cpu_port, &tmp);
    if (remain == NULL) goto drop;
    (*remain)--;
    if (*remain < 1) goto drop;
cpu:
    prt = 0;
    if (bpf_xdp_adjust_head(ctx, -2) != 0) goto drop;
    revalidatePacket(2);
    put16msb(bufD, 0, rxport->idx);
    return bpf_redirect(*cpuport, 0);
drop:
    return XDP_DROP;
}

