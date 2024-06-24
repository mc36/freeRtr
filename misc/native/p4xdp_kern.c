#include <linux/bpf.h>
#include <bpf/bpf_helpers.h>

#include "utils.h"
#include "types.h"
#include "p4xdp_tab.h"


// hack
#ifndef NULL
#define NULL ((void *)0)
#endif


struct {
    __uint(type, BPF_MAP_TYPE_ARRAY);
    __type(key, __u32);
    __type(value, __u32);
    __uint(max_entries, 2);
} cpu_port SEC(".maps");


struct {
    __uint(type, BPF_MAP_TYPE_HASH);
    __type(key, __u32);
    __type(value, struct port_res);
    __uint(max_entries, maxPorts);
} tx_ports SEC(".maps");


struct {
    __uint(type, BPF_MAP_TYPE_HASH);
    __type(key, __u32);
    __type(value, struct port_res);
    __uint(max_entries, maxPorts);
} rx_ports SEC(".maps");


struct {
    __uint(type, BPF_MAP_TYPE_HASH);
    __type(key, __u32);
    __type(value, struct vrfp_res);
    __uint(max_entries, maxPorts);
} vrf_port SEC(".maps");


struct {
    __uint(type, BPF_MAP_TYPE_HASH);
    __type(key, __u32);
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
    __type(key, __u32);
    __type(value, struct label_res);
    __uint(max_entries, 1024);
} labels SEC(".maps");


struct {
    __uint(type, BPF_MAP_TYPE_HASH);
    __type(key, __u32);
    __type(value, struct bundle_res);
    __uint(max_entries, 256);
} bundles SEC(".maps");


struct {
    __uint(type, BPF_MAP_TYPE_HASH);
    __type(key, struct vlan_key);
    __type(value, __u32);
    __uint(max_entries, 512);
} vlan_in SEC(".maps");


struct {
    __uint(type, BPF_MAP_TYPE_HASH);
    __type(key, __u32);
    __type(value, struct vlan_res);
    __uint(max_entries, 512);
} vlan_out SEC(".maps");


struct {
    __uint(type, BPF_MAP_TYPE_HASH);
    __type(key, struct pppoe_key);
    __type(value, __u32);
    __uint(max_entries, 128);
} pppoes SEC(".maps");


struct {
    __uint(type, BPF_MAP_TYPE_HASH);
    __type(key, struct bridge_key);
    __type(value, struct bridge_res);
    __uint(max_entries, 1024);
} bridges SEC(".maps");


struct {
    __uint(type, BPF_MAP_TYPE_HASH);
    __type(key, struct tunnel4_key);
    __type(value, struct tunnel_res);
    __uint(max_entries, 128);
} tunnels4 SEC(".maps");


struct {
    __uint(type, BPF_MAP_TYPE_HASH);
    __type(key, struct tunnel6_key);
    __type(value, struct tunnel_res);
    __uint(max_entries, 128);
} tunnels6 SEC(".maps");




#define revalidatePacket(size)                                      \
    bufE = (__u8*)(__u64)ctx->data_end;                             \
    bufD = (__u8*)(__u64)ctx->data;                                 \
    if (((__u64)bufD + (__u64)size) > (__u64)bufE) goto drop;




#define update_chksum(ofs, val)                                     \
    tmp = get16lsb(bufD, ofs);                                      \
    tmp -= val;                                                     \
    tmp = (tmp & 0xffff) + (tmp >> 16);                             \
    put16lsb(bufD, ofs, tmp);



#define doRouted(res)                                               \
    res->pack++;                                                    \
    res->byte += bufE - bufD;                                       \
    switch (res->cmd) {                                             \
    case 1:                                                         \
        neik = res->hop;                                            \
        goto ethtyp_tx;                                             \
    case 2:                                                         \
        break;                                                      \
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
    case 5:                                                         \
        goto drop;                                                  \
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


#define extract_layer4(tun)                                         \
    switch (tun.prot) {                                             \
        case IP_PROTOCOL_TCP:                                       \
            revalidatePacket(bufP + 4);                             \
            tun.srcPort = get16msb(bufD, bufP + 0);                 \
            tun.trgPort = get16msb(bufD, bufP + 2);                 \
            break;                                                  \
        case IP_PROTOCOL_UDP:                                       \
            revalidatePacket(bufP + 4);                             \
            tun.srcPort = get16msb(bufD, bufP + 0);                 \
            tun.trgPort = get16msb(bufD, bufP + 2);                 \
            break;                                                  \
        default:                                                    \
            tun.srcPort = 0;                                        \
            tun.trgPort = 0;                                        \
            break;                                                  \
    }



#define ethtyp2ppptyp()                                         \
    switch (ethtyp) {                                           \
    case ETHERTYPE_MPLS_UCAST:                                  \
        ethtyp = PPPTYPE_MPLS_UCAST;                            \
        break;                                                  \
    case ETHERTYPE_IPV4:                                        \
        ethtyp = PPPTYPE_IPV4;                                  \
        break;                                                  \
    case ETHERTYPE_IPV6:                                        \
        ethtyp = PPPTYPE_IPV6;                                  \
        break;                                                  \
    case ETHERTYPE_SGT:                                         \
        ethtyp = PPPTYPE_SGT;                                   \
        break;                                                  \
    default:                                                    \
        goto drop;                                              \
    }


#define ppptyp2ethtyp()                                         \
    if ((ethtyp & 0x8000) != 0) goto cpu;                       \
    switch (ethtyp) {                                           \
    case PPPTYPE_MPLS_UCAST:                                    \
        ethtyp = ETHERTYPE_MPLS_UCAST;                          \
        break;                                                  \
    case PPPTYPE_IPV4:                                          \
        ethtyp = ETHERTYPE_IPV4;                                \
        break;                                                  \
    case PPPTYPE_IPV6:                                          \
        ethtyp = ETHERTYPE_IPV6;                                \
        break;                                                  \
    case PPPTYPE_SGT:                                           \
        ethtyp = ETHERTYPE_SGT;                                 \
        break;                                                  \
    default:                                                    \
        goto drop;                                              \
    }


#define doTunnel(tun)                                           \
    if (tunr == NULL) goto cpu;                                 \
    tunr->pack++;                                               \
    tunr->byte += bufE - bufD;                                  \
    switch (tunr->cmd) {                                        \
        case 1:                                                 \
            bufP += 2;                                          \
            break;                                              \
        case 2:                                                 \
            bufP += 8;                                          \
            revalidatePacket(bufP + 12);                        \
            if ((get16msb(bufD, bufP) & 0x8000) != 0) goto cpu; \
            bufP += 8;                                          \
            bufP += 2;                                          \
            ethtyp = get16msb(bufD, bufP);                      \
            ppptyp2ethtyp();                                    \
            put16msb(bufD, bufP, ethtyp);                       \
            break;                                              \
        default:                                                \
            goto drop;                                          \
    }                                                           \
    prt = tunr->aclport;                                        \
    if (bpf_xdp_adjust_head(ctx, bufP - sizeof(macaddr)) != 0) goto drop;   \
    continue;


#define putSgt()                                                \
    if (vrfp->sgtTag != 0) {                                    \
        bufP -= 2 * sizeof(macaddr);                            \
        if (bpf_xdp_adjust_head(ctx, bufP) != 0) goto drop;     \
        bufP = 2 * sizeof(macaddr);                             \
        revalidatePacket(3 * sizeof(macaddr));                  \
        bufP -= 8;                                              \
        put16msb(bufD, bufP + 2, 0x0101);                       \
        put16msb(bufD, bufP + 4, 0x0001);                       \
        put16msb(bufD, bufP + 6, sgt);                          \
        ethtyp = ETHERTYPE_SGT;                                 \
        put16msb(bufD, bufP + 0, ethtyp);                       \
    }


#define putIpv4header(proto)                                    \
    bufP -= 20;                                                 \
    put16msb(bufD, bufP + 0, 0x4500);                           \
    ethtyp = bufE - bufD - sizeof(macaddr) - 2;                 \
    put16msb(bufD, bufP + 2, ethtyp);                           \
    put16msb(bufD, bufP + 4, 0);                                \
    put16msb(bufD, bufP + 6, 0);                                \
    bufD[bufP + 8] = 0xff;                                      \
    bufD[bufP + 9] = proto;                                     \
    ethtyp += 0x4500 + 0xff00 + proto;                          \
    ethtyp += get16msb(neir->srcAddr, 0);                       \
    ethtyp += get16msb(neir->srcAddr, 2);                       \
    ethtyp += get16msb(neir->trgAddr, 0);                       \
    ethtyp += get16msb(neir->trgAddr, 2);                       \
    ethtyp = (ethtyp >> 16) + (ethtyp & 0xffff);                \
    ethtyp += (ethtyp >> 16);                                   \
    ethtyp = 0xffff & (~ethtyp);                                \
    put16msb(bufD, bufP + 10, ethtyp);                          \
    __builtin_memcpy(&bufD[bufP + 12], &neir->srcAddr, 4);      \
    __builtin_memcpy(&bufD[bufP + 16], &neir->trgAddr, 4);      \
    ethtyp = ETHERTYPE_IPV4;                                    \
    bufP -= 2;                                                  \
    put16msb(bufD, bufP, ethtyp);



#define putIpv6header(proto)                                    \
    bufP -= 40;                                                 \
    put16msb(bufD, bufP + 0, 0x6000);                           \
    put16msb(bufD, bufP + 2, 0);                                \
    ethtyp = bufE - bufD - sizeof(macaddr) - 42;                \
    put16msb(bufD, bufP + 4, ethtyp);                           \
    bufD[bufP + 6] = proto;                                     \
    bufD[bufP + 7] = 0xff;                                      \
    __builtin_memcpy(&bufD[bufP + 8], &neir->srcAddr, 16);      \
    __builtin_memcpy(&bufD[bufP + 24], &neir->trgAddr, 16);     \
    ethtyp = ETHERTYPE_IPV6;                                    \
    bufP -= 2;                                                  \
    put16msb(bufD, bufP, ethtyp);



#define putGreHeader()                                          \
    bufP -= 2;                                                  \
    put16msb(bufD, bufP, 0x0000);


#define putUdpHeader()                                          \
    bufP -= 8;                                                  \
    put16msb(bufD, bufP + 0, neir->srcPort);                    \
    put16msb(bufD, bufP + 2, neir->trgPort);                    \
    ethtyp = bufE - bufD - bufP;                                \
    put16msb(bufD, bufP + 4, ethtyp);                           \
    put16msb(bufD, bufP + 6, 0);


#define putL2tpHeader()                                         \
    put16msb(bufD, bufP, ethtyp);                               \
    bufP -= 10;                                                 \
    put16msb(bufD, bufP + 0, 0x0202);                           \
    put32msb(bufD, bufP + 2, neir->sess);                       \
    put16msb(bufD, bufP + 6, 0);                                \
    put16msb(bufD, bufP + 8, 0xff03);






SEC("p4xdp_router")
__u32 xdp_router(struct xdp_md *ctx) {

    __u8* bufE;
    __u8* bufD;
    __u8 macaddr[6 + 6];
    revalidatePacket(sizeof(macaddr) + 8);
    __builtin_memcpy(macaddr, &bufD[0], sizeof(macaddr));

    __u32 tmp = 0;
    __u32* cpuPort = bpf_map_lookup_elem(&cpu_port, &tmp);
    if (cpuPort == NULL) goto drop;

    __u32 prt = ctx->ingress_ifindex;
    struct port_res* rxport = bpf_map_lookup_elem(&rx_ports, &prt);
    if (rxport == NULL) goto drop;
    rxport->pack++;
    rxport->byte += bufE - bufD;
    if (prt == *cpuPort) {
        prt = get16msb(bufD, 0);
        struct port_res* txport = bpf_map_lookup_elem(&tx_ports, &prt);
        if (txport == NULL) goto drop;
        txport->pack++;
        txport->byte += bufE - bufD;
        if (bpf_xdp_adjust_head(ctx, 2) != 0) goto drop;
        return bpf_redirect(txport->idx, 0);
    }
    prt = rxport->idx;

#ifdef HAVE_NOHW

    goto cpu;

#else
    __u32 hash = get32msb(macaddr, 0);
    hash ^= get32msb(macaddr, 4);
    hash ^= get32msb(macaddr, 8);
    __u32 sgt = 0;

    for (__u32 rounds = 0; rounds < 8; rounds++) {

        __u64 bufP = sizeof(macaddr) + 2;
        revalidatePacket(bufP);
        __u32 ethtyp = get16msb(bufD, bufP - 2);
        __u32 neik = 0;
        __s32 ttl = 0;

        struct vrfp_res* vrfp = bpf_map_lookup_elem(&vrf_port, &prt);
        if (vrfp != NULL) {
            vrfp->packRx++;
            vrfp->byteRx += bufE - bufD;

            if (vrfp->sgtTag != 0) {
                revalidatePacket(bufP + 12);
                if (ethtyp != ETHERTYPE_SGT) goto drop;
                if (get32msb(bufD, bufP + 0) != 0x01010001) goto drop;
                sgt = get16msb(bufD, bufP + 4);
                ethtyp = get16msb(bufD, bufP + 6);
                bufP += 8;
            }
            if (vrfp->sgtSet >= 0) {
                sgt = vrfp->sgtSet;
            }
            switch (vrfp->cmd) {
            case 1: // route
                break;
            case 2: // bridge
                tmp = vrfp->brdg;
                switch (ethtyp) {
                case ETHERTYPE_IPV4: // ipv4
                    if (vrfp->pmtud4 > 0) {
                        if ((bufE - bufD - bufP) > vrfp->pmtud4) goto punt;
                    }
                    break;
                case ETHERTYPE_IPV6: // ipv6
                    if (vrfp->pmtud6 > 0) {
                        if ((bufE - bufD - bufP) > vrfp->pmtud6) goto punt;
                    }
                    break;
                }
                goto bridge_rx;
            case 3: // xconn
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
            case 4: // loconnifc
                prt = vrfp->label1;
                bufP -= 2;
                put16msb(bufD, bufP, ethtyp);
                goto subif_tx;
            case 5: // loconnnei
                neik = vrfp->label1;
                goto ethtyp_tx;
            default:
                break;
            }
        }

        switch (ethtyp) {
        case ETHERTYPE_MPLS_UCAST:
            if (vrfp == NULL) goto drop;
            if (vrfp->mpls == 0) goto drop;
            revalidatePacket(bufP + 12);
            __u32 label;
            struct label_res* resm;
            readMpls();
            switch (resm->cmd) {
            case 1:
                if ((tmp & 0x100) == 0) {
                    bufD[bufP + 3] = ttl + 1;
                    readMpls();
                    switch (resm->cmd) {
                    case 1:
                        if ((tmp & 0x100) == 0) goto drop;
                        routeMpls();
                        switchMpls();
                    }
                }
                routeMpls();
                switchMpls();
            }
            goto drop;
        case ETHERTYPE_IPV4:
            if (vrfp == NULL) goto drop;
ipv4_rx:
            revalidatePacket(bufP + 20);
            if ((bufD[bufP + 0] & 0xf0) != 0x40) goto drop;
            if ((bufD[bufP + 0] & 0xf) < 5) goto drop;
            if (vrfp->pmtud4 > 0) {
                if ((bufE - bufD - bufP) > vrfp->pmtud4) goto punt;
            }
            ttl = bufD[bufP + 8] - 1;
            if (ttl <= 1) goto punt;
            if (bufD[bufP + 9] == 46) goto cpu;
            bufD[bufP + 8] = ttl;
            update_chksum(bufP + 10, -1);
            ttl |= vrfp->pttl4;
            struct route4_key rou4;
            rou4.bits = (sizeof(rou4) * 8) - routes_bits;
            rou4.vrf = vrfp->vrf;
            if (vrfp->verify4 > 0) {
                __builtin_memcpy(rou4.addr, &bufD[bufP + 12], sizeof(rou4.addr));
                struct routes_res* res4 = bpf_map_lookup_elem(&routes4, &rou4);
                if (res4 == NULL) goto punt;
                if (vrfp->verify4 > 1) {
                    neik = res4->hop;
                    struct neigh_res* neir = bpf_map_lookup_elem(&neighs, &neik);
                    if (neir == NULL) goto punt;
                    if (neir->aclport != prt) goto punt;
                }
            }
            __builtin_memcpy(rou4.addr, &bufD[bufP + 16], sizeof(rou4.addr));
            struct routes_res* res4 = bpf_map_lookup_elem(&routes4, &rou4);
            if (res4 == NULL) goto punt;
            hash ^= get32msb(bufD, bufP + 12);
            hash ^= get32msb(bufD, bufP + 16);
            doRouted(res4);
            struct tunnel4_key tun4;
            tun4.vrf = vrfp->vrf;
            tun4.prot = bufD[bufP + 9];
            __builtin_memcpy(tun4.srcAddr, &bufD[bufP + 12], sizeof(tun4.srcAddr));
            __builtin_memcpy(tun4.trgAddr, &bufD[bufP + 16], sizeof(tun4.trgAddr));
            bufP += 20;
            extract_layer4(tun4);
            struct tunnel_res* tunr = bpf_map_lookup_elem(&tunnels4, &tun4);
            doTunnel();
        case ETHERTYPE_IPV6:
            if (vrfp == NULL) goto drop;
ipv6_rx:
            revalidatePacket(bufP + 40);
            if ((bufD[bufP + 0] & 0xf0) != 0x60) goto drop;
            if (vrfp->pmtud6 > 0) {
                if ((bufE - bufD - bufP) > vrfp->pmtud6) goto punt;
            }
            ttl = bufD[bufP + 7] - 1;
            if (ttl <= 1) goto punt;
            if (bufD[bufP + 6] == 0) goto cpu;
            bufD[bufP + 7] = ttl;
            ttl |= vrfp->pttl6;
            struct route6_key rou6;
            rou6.bits = (sizeof(rou6) * 8) - routes_bits;
            rou6.vrf = vrfp->vrf;
            if (vrfp->verify6 > 0) {
                __builtin_memcpy(rou6.addr, &bufD[bufP + 8], sizeof(rou6.addr));
                struct routes_res* res6 = bpf_map_lookup_elem(&routes6, &rou6);
                if (res6 == NULL) goto punt;
                if (vrfp->verify6 > 1) {
                    neik = res6->hop;
                    struct neigh_res* neir = bpf_map_lookup_elem(&neighs, &neik);
                    if (neir == NULL) goto punt;
                    if (neir->aclport != prt) goto punt;
                }
            }
            __builtin_memcpy(rou6.addr, &bufD[bufP + 24], sizeof(rou6.addr));
            struct routes_res* res6 = bpf_map_lookup_elem(&routes6, &rou6);
            if (res6 == NULL) goto punt;
            hash ^= get32msb(bufD, bufP + 8);
            hash ^= get32msb(bufD, bufP + 12);
            hash ^= get32msb(bufD, bufP + 16);
            hash ^= get32msb(bufD, bufP + 20);
            hash ^= get32msb(bufD, bufP + 24);
            hash ^= get32msb(bufD, bufP + 28);
            hash ^= get32msb(bufD, bufP + 32);
            hash ^= get32msb(bufD, bufP + 36);
            doRouted(res6);
            struct tunnel6_key tun6;
            tun6.vrf = vrfp->vrf;
            tun6.prot = bufD[bufP + 6];
            __builtin_memcpy(tun6.srcAddr, &bufD[bufP + 8], sizeof(tun6.srcAddr));
            __builtin_memcpy(tun6.trgAddr, &bufD[bufP + 24], sizeof(tun6.trgAddr));
            bufP += 40;
            extract_layer4(tun6);
            tunr = bpf_map_lookup_elem(&tunnels6, &tun6);
            doTunnel();
        case ETHERTYPE_VLAN:
            revalidatePacket(bufP + 4);
            struct vlan_key vlnk;
            vlnk.port = prt;
            vlnk.vlan = get16msb(bufD, bufP) & 0xfff;
            bufP += 2;
            ethtyp = get16msb(bufD, bufP);
            bufP += 2;
            __u32* res = bpf_map_lookup_elem(&vlan_in, &vlnk);
            if (res == NULL) goto drop;
            prt = *res;
            if (bpf_xdp_adjust_head(ctx, bufP - sizeof(macaddr) - 2) != 0) goto drop;
            continue;
        case ETHERTYPE_PPPOE_DATA:
            revalidatePacket(bufP + 12);
            struct pppoe_key pppk;
            pppk.port = prt;
            pppk.sess = get16msb(bufD, bufP + 2);
            res = bpf_map_lookup_elem(&pppoes, &pppk);
            if (res == NULL) goto drop;
            ethtyp = get16msb(bufD, bufP + 6);
            bufP += 8;
            ppptyp2ethtyp();
            put16msb(bufD, bufP - 2, ethtyp);
            prt = *res;
            if (bpf_xdp_adjust_head(ctx, bufP - sizeof(macaddr) - 2) != 0) goto drop;
            continue;
        case ETHERTYPE_PPPOE_CTRL:
            goto cpu;
        case ETHERTYPE_ARP:
            goto cpu;
        case ETHERTYPE_LACP:
            goto cpu;
        case ETHERTYPE_LLDP:
            goto cpu;
        default:
            if (ethtyp < 1500) goto cpu;
            goto punt;
        }

bridge_rx:
        {}
        struct bridge_key brdk;
        __builtin_memcpy(brdk.mac, &macaddr[6], 6);
        brdk.id = tmp;
        brdk.mac[6] = 0;
        brdk.mac[7] = 0;
        struct bridge_res* brdr = bpf_map_lookup_elem(&bridges, &brdk);
        if (brdr == NULL) goto cpu;
        brdr->packRx++;
        brdr->byteRx += bufE - bufD;
        __builtin_memcpy(brdk.mac, &macaddr[0], 6);
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
        __builtin_memcpy(&macaddr[0], neir->macs, sizeof(neir->macs));
        prt = neir->aclport;
        vrfp = bpf_map_lookup_elem(&vrf_port, &prt);
        if ((neir->cmd > 1) && (vrfp != NULL)) {
            vrfp->packTx++;
            vrfp->byteTx += bufE - bufD;
            putSgt();
        }
        prt = neir->port;
        switch (neir->cmd) {
        case 1: // raw
            break;
        case 2: // pppoe
            ethtyp2ppptyp();
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
        case 3: // gre4
            bufP -= sizeof(macaddr) + 24;
            if (bpf_xdp_adjust_head(ctx, bufP) != 0) goto drop;
            bufP = sizeof(macaddr) + 24;
            revalidatePacket(bufP);
            putGreHeader();
            putIpv4header(IP_PROTOCOL_GRE);
            break;
        case 4: // gre6
            bufP -= sizeof(macaddr) + 44;
            if (bpf_xdp_adjust_head(ctx, bufP) != 0) goto drop;
            bufP = sizeof(macaddr) + 44;
            revalidatePacket(bufP);
            putGreHeader();
            putIpv6header(IP_PROTOCOL_GRE);
            break;
        case 5: // l2tp4
            bufP -= sizeof(macaddr) + 40;
            if (bpf_xdp_adjust_head(ctx, bufP) != 0) goto drop;
            bufP = sizeof(macaddr) + 40;
            revalidatePacket(bufP + 2);
            ethtyp2ppptyp();
            putL2tpHeader();
            putUdpHeader();
            putIpv4header(IP_PROTOCOL_UDP);
            break;
        case 6: // l2tp6
            bufP -= sizeof(macaddr) + 60;
            if (bpf_xdp_adjust_head(ctx, bufP) != 0) goto drop;
            bufP = sizeof(macaddr) + 60;
            revalidatePacket(bufP + 2);
            ethtyp2ppptyp();
            putL2tpHeader();
            putUdpHeader();
            putIpv6header(IP_PROTOCOL_UDP);
            break;
        default:
            goto drop;
        }

subif_tx:
        vrfp = bpf_map_lookup_elem(&vrf_port, &prt);
        if (vrfp != NULL) {
            vrfp->packTx++;
            vrfp->byteTx += bufE - bufD;
            putSgt();
        }
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
            tmp = bunr->cmd;
        } else tmp = 1;
        bufP -= sizeof(macaddr);
        if (bpf_xdp_adjust_head(ctx, bufP) != 0) goto drop;
        bufP = 0;
        revalidatePacket(sizeof(macaddr));
        __builtin_memcpy(bufD, &macaddr, sizeof(macaddr));
        if (tmp != 2) goto done;

    }

done:
    {}
    struct port_res* txport = bpf_map_lookup_elem(&tx_ports, &prt);
    if (txport == NULL) goto drop;
    txport->pack++;
    txport->byte += bufE - bufD;
    return bpf_redirect(txport->idx, 0);

punt:
    tmp = 1;
    __u32* remain = bpf_map_lookup_elem(&cpu_port, &tmp);
    if (remain == NULL) goto drop;
    if (*remain < 1) goto drop;
    (*remain)--;

#endif

cpu:
    if (bpf_xdp_adjust_head(ctx, -2) != 0) goto drop;
    revalidatePacket(sizeof(macaddr) + 2);
    put16msb(bufD, 0, prt);
    __builtin_memcpy(&bufD[2], &macaddr, sizeof(macaddr));
    return bpf_redirect(*cpuPort, 0);
drop:
    return XDP_DROP;
}
