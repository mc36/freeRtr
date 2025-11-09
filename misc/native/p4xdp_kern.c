#include <linux/bpf.h>
#include <bpf/bpf_helpers.h>

#include "utils.h"
#include "types.h"
#include "p4xdp_tab.h"



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


struct {
    __uint(type, BPF_MAP_TYPE_HASH);
    __type(key, struct nsh_key);
    __type(value, struct nsh_res);
    __uint(max_entries, 512);
} nshs SEC(".maps");


struct {
    __uint(type, BPF_MAP_TYPE_HASH);
    __type(key, __u32);
    __type(value, struct polpol_res);
    __uint(max_entries, 512);
} polpols SEC(".maps");


struct {
    __uint(type, BPF_MAP_TYPE_HASH);
    __type(key, struct polidx_key);
    __type(value, __u32);
    __uint(max_entries, 512);
} polidxs SEC(".maps");




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
        neik = res->nexthop;                                        \
        goto ethtyp_tx;                                             \
    case 2:                                                         \
        break;                                                      \
    case 3:                                                         \
        bufP -= 4;                                                  \
        tmp = (res->label1 << 12) | 0x100 | ttl;                    \
        put32msb(bufD, bufP, tmp);                                  \
        ethtyp = ETHERTYPE_MPLS_UCAST;                              \
        neik = res->nexthop;                                        \
        goto ethtyp_tx;                                             \
    case 4:                                                         \
        bufP -= 4;                                                  \
        tmp = (res->label2 << 12) | 0x100 | ttl;                    \
        put32msb(bufD, bufP, tmp);                                  \
        bufP -= 4;                                                  \
        tmp = (res->label1 << 12) | ttl;                            \
        put32msb(bufD, bufP, tmp);                                  \
        ethtyp = ETHERTYPE_MPLS_UCAST;                              \
        neik = res->nexthop;                                        \
        goto ethtyp_tx;                                             \
    case 5:                                                         \
        goto drop;                                                  \
    case 6:                                                         \
        bufP -= 3 * sizeof(macaddr);                                \
        if (bpf_xdp_adjust_head(ctx, bufP) != 0) goto drop;         \
        bufP = 3 * sizeof(macaddr);                                 \
        revalidatePacket(3 * sizeof(macaddr));                      \
        bufP -= 20;                                                 \
        bufD[bufP + 0] = 0;                                         \
        bufD[bufP + 1] = ttl;                                       \
        put16msb(bufD, bufP + 2, ethtyp);                           \
        __builtin_memcpy(&bufD[bufP + 4], res->polka, 16);          \
        neik = res->nexthop;                                        \
        ethtyp = ETHERTYPE_POLKA;                                   \
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
        neik = resm->nexthop;                                       \
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
        neik = resm->nexthop;                                       \
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
        tmp = resm->port;                                           \
        goto bridge_rx;                                             \
    case 6:                                                         \
        bufP -= 4;                                                  \
        label = (tmp & 0xf00) | ttl | (resm->push << 12);           \
        put32msb(bufD, bufP, label);                                \
        bufP -= 4;                                                  \
        label = (tmp & 0xe00) | ttl | (resm->swap << 12);           \
        put32msb(bufD, bufP, label);                                \
        neik = resm->nexthop;                                       \
        goto ethtyp_tx;                                             \
    case 7:                                                         \
        goto cpu;                                                   \
    case 8:                                                         \
        if (bpf_xdp_adjust_head(ctx, bufP) != 0) goto drop;         \
        revalidatePacket(14);                                       \
        __builtin_memcpy(&macaddr[0], &bufD[0], 12);                \
        prt = resm->port;                                           \
        continue;                                                   \
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
    case ETHERTYPE_NSH:                                         \
        ethtyp = PPPTYPE_NSH;                                   \
        break;                                                  \
    case ETHERTYPE_ROUTEDMAC:                                   \
        bufP -= 2;                                              \
        put16msb(bufD, bufP, 1);                                \
        ethtyp = PPPTYPE_ROUTEDMAC;                             \
        break;                                                  \
    case ETHERTYPE_POLKA:                                       \
        ethtyp = PPPTYPE_POLKA;                                 \
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
    case PPPTYPE_NSH:                                           \
        ethtyp = ETHERTYPE_NSH;                                 \
        break;                                                  \
    case PPPTYPE_ROUTEDMAC:                                     \
        ethtyp = ETHERTYPE_ROUTEDMAC;                           \
        bufP += 2;                                              \
        break;                                                  \
    case PPPTYPE_POLKA:                                         \
        ethtyp = ETHERTYPE_POLKA;                               \
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
            revalidatePacket(bufP + 14);                        \
            if ((get16msb(bufD, bufP) & 0x8000) != 0) goto cpu; \
            bufP += 8;                                          \
            bufP += 2;                                          \
            ethtyp = get16msb(bufD, bufP);                      \
            ppptyp2ethtyp();                                    \
            put16msb(bufD, bufP, ethtyp);                       \
            break;                                              \
        case 3:                                                 \
            bufP += 4;                                          \
            revalidatePacket(bufP + 4);                         \
            ethtyp = get16msb(bufD, bufP);                      \
            ppptyp2ethtyp();                                    \
            put16msb(bufD, bufP, ethtyp);                       \
            break;                                              \
        case 4:                                                 \
            bufP += 8;                                          \
            bufP += 8;                                          \
            revalidatePacket(bufP + 4);                         \
            guessEthtyp();                                      \
            bufP -= 2;                                          \
            put16msb(bufD, bufP, ethtyp);                       \
            break;                                              \
        case 5:                                                 \
            bufP += 8;                                          \
            revalidatePacket(bufP + 4);                         \
            bufP -= 2;                                          \
            ethtyp = ETHERTYPE_ROUTEDMAC;                       \
            put16msb(bufD, bufP, ethtyp);                       \
            break;                                              \
        case 6:                                                 \
            bufP += 8;                                          \
            bufP += 8;                                          \
            revalidatePacket(bufP + 4);                         \
            bufP -= 2;                                          \
            ethtyp = ETHERTYPE_ROUTEDMAC;                       \
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


#define putIpv4header(ntry, proto)                              \
    bufP -= 20;                                                 \
    put16msb(bufD, bufP + 0, 0x4500);                           \
    ethtyp = bufE - bufD  - bufP;                               \
    put16msb(bufD, bufP + 2, ethtyp);                           \
    put16msb(bufD, bufP + 4, 0);                                \
    put16msb(bufD, bufP + 6, 0);                                \
    bufD[bufP + 8] = 0xff;                                      \
    bufD[bufP + 9] = proto;                                     \
    ethtyp += 0x4500 + 0xff00 + proto;                          \
    ethtyp += get16msb(ntry->srcAddr, 0);                       \
    ethtyp += get16msb(ntry->srcAddr, 2);                       \
    ethtyp += get16msb(ntry->trgAddr, 0);                       \
    ethtyp += get16msb(ntry->trgAddr, 2);                       \
    ethtyp = (ethtyp >> 16) + (ethtyp & 0xffff);                \
    ethtyp += (ethtyp >> 16);                                   \
    ethtyp = 0xffff & (~ethtyp);                                \
    put16msb(bufD, bufP + 10, ethtyp);                          \
    __builtin_memcpy(&bufD[bufP + 12], &ntry->srcAddr, 4);      \
    __builtin_memcpy(&bufD[bufP + 16], &ntry->trgAddr, 4);      \
    ethtyp = ETHERTYPE_IPV4;                                    \
    bufP -= 2;                                                  \
    put16msb(bufD, bufP, ethtyp);



#define putIpv6header(ntry, proto)                              \
    bufP -= 40;                                                 \
    put16msb(bufD, bufP + 0, 0x6000);                           \
    put16msb(bufD, bufP + 2, 0);                                \
    ethtyp = bufE - bufD - bufP - 40;                           \
    put16msb(bufD, bufP + 4, ethtyp);                           \
    bufD[bufP + 6] = proto;                                     \
    bufD[bufP + 7] = 0xff;                                      \
    __builtin_memcpy(&bufD[bufP + 8], &ntry->srcAddr, 16);      \
    __builtin_memcpy(&bufD[bufP + 24], &ntry->trgAddr, 16);     \
    ethtyp = ETHERTYPE_IPV6;                                    \
    bufP -= 2;                                                  \
    put16msb(bufD, bufP, ethtyp);



#define putGreHeader()                                          \
    bufP -= 2;                                                  \
    put16msb(bufD, bufP, 0x0000);


#define putUdpHeader(ntry)                                      \
    bufP -= 8;                                                  \
    put16msb(bufD, bufP + 0, ntry->srcPort);                    \
    put16msb(bufD, bufP + 2, ntry->trgPort);                    \
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


#define putL3tpHeader()                                         \
    put16msb(bufD, bufP, ethtyp);                               \
    bufP -= 4;                                                  \
    put32msb(bufD, bufP + 0, neir->sess);


#define guessEthtyp()                                           \
    switch (bufD[bufP] & 0xf0) {                                \
        case 0x40:                                              \
            ethtyp = ETHERTYPE_IPV4;                            \
            break;                                              \
        case 0x60:                                              \
            ethtyp = ETHERTYPE_IPV6;                            \
            break;                                              \
    default:                                                    \
        goto drop;                                              \
    }



#define putGtpHeader()                                          \
    bufP -= 6;                                                  \
    put16msb(bufD, bufP + 0, 0x30ff);                           \
    ethtyp = bufE - bufD - bufP - 8;                            \
    put16msb(bufD, bufP + 2, ethtyp);                           \
    put32msb(bufD, bufP + 4, neir->sess);



#define putVxlanHeader()                                        \
    bufP -= 8;                                                  \
    put16msb(bufD, bufP + 0, 0x800);                            \
    put16msb(bufD, bufP + 2, 0);                                \
    tmp = brdr->label1 << 8;                                    \
    put32msb(bufD, bufP + 4, tmp);



#define crc16calc(tmp, tab, buf, ofs, len)                                      \
    tmp = 0;                                                                    \
    for (__u32 i = 0; i < len; i++) {                                           \
        tmp = ((tmp << 8) & 0xffff) ^ tab[((tmp >> 8) ^ buf[ofs + i]) & 0xff];  \
    }







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
        prt = get32msb(bufD, 0);
        struct port_res* txport = bpf_map_lookup_elem(&tx_ports, &prt);
        if (txport == NULL) goto drop;
        txport->pack++;
        txport->byte += bufE - bufD;
        if (bpf_xdp_adjust_head(ctx, 4) != 0) goto drop;
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

    for (__u32 rounds = 0; rounds < 4; rounds++) {

        __u64 bufP = sizeof(macaddr) + 2;
        revalidatePacket(bufP);
        __u32 ethtyp = get16msb(bufD, bufP - 2);
        __u32 neik = 0;
        __s32 ttl = 0;

        struct vrfp_res* vrfp = bpf_map_lookup_elem(&vrf_port, &prt);
        if (vrfp == NULL) goto etyped_rx;
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
            tmp = vrfp->bridge;
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
            case ETHERTYPE_ROUTEDMAC:
                goto etyped_rx;
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
            neik = vrfp->nexthop;
            goto ethtyp_tx;
        case 4: // loconnifc
            prt = vrfp->label1;
            bufP -= 2;
            put16msb(bufD, bufP, ethtyp);
            goto subif_tx;
        case 5: // loconnnei
            neik = vrfp->label1;
            goto ethtyp_tx;
        case 6: // nshconn
            bufP -= 2;
            put16msb(bufD, bufP, ethtyp);
            bufP -= 12;
            bufP -= 2 * sizeof(macaddr);
            if (bpf_xdp_adjust_head(ctx, bufP) != 0) goto drop;
            bufP = 2 * sizeof(macaddr);
            revalidatePacket(3 * sizeof(macaddr));
            __builtin_memcpy(&bufD[bufP], &macaddr[0], sizeof(macaddr));
            bufP -= 8;
            put16msb(bufD, bufP + 0, 0xfc2);
            put16msb(bufD, bufP + 2, 0x203);
            tmp = (vrfp->label1 << 8) | vrfp->label2;
            put32msb(bufD, bufP + 4, tmp);
            goto nsh_rx;
        default:
            break;
        }

etyped_rx:
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
                    neik = res4->nexthop;
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
                    neik = res6->nexthop;
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
            bufP += 4;
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
        case ETHERTYPE_NSH:
            if (vrfp == NULL) goto drop;
            if (vrfp->nsh == 0) goto drop;
nsh_rx:
            revalidatePacket(bufP + 8);
            ttl = get16msb(bufD, bufP + 0);
            if ((ttl & 0xe000) != 0) goto drop;
            if (((ttl >> 6) & 0x3f) <= 1) goto punt;
            tmp = get32msb(bufD, bufP + 4);
            ethtyp = bufD[bufP + 3];
            struct nsh_key nshk;
            nshk.sp = tmp >> 8;
            nshk.si = tmp & 0xff;
            hash ^= tmp;
            struct nsh_res *nshr = bpf_map_lookup_elem(&nshs, &nshk);
            if (nshr == NULL) goto drop;
            nshr->pack++;
            nshr->byte += bufE - bufD;
            switch (nshr->cmd) {
            case 1:
                ttl = ttl - 0x40;
                put16msb(bufD, bufP + 0, ttl);
                put32msb(bufD, bufP + 4, nshr->trg);
                ethtyp = ETHERTYPE_NSH;
                bufP -= 2;
                put16msb(bufD, bufP, ethtyp);
                __builtin_memcpy(&macaddr[0], &nshr->macs, 12);
                prt = nshr->port;
                goto subif_tx;
            case 2:
                bufP += 8;
                switch (ethtyp) {
                case 5:
                    ethtyp = ETHERTYPE_MPLS_UCAST;
                    break;
                case 1:
                    ethtyp = ETHERTYPE_IPV4;
                    break;
                case 2:
                    ethtyp = ETHERTYPE_IPV6;
                    break;
                case 3:
                    if (bpf_xdp_adjust_head(ctx, bufP) != 0) goto drop;
                    continue;
                }
                revalidatePacket(bufP + 2);
                bufP -= 2;
                put16msb(bufD, bufP, ethtyp);
                bufP -= sizeof(macaddr);
                __builtin_memcpy(&bufD[bufP], &macaddr[0], sizeof(macaddr));
                if (bpf_xdp_adjust_head(ctx, bufP) != 0) goto drop;
                continue;
            case 3:
                ttl = ttl - 0x40;
                put16msb(bufD, bufP + 0, ttl);
                put32msb(bufD, bufP + 4, nshr->trg);
                ethtyp = ETHERTYPE_NSH;
                neik = nshr->port;
                goto ethtyp_tx;
            }
            goto drop;
        case ETHERTYPE_POLKA:
            if (vrfp == NULL) goto drop;
            revalidatePacket(bufP + 20);
            struct polpol_res *polr = bpf_map_lookup_elem(&polpols, &prt);
            if (polr == NULL) goto drop;
            polr->pack++;
            polr->byte += bufE - bufD;
            ttl = bufD[bufP + 1];
            if ((ttl & 0xff) <= 1) goto punt;
            ttl--;
            bufD[bufP + 1] = ttl;
            crc16calc(tmp, polr->tab, bufD, bufP + 4, 14);
            tmp ^= get16msb(bufD, bufP + 18);
            if (tmp == 0) {
                ethtyp = get16msb(bufD, bufP + 2);
                bufP += 20;
                bufP -= 2;
                put16msb(bufD, bufP, ethtyp);
                bufP -= sizeof(macaddr);
                __builtin_memcpy(&bufD[bufP], &macaddr[0], sizeof(macaddr));
                if (bpf_xdp_adjust_head(ctx, bufP) != 0) goto drop;
                continue;
            }
            struct polidx_key polk;
            polk.vrf = vrfp->vrf;
            polk.idx = tmp;
            res = bpf_map_lookup_elem(&polidxs, &polk);
            if (res == NULL) goto drop;
            ethtyp = ETHERTYPE_POLKA;
            neik = *res;
            goto ethtyp_tx;
        case ETHERTYPE_ROUTEDMAC:
            if (vrfp == NULL) goto drop;
            if (vrfp->cmd != 2) goto drop;
            revalidatePacket(bufP + 20);
            __builtin_memcpy(&macaddr[0], &bufD[bufP], 12);
            bufP += 12;
            ethtyp = get16msb(bufD, bufP);
            bufP += 2;
            tmp = vrfp->bridge;
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
            bufP -= 2;
            put16msb(bufD, bufP, ethtyp);
            switch (brdr->cmd) {
            case 1: // port
                prt = brdr->port;
                goto subif_tx;
            case 2: // vpls
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
                neik = brdr->nexthop;
                goto ethtyp_tx;
            case 3: // routed
                bufP -= 12;
                bufP -= 2 * sizeof(macaddr);
                if (bpf_xdp_adjust_head(ctx, bufP) != 0) goto drop;
                bufP = 2 * sizeof(macaddr);
                revalidatePacket(3 * sizeof(macaddr));
                __builtin_memcpy(&bufD[bufP], &macaddr[0], sizeof(macaddr));
                ethtyp = ETHERTYPE_ROUTEDMAC;
                neik = brdr->nexthop;
                goto ethtyp_tx;
            case 4: // pckoudp4
                bufP -= 12;
                bufP -= sizeof(macaddr) + 26;
                if (bpf_xdp_adjust_head(ctx, bufP) != 0) goto drop;
                bufP = sizeof(macaddr) + 26;
                revalidatePacket(bufP + sizeof(macaddr));
                __builtin_memcpy(&bufD[bufP], &macaddr[0], sizeof(macaddr));
                putUdpHeader(brdr);
                putIpv4header(brdr, IP_PROTOCOL_UDP);
                bufP += 2;
                neik = brdr->nexthop;
                goto ethtyp_tx;
            case 5: // pckoudp6
                bufP -= 12;
                bufP -= sizeof(macaddr) + 46;
                if (bpf_xdp_adjust_head(ctx, bufP) != 0) goto drop;
                bufP = sizeof(macaddr) + 46;
                revalidatePacket(bufP + sizeof(macaddr));
                __builtin_memcpy(&bufD[bufP], &macaddr[0], sizeof(macaddr));
                putUdpHeader(brdr);
                putIpv6header(brdr, IP_PROTOCOL_UDP);
                bufP += 2;
                neik = brdr->nexthop;
                goto ethtyp_tx;
            case 6: // vxlan4
                bufP -= 12;
                bufP -= sizeof(macaddr) + 36;
                if (bpf_xdp_adjust_head(ctx, bufP) != 0) goto drop;
                bufP = sizeof(macaddr) + 36;
                revalidatePacket(bufP + sizeof(macaddr));
                __builtin_memcpy(&bufD[bufP], &macaddr[0], sizeof(macaddr));
                putVxlanHeader();
                putUdpHeader(brdr);
                putIpv4header(brdr, IP_PROTOCOL_UDP);
                bufP += 2;
                neik = brdr->nexthop;
                goto ethtyp_tx;
            case 7: // vxlan6
                bufP -= 12;
                bufP -= sizeof(macaddr) + 56;
                if (bpf_xdp_adjust_head(ctx, bufP) != 0) goto drop;
                bufP = sizeof(macaddr) + 56;
                revalidatePacket(bufP + sizeof(macaddr));
                __builtin_memcpy(&bufD[bufP], &macaddr[0], sizeof(macaddr));
                putVxlanHeader();
                putUdpHeader(brdr);
                putIpv6header(brdr, IP_PROTOCOL_UDP);
                bufP += 2;
                neik = brdr->nexthop;
                goto ethtyp_tx;
            default:
                goto drop;
            }
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
            bufP -= sizeof(macaddr) + 26;
            if (bpf_xdp_adjust_head(ctx, bufP) != 0) goto drop;
            bufP = sizeof(macaddr) + 26;
            revalidatePacket(bufP);
            putGreHeader();
            putIpv4header(neir, IP_PROTOCOL_GRE);
            break;
        case 4: // gre6
            bufP -= sizeof(macaddr) + 46;
            if (bpf_xdp_adjust_head(ctx, bufP) != 0) goto drop;
            bufP = sizeof(macaddr) + 46;
            revalidatePacket(bufP);
            putGreHeader();
            putIpv6header(neir, IP_PROTOCOL_GRE);
            break;
        case 5: // l2tp4
            bufP -= sizeof(macaddr) + 42;
            if (bpf_xdp_adjust_head(ctx, bufP) != 0) goto drop;
            bufP = sizeof(macaddr) + 42;
            revalidatePacket(bufP + 2);
            ethtyp2ppptyp();
            putL2tpHeader();
            putUdpHeader(neir);
            putIpv4header(neir, IP_PROTOCOL_UDP);
            break;
        case 6: // l2tp6
            bufP -= sizeof(macaddr) + 62;
            if (bpf_xdp_adjust_head(ctx, bufP) != 0) goto drop;
            bufP = sizeof(macaddr) + 62;
            revalidatePacket(bufP + 2);
            ethtyp2ppptyp();
            putL2tpHeader();
            putUdpHeader(neir);
            putIpv6header(neir, IP_PROTOCOL_UDP);
            break;
        case 7: // l3tp4
            bufP -= sizeof(macaddr) + 28;
            if (bpf_xdp_adjust_head(ctx, bufP) != 0) goto drop;
            bufP = sizeof(macaddr) + 28;
            revalidatePacket(bufP + 2);
            ethtyp2ppptyp();
            putL3tpHeader();
            putIpv4header(neir, IP_PROTOCOL_L2TP);
            break;
        case 8: // l3tp6
            bufP -= sizeof(macaddr) + 48;
            if (bpf_xdp_adjust_head(ctx, bufP) != 0) goto drop;
            bufP = sizeof(macaddr) + 48;
            revalidatePacket(bufP + 2);
            ethtyp2ppptyp();
            putL3tpHeader();
            putIpv6header(neir, IP_PROTOCOL_L2TP);
            break;
        case 9: // gtp4
            bufP -= sizeof(macaddr) + 30;
            if (bpf_xdp_adjust_head(ctx, bufP) != 0) goto drop;
            bufP = sizeof(macaddr) + 30;
            revalidatePacket(bufP + 2);
            putGtpHeader();
            putUdpHeader(neir);
            putIpv4header(neir, IP_PROTOCOL_UDP);
            break;
        case 10: // gtp6
            bufP -= sizeof(macaddr) + 50;
            if (bpf_xdp_adjust_head(ctx, bufP) != 0) goto drop;
            bufP = sizeof(macaddr) + 50;
            revalidatePacket(bufP + 2);
            putGtpHeader();
            putUdpHeader(neir);
            putIpv6header(neir, IP_PROTOCOL_UDP);
            break;
        case 11: // pwhe
            bufP -= sizeof(macaddr) + 16;
            if (bpf_xdp_adjust_head(ctx, bufP) != 0) goto drop;
            bufP = sizeof(macaddr) + 16;
            revalidatePacket(bufP + 2);
            bufP -= sizeof(macaddr);
            __builtin_memcpy(&bufD[bufP], &macaddr, sizeof(macaddr));
            bufP -= 4;
            tmp = 0x1ff | (neir->trgPort << 12);
            put32msb(bufD, bufP, tmp);
            bufP -= 4;
            tmp = 0xff | (neir->srcPort << 12);
            put32msb(bufD, bufP, tmp);
            ethtyp = ETHERTYPE_MPLS_UCAST;
            bufP -= 2;
            put16msb(bufD, bufP, ethtyp);
            __builtin_memcpy(&macaddr[0], neir->mac2, sizeof(neir->mac2));
            break;
        case 12: // labels
            bufP -= sizeof(macaddr) + 44;
            if (bpf_xdp_adjust_head(ctx, bufP) != 0) goto drop;
            bufP = sizeof(macaddr) + 44;
            revalidatePacket(bufP + 2);
            bufP += 2;
            if (neir->sess >= 1) {
                ethtyp = ETHERTYPE_MPLS_UCAST;
                bufP -= 4;
                tmp = 0x1ff | (neir->trgPort << 12);
                put32msb(bufD, bufP, tmp);
            }
            if (neir->sess >= 2) {
                bufP -= 4;
                tmp = 0xff | (neir->srcPort << 12);
                put32msb(bufD, bufP, tmp);
            }
            if (neir->sess >= 3) {
                bufP -= 4;
                tmp = 0xff | (*((int*)&neir->srcAddr[0]) << 12);
                put32msb(bufD, bufP, tmp);
            }
            if (neir->sess >= 4) {
                bufP -= 4;
                tmp = 0xff | (*((int*)&neir->srcAddr[4]) << 12);
                put32msb(bufD, bufP, tmp);
            }
            if (neir->sess >= 5) {
                bufP -= 4;
                tmp = 0xff | (*((int*)&neir->srcAddr[8]) << 12);
                put32msb(bufD, bufP, tmp);
            }
            if (neir->sess >= 6) {
                bufP -= 4;
                tmp = 0xff | (*((int*)&neir->srcAddr[12]) << 12);
                put32msb(bufD, bufP, tmp);
            }
            if (neir->sess >= 7) {
                bufP -= 4;
                tmp = 0xff | (*((int*)&neir->trgAddr[0]) << 12);
                put32msb(bufD, bufP, tmp);
            }
            if (neir->sess >= 8) {
                bufP -= 4;
                tmp = 0xff | (*((int*)&neir->trgAddr[4]) << 12);
                put32msb(bufD, bufP, tmp);
            }
            if (neir->sess >= 9) {
                bufP -= 4;
                tmp = 0xff | (*((int*)&neir->trgAddr[8]) << 12);
                put32msb(bufD, bufP, tmp);
            }
            if (neir->sess >= 10) {
                bufP -= 4;
                tmp = 0xff | (*((int*)&neir->trgAddr[12]) << 12);
                put32msb(bufD, bufP, tmp);
            }
            bufP -= 2;
            put16msb(bufD, bufP, ethtyp);
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
            if (vlnr->vlan2 > 0) {
                prt = vlnr->port2;
                vrfp = bpf_map_lookup_elem(&vrf_port, &prt);
                if (vrfp != NULL) {
                    vrfp->packTx++;
                    vrfp->byteTx += bufE - bufD;
                    putSgt();
                }
                bufP -= sizeof(macaddr);
                if (bpf_xdp_adjust_head(ctx, bufP) != 0) goto drop;
                bufP = sizeof(macaddr);
                revalidatePacket(bufP + 2);
                hash ^= vlnr->vlan2;
                bufP -= 2;
                put16msb(bufD, bufP, vlnr->vlan2);
                bufP -= 2;
                put16msb(bufD, bufP, ETHERTYPE_VLAN);
            }
            prt = vlnr->port;
            vlnr->pack++;
            vlnr->byte += bufE - bufD;
            vrfp = bpf_map_lookup_elem(&vrf_port, &prt);
            if (vrfp != NULL) {
                vrfp->packTx++;
                vrfp->byteTx += bufE - bufD;
                putSgt();
            }
        }
        bufP -= sizeof(macaddr);
        if (bpf_xdp_adjust_head(ctx, bufP) != 0) goto drop;
        revalidatePacket(sizeof(macaddr));
        __builtin_memcpy(bufD, &macaddr, sizeof(macaddr));
        struct bundle_res* bunr = bpf_map_lookup_elem(&bundles, &prt);
        if (bunr == NULL) goto done;
        bunr->pack++;
        bunr->byte += bufE - bufD;
        hash = ((hash >> 16) ^ hash) & 0xffff;
        hash = ((hash >> 8) ^ hash) & 0xff;
        hash = ((hash >> 4) ^ hash) & 0xf;
        prt = bunr->out[hash];
        if (bunr->cmd != 2) goto done;

    }

    goto drop;

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
    if (bpf_xdp_adjust_head(ctx, -4) != 0) goto drop;
    revalidatePacket(sizeof(macaddr) + 4);
    put32msb(bufD, 0, prt);
    __builtin_memcpy(&bufD[4], &macaddr, sizeof(macaddr));
    return bpf_redirect(*cpuPort, 0);
drop:
    return XDP_DROP;
}
