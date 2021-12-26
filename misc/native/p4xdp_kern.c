#include <linux/bpf.h>
#include <bpf/bpf_helpers.h>
#include <bpf/bpf_endian.h>
#include "utils.h"
#include "types.h"
#include "p4xdp_str.h"

struct {
    __uint(type, BPF_MAP_TYPE_ARRAY);
    __type(key, int);
    __type(value, int);
    __uint(max_entries, 2);
} cpu_port SEC(".maps");

struct {
    __uint(type, BPF_MAP_TYPE_HASH);
    __type(key, int);
    __type(value, struct port_entry);
    __uint(max_entries, MAX_PORTS);
} tx_ports SEC(".maps");

struct {
    __uint(type, BPF_MAP_TYPE_HASH);
    __type(key, int);
    __type(value, struct port_entry);
    __uint(max_entries, MAX_PORTS);
} rx_ports SEC(".maps");

struct {
    __uint(type, BPF_MAP_TYPE_HASH);
    __type(key, int);
    __type(value, struct vrfp_entry);
    __uint(max_entries, MAX_PORTS);
} vrf_port SEC(".maps");

struct {
    __uint(type, BPF_MAP_TYPE_HASH);
    __type(key, int);
    __type(value, struct neigh_res);
    __uint(max_entries, MAX_NEIGHS);
} neighs SEC(".maps");

struct {
    __uint(type, BPF_MAP_TYPE_LPM_TRIE);
    __type(key, struct route4_key);
    __type(value, struct routes_res);
    __uint(max_entries, MAX_ROUTES4);
    __uint(map_flags, BPF_F_NO_PREALLOC);
} routes4 SEC(".maps");

struct {
    __uint(type, BPF_MAP_TYPE_LPM_TRIE);
    __type(key, struct route6_key);
    __type(value, struct routes_res);
    __uint(max_entries, MAX_ROUTES6);
    __uint(map_flags, BPF_F_NO_PREALLOC);
} routes6 SEC(".maps");

struct {
    __uint(type, BPF_MAP_TYPE_HASH);
    __type(key, int);
    __type(value, struct label_res);
    __uint(max_entries, MAX_NEIGHS);
} labels SEC(".maps");




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
    port = label & 0xf00;                                           \
    label = (label >> 12) & 0xfffff;                                \
    bufP += 4;                                                      \
    resm = bpf_map_lookup_elem(&labels, &label);                    \
    if (resm == NULL) goto drop;



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
        if ((port & 0x100) == 0) {                                  \
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
        label = (port & 0xf00) | ttl | (resm->swap << 12);          \
        put32msb(bufD, bufP, label);                                \
        neik = resm->hop;                                           \
        goto ethtyp_tx;                                             \
    default:                                                        \
        goto drop;




SEC("p4xdp_router")
int xdp_packet(struct xdp_md *ctx) {

    unsigned char* bufE;
    unsigned char* bufD;
    revalidatePacket(18);
    unsigned char macaddr[6 + 6];
    __builtin_memcpy(macaddr, &bufD[0], sizeof(macaddr));

    int i = 0;
    int* cpuport = bpf_map_lookup_elem(&cpu_port, &i);
    if (cpuport == NULL) goto drop;

    int port = ctx->ingress_ifindex;
    struct port_entry* rxport = bpf_map_lookup_elem(&rx_ports, &port);
    if (rxport == NULL) goto drop;
    rxport->packs++;
    rxport->bytes += bufE - bufD;
    if (port == *cpuport) {
        port = get16msb(bufD, 0);
        struct port_entry* txport = bpf_map_lookup_elem(&tx_ports, &port);
        if (txport == NULL) goto drop;
        txport->packs++;
        txport->bytes += bufE - bufD;
        if (bpf_xdp_adjust_head(ctx, 2) != 0) goto drop;
        return bpf_redirect(txport->idx, 0);
    }
    port = rxport->idx;

    int ethtyp = get16msb(bufD, sizeof(macaddr));
    int bufP = sizeof(macaddr) + 2;

    /////// vlan in

    struct vrfp_entry* vrfp = bpf_map_lookup_elem(&vrf_port, &port);
    if (vrfp == NULL) goto drop;

    int neik;
    int ttl;
    switch (ethtyp) {
    case ETHERTYPE_MPLS_UCAST:
        revalidatePacket(bufP + 12);
        int label;
        struct label_res* resm;
        readMpls();
        switch (resm->cmd) {
        case 1:
            if ((port & 0x100) == 0) {
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
        doRouted(res6);
    case ETHERTYPE_ARP:
        goto cpu;
    default:
        goto punt;
    }


ethtyp_tx:
    bufP -= 2;
    put16msb(bufD, bufP, ethtyp);

    struct neigh_res* neir = bpf_map_lookup_elem(&neighs, &neik);
    if (neir == NULL) goto punt;
    __builtin_memcpy(&macaddr[0], neir->dmac, sizeof(neir->dmac));
    __builtin_memcpy(&macaddr[6], neir->smac, sizeof(neir->smac));
    port = neir->port;

    //// vlan out

    bufP -= sizeof(macaddr);
    if (bpf_xdp_adjust_head(ctx, bufP) != 0) goto drop;
    bufP = 0;
    revalidatePacket(sizeof(macaddr));
    __builtin_memcpy(bufD, &macaddr, sizeof(macaddr));

    struct port_entry* txport = bpf_map_lookup_elem(&tx_ports, &port);
    if (txport == NULL) goto drop;
    txport->packs++;
    txport->bytes += bufE - bufD;
    return bpf_redirect(txport->idx, 0);

punt:
    port = 1;
    int* remain = bpf_map_lookup_elem(&cpu_port, &port);
    if (remain == NULL) goto drop;
    (*remain)--;
    if (*remain < 1) goto drop;
cpu:
    port = 0;
    if (bpf_xdp_adjust_head(ctx, -2) != 0) goto drop;
    revalidatePacket(2);
    put16msb(bufD, 0, rxport->idx);
    return bpf_redirect(*cpuport, 0);
drop:
    return XDP_DROP;
}
