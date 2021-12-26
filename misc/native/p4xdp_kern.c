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
    __type(key, struct neigh_key);
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




#define revalidatePacket(size)                  \
    bufE = (unsigned char*)(long)ctx->data_end; \
    bufP = (unsigned char*)(long)ctx->data;     \
    if ((size + bufP) > bufE) goto drop;



SEC("p4xdp_router")
int xdp_packet(struct xdp_md *ctx) {

    unsigned char* bufE;
    unsigned char* bufP;
    revalidatePacket(18);
    unsigned char macaddr[6 + 6];
    __builtin_memcpy(macaddr, &bufP[0], sizeof(macaddr));

    int i = 0;
    int* cpuport = bpf_map_lookup_elem(&cpu_port, &i);
    if (cpuport == NULL) goto drop;

    int port = ctx->ingress_ifindex;
    struct port_entry* rxport = bpf_map_lookup_elem(&rx_ports, &port);
    if (rxport == NULL) goto drop;
    rxport->packs++;
    rxport->bytes += bufE - bufP;
    if (port == *cpuport) {
        port = get16msb(bufP, 0);
        struct port_entry* txport = bpf_map_lookup_elem(&tx_ports, &port);
        if (txport == NULL) goto drop;
        txport->packs++;
        txport->bytes += bufE - bufP;
        if (bpf_xdp_adjust_head(ctx, 2) != 0) goto drop;
        return bpf_redirect(txport->idx, 0);
    }
    port = rxport->idx;

    int ethtyp = get16msb(bufP, sizeof(macaddr));
    int bufO = sizeof(macaddr) + 2;

    /////// vlan in

    struct vrfp_entry* vrfp = bpf_map_lookup_elem(&vrf_port, &port);
    if (vrfp == NULL) goto drop;

    struct neigh_key neik;
    switch (ethtyp) {
    case ETHERTYPE_IPV4:
        revalidatePacket(bufO + 20);
        struct route4_key rou4;
        rou4.bits = (sizeof(rou4) * 8) - routes_bits;
        neik.vrf = rou4.vrf = vrfp->vrf;
        __builtin_memcpy(rou4.addr, &bufP[bufO + 16], sizeof(rou4.addr));
        struct routes_res* res4 = bpf_map_lookup_elem(&routes4, &rou4);
        if (res4 == NULL) goto punt;
        neik.id = res4->hop;
        port = res4->cmd;
        goto route;
    case ETHERTYPE_IPV6:
        revalidatePacket(bufO + 40);
        struct route6_key rou6;
        rou6.bits = (sizeof(rou6) * 8) - routes_bits;
        neik.vrf = rou6.vrf = vrfp->vrf;
        __builtin_memcpy(rou6.addr, &bufP[bufO + 24], sizeof(rou6.addr));
        struct routes_res* res6 = bpf_map_lookup_elem(&routes6, &rou6);
        if (res6 == NULL) goto punt;
        neik.id = res6->hop;
        port = res6->cmd;
        goto route;
    case ETHERTYPE_ARP:
        goto cpu;
    default:
        goto punt;
    }

route:
    switch (port) {
    case 1:
        goto neigh;
    case 2:
        goto cpu;
    default:
        goto drop;
    }

neigh:
    {
        struct neigh_res* neir = bpf_map_lookup_elem(&neighs, &neik);
        if (neir == NULL) goto punt;
        __builtin_memcpy(&macaddr[0], neir->dmac, sizeof(neir->dmac));
        __builtin_memcpy(&macaddr[6], neir->smac, sizeof(neir->smac));
        port = neir->port;
    }

    //// vlan out

    bufO -= sizeof(macaddr) + 2;
    if (bpf_xdp_adjust_head(ctx, bufO) != 0) goto drop;
    revalidatePacket(bufO + 2 + sizeof(macaddr));
    __builtin_memcpy(bufP + bufO, &macaddr, sizeof(macaddr));
    put16msb(bufP, bufO + sizeof(macaddr), ethtyp);

    struct port_entry* txport = bpf_map_lookup_elem(&tx_ports, &port);
    if (txport == NULL) goto drop;
    txport->packs++;
    txport->bytes += bufE - bufP;
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
    put16msb(bufP, 0, rxport->idx);
    return bpf_redirect(*cpuport, 0);
drop:
    return XDP_DROP;
}
