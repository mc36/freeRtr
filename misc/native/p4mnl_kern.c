#include <linux/bpf.h>
#include <bpf/bpf_helpers.h>
#include <linux/ip.h>
#include <linux/ipv6.h>

#include "utils.h"
#include "types.h"
#include "p4mnl_tab.h"


// hack
#ifndef NULL
#define NULL ((void *)0)
#endif


struct {
    __uint(type, BPF_MAP_TYPE_ARRAY);
    __type(key, __u32);
    __type(value, __u32);
    __uint(max_entries, 1);
} cpu_port SEC(".maps");


struct {
    __uint(type, BPF_MAP_TYPE_HASH);
    __type(key, __u32);
    __type(value, __u32);
    __uint(max_entries, maxPorts);
} tx_ports SEC(".maps");


struct {
    __uint(type, BPF_MAP_TYPE_HASH);
    __type(key, __u32);
    __type(value, __u32);
    __uint(max_entries, maxPorts);
} rx_ports SEC(".maps");




#define revalidatePacket(size)                                      \
    bufE = (__u8*)(__u64)ctx->data_end;                             \
    bufD = (__u8*)(__u64)ctx->data;                                 \
    if (((__u64)bufD + (__u64)size) > (__u64)bufE) goto drop;



SEC("p4mnl_pckio")
__u32 mnl_pckio(struct xdp_md *ctx) {

    __u8* bufE;
    __u8* bufD;
    revalidatePacket(16);

    __u32 prt = 0;
    __u32* cpu = bpf_map_lookup_elem(&cpu_port, &prt);
    if (cpu == NULL) goto drop;

    if (ctx->ingress_ifindex == *cpu) {
        prt = get16msb(bufD, 0);
        __u32* res = bpf_map_lookup_elem(&tx_ports, &prt);
        if (res == NULL) goto drop;
        if (bpf_xdp_adjust_head(ctx, 2) != 0) goto drop;
        return bpf_redirect(*res, 0);
    }

    struct bpf_fib_lookup fibpar;
    __builtin_memset(&fibpar, 0, sizeof(fibpar));

    __u16 ethtyp = get16msb(bufD, 12);
    switch (ethtyp) {
    case ETHERTYPE_IPV4:
        revalidatePacket(34);
        fibpar.family = 2; // AF_INET
        struct iphdr *ip4h = (struct iphdr*)&bufD[14];
        __builtin_memcpy(&fibpar.ipv4_src, &ip4h->saddr, 4);
        __builtin_memcpy(&fibpar.ipv4_dst, &ip4h->daddr, 4);
        break;
    case ETHERTYPE_IPV6:
        revalidatePacket(54);
        fibpar.family = 10; // AF_INET6
        struct ipv6hdr *ip6h = (struct ipv6hdr*)&bufD[14];
        __builtin_memcpy(&fibpar.ipv6_src, &ip6h->saddr, 16);
        __builtin_memcpy(&fibpar.ipv6_dst, &ip6h->daddr, 16);
        break;
    default:
        goto punt;
    }
    fibpar.ifindex = ctx->ingress_ifindex;
    fibpar.tot_len = 64;
    fibpar.l4_protocol = IP_PROTOCOL_GRE;
    if (bpf_fib_lookup(ctx, &fibpar, sizeof(fibpar), 0) == BPF_FIB_LKUP_RET_SUCCESS) {
        return bpf_redirect(fibpar.ifindex, 0);
    }

punt:
    prt = ctx->ingress_ifindex;
    __u32* res = bpf_map_lookup_elem(&rx_ports, &prt);
    if (res == NULL) goto drop;
    if (bpf_xdp_adjust_head(ctx, -2) != 0) goto drop;
    revalidatePacket(2);
    put16msb(bufD, 0, *res);
    return bpf_redirect(*cpu, 0);

drop:
    return XDP_DROP;
}

char _license[] SEC("license") = "GPL";
