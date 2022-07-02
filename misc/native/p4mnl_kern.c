#include <linux/bpf.h>
#include <bpf/bpf_helpers.h>

#include "utils.h"
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
    revalidatePacket(2);

    __u32 prt = 0;
    __u32* cpu = bpf_map_lookup_elem(&cpu_port, &prt);
    if (cpu == NULL) goto drop;

    if (ctx->ingress_ifindex == *cpu) {
        prt = get16msb(bufD, 0);
        __u32* res = bpf_map_lookup_elem(&tx_ports, &prt);
        if (res == NULL) goto drop;
        if (bpf_xdp_adjust_head(ctx, 2) != 0) goto drop;
        return bpf_redirect(*res, 0);
    } else {
        prt = ctx->ingress_ifindex;
        __u32* res = bpf_map_lookup_elem(&rx_ports, &prt);
        if (res == NULL) goto drop;
        if (bpf_xdp_adjust_head(ctx, -2) != 0) goto drop;
        revalidatePacket(2);
        put16msb(bufD, 0, *res);
        return bpf_redirect(*cpu, 0);
    }

drop:
    return XDP_DROP;
}
