#include <linux/bpf.h>
#include <bpf/bpf_helpers.h>

SEC("p4xdp_drop")
int xdp_drop(struct xdp_md *ctx) {
    return XDP_DROP;
}
