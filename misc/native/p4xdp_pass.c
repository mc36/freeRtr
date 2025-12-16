#include <linux/bpf.h>
#include <bpf/bpf_helpers.h>

SEC("p4xdp_pass")
int xdp_pass(struct xdp_md *ctx) {
    return XDP_PASS;
}
