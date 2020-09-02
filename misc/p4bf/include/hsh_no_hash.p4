/* -*- P4_16 -*- */

/*
 * no_hash.p4
 *
 * This file is intended to be included by simple_l3_lag_ecmp.p4 and provides
 * no hash calculation. Rather it returns a constant value. You can use this
 * file as a stub to define your own algorithm or for debug.
 *
 * The hash module is supposed to define the following:
 *  1) Controls that will be called from the ingress() control to calculate
 *     hash. If they are not needed, then they should be empty
 *     1) calc_ipv4_hashes() -- called for IPv4 packets only
 *     2) calc_ipv6_hashes() -- called for IPv6 packets only
 *     3) calc_common_hashes() -- called for all packets
 *
 *  2) Optionally: SCRAMBLER_ENABLE
*/

#ifndef SCRAMBLER_ENABLE
#define SCRAMBLER_ENABLE 0
#endif

control calc_common_hashes(
    in    headers   hdr,
    in    ingress_metadata_t  ig_md,
    inout selector_hash_t        hash)
{
    apply {
        hash = 0;  /* It's better to initialize the hash */
    }
}

control calc_ipv4_hashes(
    in    headers   hdr,
    in    ingress_metadata_t  ig_md,
    inout selector_hash_t        hash)
{
    apply {}
}

control calc_ipv6_hashes(
    in    headers   hdr,
    in    ingress_metadata_t  ig_md,
    inout selector_hash_t        hash)
{
    apply {}
}
