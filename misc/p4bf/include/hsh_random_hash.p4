/* -*- P4_16 -*- */

/*
 * random_hash.p4
 *
 * This file is intended to be included by simple_l3_lag_ecmp.p4 and provides
 * hash calculation, based on IPv4/IPv6 headers
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
#define SCRAMBLER_ENABLE 1
#endif

control calc_rng(out bit<32> random_number)
{
    Random<bit<32>>() rng;

    action get_random() {
        random_number = rng.get();
    }

    table random {
        actions = { get_random; }
        default_action = get_random();
        size = 1;
    }

    apply {
        random.apply();
    }
}

control calc_common_hashes(
    in    headers   hdr,
    in    ingress_metadata_t  ig_md,
    inout selector_hash_t        hash)
{
    calc_rng()     rng1;
#if HASH_WIDTH > 32
    calc_rng()     rng2;
#if HASH_WIDTH > 64
    calc_rng()     rng3;
#endif
#endif

    apply {
        rng1.apply(hash[31:0]);
#if HASH_WIDTH > 32
        rng2.apply(hash[63:32]);
#if HASH_WIDTH > 64
        rng3.apply(hash[95:64]);
#endif
#endif
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
