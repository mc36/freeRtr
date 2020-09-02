/* -*- P4_16 -*- */


/*
 * round_robin_hash.p4
 *
 * This file is intended to be included by simple_l3_lag_ecmp.p4 and provides
 * hash calculation that results in round-robin distribution of traffic.
 * Assuming that all members of the selector group are up, the traffic is
 * distributed in a manner that the first packet goes to member 0, then
 * member 1 and so on.
 *
 * Resilient selection, obviously breaks this and supporting groups that
 * have more than 120 members is challenging (though not impossible). Thus,
 * for now we will only support non-resilient (fair) hashing mode and
 * MAX_GROUP_SIZE less than or equal to 120 members.
 *
 * Notice, that this particular program uses only one register to track packet
 * count, meaning that if you have packets going to more than one LAG/ECMP
 * group the distribution might not be correct. Can you fix the program?
 *
 * The hash module is supposed to define the following:
 *  1) Controls that will be called from the ingress() control to calculate
 *     hash. If they are not needed, then they should be empty
 *     1) calc_ipv4_hashes() -- called for IPv4 packets only
 *     2) calc_ipv6_hashes() -- called for IPv6 packets only
 *     3) calc_common_hashes() -- called for all packets
 *
 *  2) Optionally: SCRAMBLE_ENABLE. This module MUST disable the scrambler.
*/

#ifndef SCRAMBLE_ENABLE
#define SCRAMBLE_ENABLE 0
#elif SCRAMBLE_ENABLE != 0
#error "Round-Robin does not work when the scrambler is enabled"
#endif

#if RESILIENT_SELECTION != 0
#error "Round-Robin Mode does not currently support resilient selection"
#endif

#if MAX_GROUP_SIZE > 120
#error "Round-Robin Mode does not currently support groups with > 120 members"
#endif

control calc_common_hashes(
    in    headers   hdr,
    in    ingress_metadata_t  ig_md,
    inout selector_hash_t        hash)
{
    Register<bit<32>, bit<16>>(size=1, initial_value=0) packet_num;
    RegisterAction<bit<32>, bit<16>, bit<32>>(packet_num)
    increment = {
        void apply(inout bit<32> register_data, out bit<32> result) {
            result = register_data;
            register_data = register_data + 1;
        }
    };

    apply {
        hash[31:0] = increment.execute(0);
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
