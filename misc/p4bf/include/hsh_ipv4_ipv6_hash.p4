/* -*- P4_16 -*- */

/*
 * ipv4_ipv6_hash.p4
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

control calc_common_hashes(
    in    headers   hdr,
    in    ingress_metadata_t  ig_md,
    inout selector_hash_t        hash)
{
    apply {}
}

control calc_ipv4_hash(
    in    headers   hdr,
    in    ingress_metadata_t  ig_md,
    out   bit<32>                hash)(
        CRCPolynomial<bit<32>>       poly)
{
    //@symmetric("hdr.ipv4.src_addr", "hdr.ipv4.dst_addr")
    Hash<bit<32>>(HashAlgorithm_t.CUSTOM, poly) hash_algo;

    action do_hash() {
        hash = hash_algo.get({
            hdr.ipv4.src_addr,
            hdr.ipv4.dst_addr,
            hdr.ipv4.protocol,
            ig_md.l4_lookup.word_1,
            ig_md.l4_lookup.word_2
        });
    }

    apply {
        do_hash();
    }
}

control calc_ipv4_hashes(
    in    headers   hdr,
    in    ingress_metadata_t  ig_md,
    inout selector_hash_t        hash)
{
    calc_ipv4_hash(CRCPolynomial<bit<32>>(
                       coeff=32w0x04C11DB7, reversed=true, msb=false, extended=false,
                       init=32w0xFFFFFFFF, xor=32w0xFFFFFFFF))
    hash1;

#if HASH_WIDTH > 32
    calc_ipv4_hash(CRCPolynomial<bit<32>>(
                       coeff=32w0x1EDC6F41, reversed=true, msb=false, extended=false,
                       init=32w0xFFFFFFFF, xor=32w0xFFFFFFFF))
    hash2;

#if HASH_WIDTH > 64
    calc_ipv4_hash(CRCPolynomial<bit<32>>(
                       coeff=32w0xA833982B, reversed=true, msb=false, extended=false,
                       init=32w0xFFFFFFFF, xor=32w0xFFFFFFFF))
    hash3;
#endif
#endif

    apply {
        hash1.apply(hdr, ig_md, hash[31:0]);
#if HASH_WIDTH > 32
        hash2.apply(hdr, ig_md, hash[63:32]);
#if HASH_WIDTH > 64
        hash3.apply(hdr, ig_md, hash[95:64]);
#endif
#endif
    }
}

/*
 * This control is written differently compared to calc_ipv4_hash --
 * instead of accepting a CRCPolynomial extern as a compile-time parameter,
 * it takes the parameters for the extern instead.The extern itself is
 * instantiated inside the control.
 *
 * Both styles produce the same result, but this one allows the programmer
 * to have simplified constructors (in this case for the control), because some
 * parameters are assigned default values in the control itself. In this
 * particular case, when the control is instantiated we only need to specify
 * the polynomial coefficients.
 */
control calc_ipv6_hash(
    in    headers   hdr,
    in    ingress_metadata_t  ig_md,
    out   bit<32>                hash)(
        bit<32>  coeff)
{
    CRCPolynomial<bit<32>>(
                            coeff    = coeff,
                            reversed = true,
                            msb      = false,
                            extended = false,
                            init     = 0xFFFFFFFF,
                            xor      = 0xFFFFFFFF) poly;
    Hash<bit<32>>(HashAlgorithm_t.CUSTOM, poly) hash_algo;

    action do_hash() {
        hash = hash_algo.get({
            hdr.ipv6.src_addr,
            hdr.ipv6.dst_addr,
            hdr.ipv6.next_hdr,
            ig_md.l4_lookup.word_1,
            ig_md.l4_lookup.word_2
        });
    }

    apply {
        do_hash();
    }
}

control calc_ipv6_hashes(
    in    headers   hdr,
    in    ingress_metadata_t  ig_md,
    inout selector_hash_t        hash)
{
    calc_ipv6_hash(coeff=0x04C11DB7) hash1;
#if HASH_WIDTH > 32
    calc_ipv6_hash(coeff=0x1EDC6F41) hash2;
#if HASH_WIDTH > 64
    calc_ipv6_hash(coeff=0xA833982B) hash3;
#endif
#endif

    apply {
        hash1.apply(hdr, ig_md, hash[31:0]);
#if HASH_WIDTH > 32
        hash2.apply(hdr, ig_md, hash[63:32]);
#if HASH_WIDTH > 64
        hash3.apply(hdr, ig_md, hash[95:64]);
#endif
#endif
    }
}
