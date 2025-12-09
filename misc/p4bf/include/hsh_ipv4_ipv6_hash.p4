/*
 * Copyright 2019-present GEANT RARE project
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed On an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

control calc_default_hashes(
    in    headers   hdr,
    in    ingress_metadata_t  ig_md,
    inout selector_hash_t        hash)
{
    apply {}
}

control calc_ipv4_hash(
    in    headers   hdr,
    in    ingress_metadata_t  ig_md,
    out   bit<32> hash)(bit<32> coeff)
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
            hdr.ipv4.src_addr,
            hdr.ipv4.dst_addr,
            hdr.ipv4.protocol,
            ig_md.layer4_srcprt,
            ig_md.layer4_dstprt
        });
    }

    apply {
        do_hash();
    }
}

control calc_ipv4_hashes(
    in    headers hdr,
    in    ingress_metadata_t ig_md,
    inout selector_hash_t hash)
{
    calc_ipv4_hash(coeff=0x04C11DB7) hash1;
#if HASH_WIDTH > 32
    calc_ipv4_hash(coeff=0x1EDC6F41) hash2;
#if HASH_WIDTH > 64
    calc_ipv4_hash(coeff=0xA833982B) hash3;
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

control calc_ipv6_hash(
    in    headers   hdr,
    in    ingress_metadata_t  ig_md,
    out   bit<32> hash)(bit<32> coeff)
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
            ig_md.layer4_srcprt,
            ig_md.layer4_dstprt
        });
    }

    apply {
        do_hash();
    }
}

control calc_ipv6_hashes(
    in    headers hdr,
    in    ingress_metadata_t ig_md,
    inout selector_hash_t hash)
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
