/*
 * Copyright 2019-present Universidade Federal do Espirito Santo (UFES) and
 *                        Instituto Federal do Espirito Santo (IFES)
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

#ifndef _IG_CTL_POLKA_P4_
#define _IG_CTL_POLKA_P4_

#ifdef HAVE_POLKA

control IngressControlPOLKA(
    inout headers hdr,
    inout ingress_metadata_t ig_md,
    in ingress_intrinsic_metadata_t ig_intr_md,
    inout ingress_intrinsic_metadata_for_deparser_t ig_dprsr_md,
    inout ingress_intrinsic_metadata_for_tm_t ig_tm_md)
{

    CRCPolynomial<bit<16>>(
        coeff    = (65539 & 0xffff),
        reversed = false,
        msb      = false,
        extended = false,
        init     = 16w0x0000,
        xor      = 16w0x0000) poly;
    Hash<bit<16>>(HashAlgorithm_t.CUSTOM, poly) hash;

    action act_forward(NextHopId_t nexthop_id) {
        ig_md.nexthop_id = nexthop_id;
        ig_md.ipv4_valid = 0;
        ig_md.ipv6_valid = 0;
#ifdef HAVE_MPLS
        ig_md.mpls0_valid = 0;
        ig_md.mpls1_valid = 0;
#endif
    }

    action act_route() {
        ig_md.polka_remove = 1;
        ig_md.ethertype = hdr.polka.proto;
    }

    table tbl_polka {
        key = {
ig_md.vrf:
            exact;
ig_md.polka_next:
            exact;
        }
        actions = {
            act_forward;
            act_route;
            NoAction;
        }
        size = POLKA_TABLE_SIZE;
        default_action = NoAction();
    }

    apply {
        if (ig_md.polka_valid == 1) {
            bit<112> ndata = (bit<112>) (hdr.polka.routeid >> 16);
            bit<16> diff = (bit<16>) hdr.polka.routeid;
            bit<16> nres = hash.get(ndata);
            ig_md.polka_next = nres ^ diff;
            tbl_polka.apply();
        }
    }
}

#endif

#endif // _IG_CTL_POLKA_P4_
