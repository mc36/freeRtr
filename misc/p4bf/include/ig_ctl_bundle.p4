/*
 * Copyright 2019-present GÉANT RARE project
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

#ifndef _IG_CTL_BUNDLE_P4_
#define _IG_CTL_BUNDLE_P4_

control IngressControlBundle(inout headers hdr, inout ingress_metadata_t ig_md,
                             inout ingress_intrinsic_metadata_for_deparser_t ig_dprsr_md,
                             inout ingress_intrinsic_metadata_for_tm_t ig_tm_md)
{
//    bit<8>             ttl_dec = 0;
    selector_hash_t  hash = 0;

    ActionProfile(size = MAX_PROFILE_MEMBERS) apr_bundle;
    Hash<bit<HASH_WIDTH>>(HashAlgorithm_t.IDENTITY) hash_bundle;

    ActionSelector(
        action_profile = apr_bundle,
        hash           = hash_bundle,
        mode           = SELECTION_MODE,
        max_group_size = MAX_GROUP_SIZE,
        num_groups     = MAX_GROUPS) ase_bundle;


    action send2egress() {
        ig_tm_md.bypass_egress = 0;
        hdr.internal.setValid();
        hdr.internal.target_id = ig_md.target_id;
        hdr.internal.nexthop_id = ig_md.nexthop_id;
        hdr.internal.aclport_id = ig_md.aclport_id;
        hdr.internal.clone_session = 0;
#ifdef NEED_PKTLEN
        hdr.internal.pktlen = ig_md.pktlen;
#endif
#ifdef HAVE_SGT
        hdr.internal.sec_grp_id = ig_md.sec_grp_id;
#endif
    }


    action act_send_to_member(PortId_t port) {
        ig_tm_md.ucast_egress_port = port;
        send2egress();
        hdr.internal.reason = INTREAS_UCAST;
    }


    action act_send_to_recir(SubIntId_t port) {
        ig_tm_md.ucast_egress_port = RECIR_PORT;
//        recirculate(RECIR_PORT);
        send2egress();
        hdr.internal.reason = INTREAS_RECIR;
        hdr.internal.source_id = port;
    }

    action act_send_identical() {
        ig_tm_md.ucast_egress_port = (PortId_t)ig_md.output_id;
        send2egress();
        hdr.internal.reason = INTREAS_UCAST;
    }

#ifdef HAVE_SCRAMBLE
    @selector_enable_scramble(HAVE_SCRAMBLE)
#endif

    table tbl_nexthop_bundle {
        key = {
ig_md.output_id:
            exact;
hash:
            selector;
        }
        actions = {
            act_send_to_member;
            act_send_to_recir;
            act_send_identical;
            NoAction;
        }
        size = BUNDLE_TABLE_SIZE;
        default_action = act_send_identical();
        implementation = ase_bundle;
    }

    apply {
        if (ig_md.ipv4_valid==1) {
            calc_ipv4_hashes.apply(hdr, ig_md, hash);
        } else if (ig_md.ipv6_valid==1) {
            calc_ipv6_hashes.apply(hdr, ig_md, hash);
        } else {
            calc_default_hashes.apply(hdr, ig_md, hash);
        }
        tbl_nexthop_bundle.apply();
    }
}

#endif // _IG_CTL_BUNDLE_P4_
