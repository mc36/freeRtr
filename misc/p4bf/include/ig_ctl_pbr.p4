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

#ifndef _IG_CTL_PBR_P4_
#define _IG_CTL_PBR_P4_

#ifdef HAVE_PBR

control IngressControlPBR(inout headers hdr, inout ingress_metadata_t ig_md,
                          in ingress_intrinsic_metadata_t ig_intr_md,
                          inout ingress_intrinsic_metadata_for_deparser_t ig_dprsr_md,
                          inout ingress_intrinsic_metadata_for_tm_t ig_tm_md)
{


    action act_normal() {
#ifdef HAVE_FRAG
        ig_dprsr_md.drop_ctl = ig_dprsr_md.drop_ctl | ig_md.layer3_frag;
#endif
    }

    action act_setvrf(switch_vrf_t vrf_id) {
        ig_md.vrf = vrf_id;
#ifdef HAVE_FRAG
        ig_dprsr_md.drop_ctl = ig_dprsr_md.drop_ctl | ig_md.layer3_frag;
#endif
    }

    action act_sethop(switch_vrf_t vrf_id, NextHopId_t nexthop_id) {
        ig_md.vrf = vrf_id;
        ig_md.nexthop_id = nexthop_id;
        ig_md.ipv4_valid = 0;
        ig_md.ipv6_valid = 0;
#ifdef HAVE_FRAG
        ig_dprsr_md.drop_ctl = ig_dprsr_md.drop_ctl | ig_md.layer3_frag;
#endif
    }

#ifdef HAVE_MPLS
    action act_setlabel4(switch_vrf_t vrf_id, NextHopId_t nexthop_id, label_t label_val) {
        ig_md.mpls0_remove = 0;
        ig_md.mpls_encap_egress_label = label_val;
        ig_md.mpls_encap_rawip_valid = 1;
        ig_md.mpls_encap_decap_sap_type = 4;
        ig_md.vrf = vrf_id;
        ig_md.nexthop_id = nexthop_id;
        ig_md.ipv4_valid = 0;
        ig_md.ipv6_valid = 0;
#ifdef HAVE_FRAG
        ig_dprsr_md.drop_ctl = ig_dprsr_md.drop_ctl | ig_md.layer3_frag;
#endif
    }
#endif

#ifdef HAVE_MPLS
    action act_setlabel6(switch_vrf_t vrf_id, NextHopId_t nexthop_id, label_t label_val) {
        ig_md.mpls0_remove = 0;
        ig_md.mpls_encap_egress_label = label_val;
        ig_md.mpls_encap_rawip_valid = 1;
        ig_md.mpls_encap_decap_sap_type = 6;
        ig_md.vrf = vrf_id;
        ig_md.nexthop_id = nexthop_id;
        ig_md.ipv4_valid = 0;
        ig_md.ipv6_valid = 0;
#ifdef HAVE_FRAG
        ig_dprsr_md.drop_ctl = ig_dprsr_md.drop_ctl | ig_md.layer3_frag;
#endif
    }
#endif


    table tbl_ipv4_pbr {
        key = {
ig_md.vrf:
            exact;
hdr.ipv4.protocol:
            ternary;
hdr.ipv4.src_addr:
            ternary;
hdr.ipv4.dst_addr:
            ternary;
ig_md.layer4_srcprt:
            ternary;
ig_md.layer4_dstprt:
            ternary;
hdr.ipv4.diffserv:
            ternary;
hdr.ipv4.identification:
            ternary;
#ifdef HAVE_SGT
ig_md.sec_grp_id:
            ternary;
#endif
        }
        actions = {
            act_normal;
            act_setvrf;
            act_sethop;
#ifdef HAVE_MPLS
            act_setlabel4;
#endif
            @defaultonly NoAction;
        }
        size = IPV4_PBRACL_TABLE_SIZE;
        const default_action = NoAction();
    }

    table tbl_ipv6_pbr {
        key = {
ig_md.vrf:
            exact;
hdr.ipv6.next_hdr:
            ternary;
hdr.ipv6.src_addr:
            ternary;
hdr.ipv6.dst_addr:
            ternary;
ig_md.layer4_srcprt:
            ternary;
ig_md.layer4_dstprt:
            ternary;
hdr.ipv6.traffic_class:
            ternary;
hdr.ipv6.flow_label:
            ternary;
#ifdef HAVE_SGT
ig_md.sec_grp_id:
            ternary;
#endif
        }
        actions = {
            act_normal;
            act_setvrf;
            act_sethop;
#ifdef HAVE_MPLS
            act_setlabel6;
#endif
            @defaultonly NoAction;
        }
        size = IPV6_PBRACL_TABLE_SIZE;
        const default_action = NoAction();
    }

    apply {
        if (ig_md.ipv4_valid==1)  {
            tbl_ipv4_pbr.apply();
        } else if (ig_md.ipv6_valid==1)  {
            tbl_ipv6_pbr.apply();
        }
    }
}

#endif

#endif // _IG_CTL_PBR_P4_

