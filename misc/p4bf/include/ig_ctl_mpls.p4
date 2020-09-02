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

#ifndef _IG_CTL_MPLS_P4_
#define _IG_CTL_MPLS_P4_

#ifdef HAVE_MPLS

control IngressControlMPLS(inout headers hdr, inout ingress_metadata_t ig_md,
                           in ingress_intrinsic_metadata_t ig_intr_md)
{

    action act_mpls_swap0_set_nexthop(label_t egress_label, NextHopId_t nexthop_id) {
        hdr.mpls0.label = egress_label;
        ig_md.nexthop_id = nexthop_id;
        ig_md.mpls_op_type = 0;
        ig_md.ipv4_valid = 0;
        ig_md.ipv6_valid = 0;
    }

    action act_mpls_swap1_set_nexthop(label_t egress_label, NextHopId_t nexthop_id) {
        hdr.mpls1.label = egress_label;
        ig_md.nexthop_id = nexthop_id;
        ig_md.mpls_op_type = 0;
        ig_md.ipv4_valid = 0;
        ig_md.ipv6_valid = 0;
    }

    action act_mpls_decap_set_nexthop(NextHopId_t nexthop_id) {
        /*
         * Indicate nexthop_id
         */
        ig_md.nexthop_id = nexthop_id;
        ig_md.mpls_op_type = 0;
        ig_md.vrf = 0;
        ig_md.mpls0_remove = 1;
        ig_md.mpls1_remove = 1;
    }

    action act_mpls_decap_ipv4(switch_vrf_t vrf) {
        ig_md.vrf = vrf;
        ig_md.mpls_op_type = 1;
        ig_md.mpls0_remove = 1;
        ig_md.mpls1_remove = 1;

    }

    action act_mpls_decap_l3vpn(switch_vrf_t vrf) {
        ig_md.vrf = vrf;
        ig_md.mpls_op_type = 1;
        ig_md.mpls0_remove = 1;
        ig_md.mpls1_remove = 1;

    }

#ifdef HAVE_BRIDGE
    action act_mpls_decap_l2vpn(SubIntId_t port) {
        ig_md.ethertype = hdr.eth2.ethertype;
        hdr.ethernet.dst_mac_addr = hdr.eth2.dst_mac_addr;
        hdr.ethernet.src_mac_addr = hdr.eth2.src_mac_addr;
        hdr.eth2.setInvalid();
        hdr.mpls1.setInvalid();
        hdr.mpls0.setInvalid();
        hdr.vlan.setInvalid();
        ig_md.target_id = port;
        ig_md.mpls0_remove = 0;
        ig_md.mpls1_remove = 0;
        ig_md.mpls_op_type = 2;
        ig_md.ipv4_valid = 0;
        ig_md.ipv6_valid = 0;
    }


    action act_mpls_decap_vpls(SubIntId_t bridge) {
        ig_md.bridge_id = bridge;
        ig_md.bridge_src = bridge;
        ig_md.ethertype = hdr.eth2.ethertype;
        hdr.ethernet.dst_mac_addr = hdr.eth2.dst_mac_addr;
        hdr.ethernet.src_mac_addr = hdr.eth2.src_mac_addr;
        ig_md.mpls_op_type = 2;
        ig_md.ipv4_valid = 0;
        ig_md.ipv6_valid = 0;
    }
#endif

    table tbl_mpls_fib {
        key = {
ig_md.mpls_label:
            exact;
        }
        actions = {
            act_mpls_swap0_set_nexthop;
            act_mpls_decap_set_nexthop;
            act_mpls_decap_ipv4;
#ifdef HAVE_BRIDGE
            act_mpls_decap_l2vpn;
            act_mpls_decap_vpls;
#endif
            NoAction;
        }
        size = MPLS_TABLE_SIZE;
        default_action = NoAction();
    }

    table tbl_mpls_fib_decap {
        key = {
ig_md.mpls_label:
            exact;
        }
        actions = {
            act_mpls_swap1_set_nexthop;
            act_mpls_decap_set_nexthop;
            act_mpls_decap_l3vpn;
#ifdef HAVE_BRIDGE
            act_mpls_decap_l2vpn;
            act_mpls_decap_vpls;
#endif
            NoAction;
        }
        size = MPLS_TABLE_SIZE;
        default_action = NoAction();
    }

    apply {
        if (ig_md.mpls0_valid == 1) {
            ig_md.mpls_label = hdr.mpls0.label;
            tbl_mpls_fib.apply();
            if ((ig_md.mpls_op_type == 1) && (ig_md.mpls1_valid == 1)) {
                ig_md.mpls_label = hdr.mpls1.label;
                tbl_mpls_fib_decap.apply();
            }
        }
    }
}

#endif

#endif // _IG_CTL_MPLS_P4_
