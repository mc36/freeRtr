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

#ifndef _IG_CTL_MPLS_P4_
#define _IG_CTL_MPLS_P4_

#ifdef HAVE_MPLS

control IngressControlMPLS(inout headers hdr, inout ingress_metadata_t ig_md,
                           in ingress_intrinsic_metadata_t ig_intr_md,
                           inout ingress_intrinsic_metadata_for_deparser_t ig_dprsr_md,
                           inout ingress_intrinsic_metadata_for_tm_t ig_tm_md)
{


    action act_mpls_cpulabel() {
        ig_md.nexthop_id = CPU_PORT;
        ig_md.mpls_op_type = 0;
#ifdef HAVE_NSH
        ig_md.nsh_remove = 0;
#endif
#ifdef HAVE_POLKA
        ig_md.polka_remove = 0;
#endif
        ig_md.mpls0_remove = 0;
        ig_md.mpls1_remove = 0;
#ifdef HAVE_NSH
        ig_md.nsh_valid = 0;
#endif
#ifdef HAVE_POLKA
        ig_md.polka_valid = 0;
#endif
        ig_md.mpls0_valid = 0;
        ig_md.mpls1_valid = 0;
        ig_md.ipv4_valid = 0;
        ig_md.ipv6_valid = 0;
    }


    action act_mpls_swap0_set_nexthop(label_t egress_label, NextHopId_t nexthop_id) {
        hdr.mpls0.label = egress_label;
        ig_md.nexthop_id = nexthop_id;
        ig_md.mpls_op_type = 0;
        ig_md.ipv4_valid = 0;
        ig_md.ipv6_valid = 0;
        ig_md.mpls_encap_decap_sap_type = 1;
    }

    action act_mpls_swap1_set_nexthop(label_t egress_label, NextHopId_t nexthop_id) {
        hdr.mpls1.label = egress_label;
        ig_md.nexthop_id = nexthop_id;
        ig_md.mpls_op_type = 0;
        ig_md.ipv4_valid = 0;
        ig_md.ipv6_valid = 0;
        ig_md.mpls_encap_decap_sap_type = 1;
        ig_md.mpls1_remove = 0;
    }

    action act_mpls_swap2_set_nexthop(label_t egress_label1, label_t egress_label2, NextHopId_t nexthop_id) {
        hdr.mpls1.setValid();
        hdr.mpls1.setValid();
        hdr.mpls1 = hdr.mpls0;
        hdr.mpls0.bos = 0;
        hdr.mpls0.label = egress_label1;
        hdr.mpls1.label = egress_label2;
        ig_md.nexthop_id = nexthop_id;
        ig_md.mpls_op_type = 0;
        ig_md.ipv4_valid = 0;
        ig_md.ipv6_valid = 0;
        ig_md.mpls_encap_decap_sap_type = 1;
        ig_md.mpls0_remove = 0;
        ig_md.mpls1_remove = 0;
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
        hdr.vlanq.setInvalid();
#ifdef HAVE_PPPOE
        hdr.pppoeD.setInvalid();
#endif
#ifdef HAVE_SGT
        hdr.sgt.setInvalid();
#endif
        ig_md.target_id = port;
        ig_md.mpls0_remove = 0;
        ig_md.mpls1_remove = 0;
        ig_md.mpls_op_type = 2;
        ig_md.ipv4_valid = 0;
        ig_md.ipv6_valid = 0;
    }


    action act_mpls_decap_vpls(switch_vrf_t bridge) {
        ig_md.bridge_id = bridge;
        ig_md.bridge_src = 1;
        ig_md.ethertype = hdr.eth2.ethertype;
        hdr.ethernet.dst_mac_addr = hdr.eth2.dst_mac_addr;
        hdr.ethernet.src_mac_addr = hdr.eth2.src_mac_addr;
        ig_md.mpls_op_type = 2;
        ig_md.ipv4_valid = 0;
        ig_md.ipv6_valid = 0;
    }


    action act_mpls_decap_pwhe(SubIntId_t port) {
        ig_md.ethertype = hdr.eth2.ethertype;
        hdr.ethernet = hdr.eth2;
        hdr.eth2.setInvalid();
        hdr.mpls1.setInvalid();
        hdr.mpls0.setInvalid();
        hdr.vlan.setInvalid();
        hdr.vlanq.setInvalid();
#ifdef HAVE_PPPOE
        hdr.pppoeD.setInvalid();
#endif
#ifdef HAVE_SGT
        hdr.sgt.setInvalid();
#endif
        ig_md.target_id = RECIR_PORT;
        ig_md.mpls0_remove = 0;
        ig_md.mpls1_remove = 0;
        ig_md.mpls_op_type = 2;
        ig_md.ipv4_valid = 0;
        ig_md.ipv6_valid = 0;
        ig_tm_md.ucast_egress_port = RECIR_PORT;
        ig_tm_md.bypass_egress = 1;
//        recirculate(RECIR_PORT);
        hdr.cpu.setValid();
        hdr.cpu._padding1 = 0;
        hdr.cpu._padding2 = 0;
        hdr.cpu.port = port;
    }
#endif


#ifdef HAVE_DUPLAB
    action act_mpls_bcast_label(bit<16> sess) {
        ig_md.clone_session = sess;
        ig_md.mpls0_remove = 0;
        ig_md.mpls1_remove = 0;
        ig_md.mpls_op_type = 0;
        ig_md.ipv4_valid = 0;
        ig_md.ipv6_valid = 0;
        ig_tm_md.mcast_grp_a = sess;
        ig_tm_md.qid = 1;
        ig_tm_md.ucast_egress_port = CPU_PORT;
        ig_tm_md.bypass_egress = 0;
        hdr.vlan.setInvalid();
        hdr.vlanq.setInvalid();
#ifdef HAVE_PPPOE
        hdr.pppoeD.setInvalid();
#endif
#ifdef HAVE_SGT
        hdr.sgt.setInvalid();
#endif
        hdr.ethernet.ethertype = ig_md.ethertype;
        hdr.cpu.setInvalid();
        hdr.internal.setValid();
        hdr.internal.reason = INTREAS_MCAST;
        hdr.internal.clone_session = sess;
        hdr.internal.source_id = ig_md.source_id;
        ig_md.nexthop_id = CPU_PORT;
        ig_md.rpf_iface = ig_md.source_id;
        ig_md.mpls_encap_l2vpn_valid = 1;
        ig_md.mpls_encap_l3vpn_valid = 1;
    }
#endif



#ifdef HAVE_BIER
    action act_mpls_bier_label(bit<16> sess) {
        ig_md.clone_session = sess;
        ig_md.mpls0_remove = 0;
        ig_md.mpls1_remove = 0;
        ig_md.mpls_op_type = 0;
        ig_md.ipv4_valid = 0;
        ig_md.ipv6_valid = 0;
        ig_tm_md.mcast_grp_a = sess;
        ig_tm_md.qid = 1;
        ig_tm_md.ucast_egress_port = CPU_PORT;
        ig_tm_md.bypass_egress = 0;
        hdr.vlan.setInvalid();
        hdr.vlanq.setInvalid();
#ifdef HAVE_PPPOE
        hdr.pppoeD.setInvalid();
#endif
#ifdef HAVE_SGT
        hdr.sgt.setInvalid();
#endif
        hdr.ethernet.ethertype = ig_md.ethertype;
        hdr.cpu.setInvalid();
        hdr.internal.setValid();
        hdr.internal.reason = INTREAS_MCAST;
        hdr.internal.clone_session = sess;
        hdr.internal.source_id = ig_md.source_id;
        ig_md.nexthop_id = CPU_PORT;
        ig_md.rpf_iface = ig_md.source_id;
        ig_md.mpls_encap_l2vpn_valid = 1;
        ig_md.mpls_encap_l3vpn_valid = 1;
    }
#endif



    table tbl_mpls_fib {
        key = {
hdr.mpls0.label:
            exact;
        }
        actions = {
            act_mpls_cpulabel;
            act_mpls_swap0_set_nexthop;
            act_mpls_swap2_set_nexthop;
            act_mpls_decap_set_nexthop;
            act_mpls_decap_ipv4;
#ifdef HAVE_BRIDGE
            act_mpls_decap_pwhe;
            act_mpls_decap_l2vpn;
            act_mpls_decap_vpls;
#endif
#ifdef HAVE_DUPLAB
            act_mpls_bcast_label;
#endif
#ifdef HAVE_BIER
            act_mpls_bier_label;
#endif
            NoAction;
        }
        size = MPLS_TABLE_SIZE;
        default_action = NoAction();
    }

    table tbl_mpls_fib_decap {
        key = {
hdr.mpls1.label:
            exact;
        }
        actions = {
            act_mpls_cpulabel;
            act_mpls_swap1_set_nexthop;
            act_mpls_swap2_set_nexthop;
            act_mpls_decap_set_nexthop;
            act_mpls_decap_l3vpn;
#ifdef HAVE_BRIDGE
            act_mpls_decap_pwhe;
            act_mpls_decap_l2vpn;
            act_mpls_decap_vpls;
#endif
#ifdef HAVE_DUPLAB
            act_mpls_bcast_label;
#endif
#ifdef HAVE_BIER
            act_mpls_bier_label;
#endif
            NoAction;
        }
        size = MPLS_TABLE_SIZE;
        default_action = NoAction();
    }

    apply {
        if (ig_md.mpls0_valid == 1) {
            tbl_mpls_fib.apply();
            if ((ig_md.mpls_op_type == 1) && (ig_md.mpls1_valid == 1)) {
                tbl_mpls_fib_decap.apply();
            }
        }
    }
}

#endif

#endif // _IG_CTL_MPLS_P4_
