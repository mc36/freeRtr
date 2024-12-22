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

control IngressControlMPLS(inout headers hdr,
                           inout ingress_metadata_t ig_md,
                           inout standard_metadata_t ig_intr_md) {

    direct_counter(CounterType.packets_and_bytes) stats;

    action act_mpls_cpulabel() {
        ig_md.nexthop_id = CPU_PORT;
        ig_md.mpls_op_type = 0;
        ig_md.nsh_remove = 0;
        ig_md.polka_remove = 0;
        ig_md.mpls0_remove = 0;
        ig_md.mpls1_remove = 0;
        ig_md.nsh_valid = 0;
        ig_md.polka_valid = 0;
        ig_md.mpls0_valid = 0;
        ig_md.mpls1_valid = 0;
        ig_md.ipv4_valid = 0;
        ig_md.ipv6_valid = 0;
    }


    action act_mpls_bcast_label(bit<16> sess) {
        ig_md.clone_session = sess;
        ig_md.need_clone = 1;
        ig_md.mpls0_remove = 0;
        ig_md.mpls1_remove = 0;
        ig_md.mpls_op_type = 0;
        ig_md.ipv4_valid = 0;
        ig_md.ipv6_valid = 0;
        ig_intr_md.mcast_grp = sess;
    }


    action act_mpls_bier_label(bit<16> sess) {
        ig_md.clone_session = sess;
        ig_md.need_clone = 1;
        ig_md.mpls0_remove = 0;
        ig_md.mpls1_remove = 0;
        ig_md.mpls_op_type = 0;
        ig_md.ipv4_valid = 0;
        ig_md.ipv6_valid = 0;
        ig_intr_md.mcast_grp = sess;
    }


    action act_mpls_swap0_set_nexthop(label_t egress_label, NextHopId_t nexthop_id) {
        /*
         * Encapsulate MPLS header
         */
        hdr.mpls0.label = egress_label;
        /*
         * Indicate nexthop_id
         */
        ig_md.nexthop_id = nexthop_id;
        ig_md.mpls_op_type = 0;
        ig_md.ipv4_valid = 0;
        ig_md.ipv6_valid = 0;
    }


    action act_mpls_swap1_set_nexthop(label_t egress_label, NextHopId_t nexthop_id) {
        /*
         * Encapsulate MPLS header
         */
        hdr.mpls1.label = egress_label;
        /*
         * Indicate nexthop_id
         */
        ig_md.nexthop_id = nexthop_id;
        hdr.mpls0.setInvalid();
        ig_md.mpls0_remove = 0;
        ig_md.mpls1_remove = 0;
        ig_md.mpls_op_type = 0;
        ig_md.ipv4_valid = 0;
        ig_md.ipv6_valid = 0;
    }

    action act_mpls_swap2_set_nexthop(label_t egress_label1, label_t egress_label2, NextHopId_t nexthop_id) {
        /*
         * Encapsulate MPLS header
         */
        hdr.mpls1.setValid();
        hdr.mpls1 = hdr.mpls0;
        hdr.mpls0.bos = 0;
        hdr.mpls0.label = egress_label1;
        hdr.mpls1.label = egress_label2;
        /*
         * Indicate nexthop_id
         */
        ig_md.nexthop_id = nexthop_id;
        ig_md.mpls0_remove = 0;
        ig_md.mpls1_remove = 0;
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
        /*
         * Egress packet is back now an IPv4 packet
         * (LABEL PHP )
         */
        /*
         * Decapsulate MPLS header
         */
//      hdr.mpls0.setInvalid();
//      hdr.ipv4.setValid();
        /*
         * Indicate effective VRF during
         * MPLS tunnel decap
         */
        ig_md.vrf = vrf;
        ig_md.mpls_op_type = 1;
        ig_md.mpls0_remove = 1;
        ig_md.mpls1_remove = 1;

    }

    action act_mpls_decap_l3vpn(switch_vrf_t vrf) {
        /*
         * Egress packet is back now an IPv4 packet
         * (LABEL PHP )
         */
        /*
         * Decapsulate MPLS header
         */
//      hdr.mpls1.setInvalid();
//      hdr.ipv4.setValid();
        /*
         * Indicate effective VRF during
         * MPLS tunnel decap
         */
        ig_md.vrf = vrf;
        ig_md.mpls_op_type = 1;
        ig_md.mpls0_remove = 1;
        ig_md.mpls1_remove = 1;

    }

    action act_mpls_decap_l2vpn(SubIntId_t port) {
        /*
         * Egress packet is back now an IPv4 packet
         * (LABEL PHP )
         */
        /*
         * Decapsulate MPLS header
         */
        ig_md.ethertype = hdr.eth2.ethertype;
        hdr.ethernet.dst_mac_addr = hdr.eth2.dst_mac_addr;
        hdr.ethernet.src_mac_addr = hdr.eth2.src_mac_addr;
        hdr.eth2.setInvalid();
        hdr.mpls1.setInvalid();
        hdr.mpls0.setInvalid();
        hdr.vlan.setInvalid();
        hdr.vlanq.setInvalid();
        ig_md.target_id = port;
        /*
         * Indicate effective VRF during
         * MPLS tunnel decap
         */
        ig_md.mpls0_remove = 0;
        ig_md.mpls1_remove = 0;
        ig_md.mpls_op_type = 2;
        ig_md.ipv4_valid = 0;
        ig_md.ipv6_valid = 0;
    }

    action act_mpls_decap_vpls(SubIntId_t bridge) {
        /*
         * Egress packet is back now an IPv4 packet
         * (LABEL PHP )
         */
        ig_md.bridge_id = bridge;
        ig_md.bridge_src = bridge;
        ig_md.ethertype = hdr.eth2.ethertype;
        hdr.ethernet.dst_mac_addr = hdr.eth2.dst_mac_addr;
        hdr.ethernet.src_mac_addr = hdr.eth2.src_mac_addr;
        /*
         * Indicate effective VRF during
         * MPLS tunnel decap
         */
        ig_md.mpls_op_type = 2;
        ig_md.ipv4_valid = 0;
        ig_md.ipv6_valid = 0;
    }


    action act_mpls_decap_pwhe(SubIntId_t port) {
        /*
         * Egress packet is back now an IPv4 packet
         * (LABEL PHP )
         */
        ig_md.ethertype = hdr.eth2.ethertype;
        hdr.ethernet = hdr.eth2;
        hdr.eth2.setInvalid();
        hdr.mpls1.setInvalid();
        hdr.mpls0.setInvalid();
        ig_md.target_id = port;
        /*
         * Indicate effective VRF during
         * MPLS tunnel decap
         */
        ig_md.mpls_op_type = 2;
        ig_md.ipv4_valid = 0;
        ig_md.ipv6_valid = 0;
        ig_intr_md.egress_spec = (PortId_t)port;
        ig_md.need_recir = 1;
        ig_md.source_id = port;
        ig_md.target_id = port;
        hdr.cpu.setValid();
        hdr.cpu.port = port;
    }




    table tbl_mpls_fib {
        key = {
hdr.mpls0.label:
            exact;
        }
        actions = {

            /*
             * cpu label
             */
            act_mpls_cpulabel;

            /*
             * mpls core swap
             */
            act_mpls_swap0_set_nexthop;

            /*
             * mpls core push
             */
            act_mpls_swap2_set_nexthop;

            /*
             * mpls decapsulation if PHP
             */
            act_mpls_decap_set_nexthop;

            /*
             * mpls decapsulation if PHP
             */
            act_mpls_decap_ipv4;

            /*
             * mpls decapsulation if PHP
             */
            act_mpls_decap_l2vpn;

            /*
             * mpls decapsulation if PHP
             */
            act_mpls_decap_vpls;

            /*
             * mpls decapsulation if PHP
             */
            act_mpls_decap_pwhe;

            /*
             * mpls broadcast label
             */
            act_mpls_bcast_label;

            /*
             * mpls BIER label
             */
            act_mpls_bier_label;

            /*
             * Default action;
             */
            @defaultonly NoAction;
        }
        size = MPLS_TABLE_SIZE;
        default_action = NoAction();
        counters = stats;
    }


    table tbl_mpls_fib_decap {
        key = {
hdr.mpls1.label:
            exact;
        }
        actions = {

            /*
             * cpu label
             */
            act_mpls_cpulabel;

            /*
             * mpls core swap
             */
            act_mpls_swap1_set_nexthop;

            /*
             * mpls core push
             */
            act_mpls_swap2_set_nexthop;

            /*
             * mpls decapsulation if PHP
             */
            act_mpls_decap_set_nexthop;

            /*
             * mpls decapsulation if PHP
             */
            act_mpls_decap_l3vpn;

            /*
             * mpls decapsulation if PHP
             */
            act_mpls_decap_l2vpn;

            /*
             * mpls decapsulation if PHP
             */
            act_mpls_decap_vpls;

            /*
             * mpls decapsulation if PHP
             */
            act_mpls_decap_pwhe;

            /*
             * mpls broadcast label
             */
            act_mpls_bcast_label;

            /*
             * mpls BIER label
             */
            act_mpls_bier_label;

            /*
             * Default action;
             */
            @defaultonly NoAction;
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

#endif // _IG_CTL_MPLS_P4_

