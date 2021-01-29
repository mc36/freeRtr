/*
 * Copyright 2019-present GT RARE project
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

#ifndef _IG_CTL_OUTPORT_P4_
#define _IG_CTL_OUTPORT_P4_

control IngressControlOutPort(inout headers hdr,
                            inout ingress_metadata_t ig_md,
                            inout standard_metadata_t ig_intr_md) {


    action act_set_port(SubIntId_t port) {
        ig_md.outport_id = port;
    }

    action act_set_identical() {
        ig_md.outport_id = ig_md.target_id;
    }

    action act_set_drop() {
        mark_to_drop(ig_intr_md);
    }



    table tbl_vlan_out {
        key = {
ig_md.target_id:
            exact;
        }
        actions = {
            act_set_identical;
            act_set_port;
        }
        size = VLAN_TABLE_SIZE;
        default_action = act_set_identical();
    }


    table tbl_nexthop {
        key = {
ig_md.nexthop_id:
            exact;
        }
        actions = {
            act_set_port;
            act_set_drop;
        }
        size = NEXTHOP_TABLE_SIZE;
        default_action = act_set_drop();
    }



    apply {

        if (ig_md.srv_op_type != 0) {
            hdr.ipv6.setInvalid();
        }
        if (ig_md.srv_op_type == 2) {
            hdr.eth3.setInvalid();
        }

        if (ig_md.mpls1_remove == 1) {
            hdr.mpls1.setInvalid();
        }

        if (ig_md.mpls0_remove == 1) {
            hdr.mpls0.setInvalid();
            if (ig_md.ipv4_valid == 1) {
                ig_md.ethertype = ETHERTYPE_IPV4;
            } else {
                ig_md.ethertype = ETHERTYPE_IPV6;
            }
        }


        if (ig_md.target_id != 0) {
            tbl_vlan_out.apply();
            return;
        }

        tbl_nexthop.apply();

        if (hdr.ipv4.isValid()) {
            hdr.ipv4.ttl = hdr.ipv4.ttl -1;
        }
        if (hdr.ipv6.isValid()) {
            hdr.ipv6.hop_limit = hdr.ipv6.hop_limit -1;
        }

    }
}

#endif // _IG_CTL_OUTPORT_P4_

