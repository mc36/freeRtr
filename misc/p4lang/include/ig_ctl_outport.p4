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


    action act_set_port_vlan(SubIntId_t port) {
        ig_md.outport_id = port;
        ig_md.aclport_id = ig_md.target_id;
    }

    action act_set_port_novlan() {
        ig_md.outport_id = ig_md.target_id;
        ig_md.aclport_id = ig_md.target_id;
    }

    action act_set_port_nexthop(SubIntId_t port, SubIntId_t subif) {
        ig_md.outport_id = port;
        ig_md.aclport_id = subif;
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
            act_set_port_novlan;
            act_set_port_vlan;
        }
        size = VLAN_TABLE_SIZE;
        default_action = act_set_port_novlan();
    }


    table tbl_nexthop {
        key = {
ig_md.nexthop_id:
            exact;
        }
        actions = {
            act_set_port_nexthop;
            act_set_drop;
        }
        size = NEXTHOP_TABLE_SIZE;
        default_action = act_set_drop();
    }



    apply {

        if (ig_md.target_id != 0) {
            tbl_vlan_out.apply();
            return;
        }

        tbl_nexthop.apply();

    }
}

#endif // _IG_CTL_OUTPORT_P4_

