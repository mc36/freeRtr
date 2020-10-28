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

#ifndef _IG_CTL_VLAN_OUT_P4_
#define _IG_CTL_VLAN_OUT_P4_

control IngressControlVlanOut(inout headers hdr,
                              inout ingress_metadata_t ig_md,
                              inout standard_metadata_t ig_intr_md) {

    action act_set_vlan_port(SubIntId_t port, vlan_id_t vlan) {
        ig_md.outport_id = port;
        hdr.vlan.setValid();
        hdr.vlan.ethertype = ig_md.ethertype;
        hdr.ethernet.ethertype = ETHERTYPE_VLAN;
        hdr.vlan.vid = vlan;
    }

    action act_set_port() {
        ig_md.outport_id = ig_md.target_id;
        hdr.ethernet.ethertype = ig_md.ethertype;
    }

    table tbl_vlan_out {
        key = {
ig_md.target_id:
            exact;
        }
        actions = {
            act_set_port;
            act_set_vlan_port;
        }
        size = VLAN_TABLE_SIZE;
        default_action = act_set_port();
    }

    apply {
        tbl_vlan_out.apply();
    }
}

#endif // _IG_CTL_VLAN_OUT_P4_

