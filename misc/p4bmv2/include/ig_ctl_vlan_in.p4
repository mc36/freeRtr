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

#ifndef _IG_CTL_VLAN_IN_P4_
#define _IG_CTL_VLAN_IN_P4_

control IngressControlVlanIn(inout headers hdr,
                             inout ingress_metadata_t ig_md,
                             in standard_metadata_t  ig_intr_md) {

    counter((MAX_PORT+1), CounterType.packets_and_bytes) stats;

    action act_set_vlan_iface(SubIntId_t src) {
        ig_md.source_id = src;
        ig_md.ethertype = hdr.vlan.ethertype;
        ig_md.vlan_size = ig_md.vlan_size + 4;
    }

    action act_set_qinq_iface(SubIntId_t src) {
        ig_md.source_id = src;
        ig_md.ethertype = hdr.vlanq.ethertype;
        ig_md.vlan_size = ig_md.vlan_size + 8;
    }

    action act_set_def_iface() {
        ig_md.source_id = ig_md.ingress_id;
        ig_md.ethertype = hdr.ethernet.ethertype;
    }

    table tbl_vlan_in {
        key = {
ig_md.ingress_id:
            exact;
hdr.vlan.vid:
            exact;
hdr.vlanq.vid:
            exact;
        }
        actions = {
            act_set_vlan_iface;
            act_set_qinq_iface;
            act_set_def_iface;
        }
        size = VLAN_TABLE_SIZE;
        default_action = act_set_def_iface();
    }

    apply {
        tbl_vlan_in.apply();
        stats.count((bit<32>)ig_md.source_id);
    }
}

#endif // _IG_CTL_VLAN_IN_P4_

