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

#ifndef _IG_CTL_SGT_P4_
#define _IG_CTL_SGT_P4_

control IngressControlSgt(inout headers hdr,
                          inout ingress_metadata_t ig_md,
                          in standard_metadata_t  ig_intr_md) {

    counter((MAX_PORT+1), CounterType.packets_and_bytes) stats;


    action act_need_tag() {
        ig_md.dropping = 0;
        if (ig_md.sgt_valid == 0) ig_md.dropping = 1;
        ig_md.vlan_size = ig_md.vlan_size + 8;
        ig_md.sgt_remove = 1;
        ig_md.sec_grp_id = hdr.sgt.groupid;
        ig_md.ethertype = hdr.sgt.ethertype;
    }

    action act_noned_tag() {
        ig_md.dropping = 0;
        if (ig_md.sgt_valid == 1) ig_md.dropping = 1;
    }


    table tbl_sgt_in {
        key = {
ig_md.source_id:
            exact;
        }
        actions = {
            act_need_tag;
            act_noned_tag;
        }
        size = VLAN_TABLE_SIZE;
        default_action = act_noned_tag();
    }


    action act_set_tag(bit<16> group) {
        ig_md.sec_grp_id = group;
    }

    action act_leave_tag() {
    }


    table tbl_sgt_set {
        key = {
ig_md.source_id:
            exact;
        }
        actions = {
            act_set_tag;
            act_leave_tag;
        }
        size = VLAN_TABLE_SIZE;
        default_action = act_leave_tag();
    }

    apply {
        tbl_sgt_in.apply();
        tbl_sgt_set.apply();
        stats.count((bit<32>)ig_md.source_id);
    }
}

#endif // _IG_CTL_SGT_P4_
