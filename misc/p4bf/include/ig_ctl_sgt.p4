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

#ifdef HAVE_SGT

control IngressControlSgt(inout headers hdr, inout ingress_metadata_t ig_md,
                          in ingress_intrinsic_metadata_t ig_intr_md,
                          inout ingress_intrinsic_metadata_for_deparser_t ig_dprsr_md,
                          inout ingress_intrinsic_metadata_for_tm_t ig_tm_md)
{


    action act_need_tag() {
    }

    action act_noned_tag() {
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


    action act_set_tag(sec_grp_t group) {
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
        if (tbl_sgt_in.apply().hit) {
            if (ig_md.sgt_valid == 0) ig_dprsr_md.drop_ctl = 1;
            ig_md.sec_grp_id = hdr.sgt.groupid;
            ig_md.ethertype = hdr.sgt.ethertype;
        } else {
            if (ig_md.sgt_valid == 1) ig_dprsr_md.drop_ctl = 1;
        }
        tbl_sgt_set.apply();
    }
}

#endif

#endif // _IG_CTL_SGT_P4_
