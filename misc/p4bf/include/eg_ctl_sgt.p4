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

#ifndef _EG_CTL_SGT_P4_
#define _EG_CTL_SGT_P4_

#ifdef HAVE_SGT

control EgressControlSgt(inout headers hdr, inout ingress_metadata_t eg_md,
                         in egress_intrinsic_metadata_t eg_intr_md,
                         inout egress_intrinsic_metadata_for_deparser_t eg_dprsr_md)
{


    action act_need_tag() {
#ifdef NEED_PKTLEN
        eg_md.pktlen = eg_md.pktlen + 8;
#endif
        hdr.sgt.setValid();
        hdr.sgt.version = 1;
        hdr.sgt.length = 1;
        hdr.sgt.options = 1;
        hdr.sgt.groupid = eg_md.sec_grp_id;
        hdr.sgt.ethertype = eg_md.ethertype;
        eg_md.ethertype = ETHERTYPE_SGT;
    }

    action act_noned_tag() {
    }


    table tbl_sgt_out {
        key = {
eg_md.aclport_id:
            exact;
        }
        actions = {
            act_need_tag;
            act_noned_tag;
        }
        size = VLAN_TABLE_SIZE;
        default_action = act_noned_tag();
    }

    apply {
        tbl_sgt_out.apply();
    }
}

#endif

#endif // _EG_CTL_SGT_P4_
