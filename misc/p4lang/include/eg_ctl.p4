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

#ifndef _EGRESS_CONTROL_P4_
#define _EGRESS_CONTROL_P4_

control eg_ctl(
    /* User */
    inout headers hdr,
    inout ingress_metadata_t eg_md,
    /* Intrinsic */
    inout standard_metadata_t eg_intr_md)
{

    EgressControlMcast() eg_ctl_mcast;
    EgressControlNexthop() eg_ctl_nexthop;
    EgressControlQosOut() eg_ctl_qos_out;
    EgressControlSgt() eg_ctl_sgt;
    EgressControlVlanOut() eg_ctl_vlan_out;
    EgressControlHairpin() eg_ctl_hairpin;

    apply {

        if (eg_md.punting != 0) {
            return;
        }

        if (eg_md.need_recir == 1) {
            recirculate_preserving_field_list(1);
            return;
        }

        if (eg_md.need_clone != 0) {
            eg_ctl_mcast.apply(hdr,eg_md,eg_intr_md);
            if (eg_md.dropping == 1) {
                mark_to_drop(eg_intr_md);
                return;
            }
            if (eg_md.need_recir == 1) {
                recirculate_preserving_field_list(1);
                return;
            }
        }

        eg_ctl_sgt.apply(hdr,eg_md,eg_intr_md);

        eg_ctl_nexthop.apply(hdr,eg_md,eg_intr_md);

        eg_ctl_qos_out.apply(hdr,eg_md,eg_intr_md);
        if (eg_md.dropping == 1) {
            mark_to_drop(eg_intr_md);
            return;
        }
        eg_ctl_vlan_out.apply(hdr,eg_md,eg_intr_md);
        eg_ctl_hairpin.apply(hdr,eg_md,eg_intr_md);

        if (eg_md.need_recir != 0) {
            recirculate_preserving_field_list(1);
            return;
        }

    }
}

#endif // _EGRESS_CONTROL_P4_
