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
    inout ingress_metadata_t ig_md,
    /* Intrinsic */
    inout standard_metadata_t ig_intr_md)
{

    EgressControlMcast() eg_ctl_mcast;
    IngressControlVlanOut() eg_ctl_vlan_out;
    IngressControlBundle() eg_ctl_bundle;

    apply {

        if (ig_md.need_recir == 1) {
            recir_headers_t rec_hdr;
            recirculate<recir_headers_t>(rec_hdr);
            return;
        }

        if (ig_md.need_clone == 0) return;

        eg_ctl_mcast.apply(hdr,ig_md,ig_intr_md);
        eg_ctl_vlan_out.apply(hdr,ig_md,ig_intr_md);
        eg_ctl_bundle.apply(hdr,ig_md,ig_intr_md);

        if (ig_md.need_recir == 1) {
            recir_headers_t rec_hdr;
            recirculate<recir_headers_t>(rec_hdr);
            return;
        }

    }
}

#endif // _EGRESS_CONTROL_P4_
