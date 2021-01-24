/*
 * Copyright 2019-present GÉANT RARE project
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
    inout egress_headers_t hdr, inout egress_metadata_t eg_md,
    in egress_intrinsic_metadata_t eg_intr_md,
    in egress_intrinsic_metadata_from_parser_t eg_prsr_md,
    inout egress_intrinsic_metadata_for_deparser_t eg_dprsr_md,
    inout egress_intrinsic_metadata_for_output_port_t eg_oport_md)
{

    EgressControlMcast() eg_ctl_mcast;
    EggressControlVlanOut() eg_ctl_vlan_out;

    apply {

        if (eg_intr_md.egress_rid_first == 0) {
            eg_dprsr_md.drop_ctl = 1;
        } else {
            hdr.vlan.setInvalid();

#ifdef HAVE_MCAST
            eg_ctl_mcast.apply(hdr, eg_md, eg_intr_md, eg_dprsr_md);
#endif

            eg_ctl_vlan_out.apply(hdr, eg_md, eg_intr_md, eg_dprsr_md);

            hdr.internal.setInvalid();
        }

    }
}

#endif // _EGRESS_CONTROL_P4_
