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

#ifndef _EGRESS_PARSER_P4_
#define _EGRESS_PARSER_P4_

parser eg_prs_main(packet_in pkt,
                   /* User */
                   out headers hdr, out ingress_metadata_t ig_md,
                   /* Intrinsic */
                   out egress_intrinsic_metadata_t eg_intr_md)
{

#include "include/ig_prs_def.p4"

    state start {
        pkt.extract(eg_intr_md);

#include "include/ig_prs_clr.p4"

        pkt.extract(hdr.internal);
        ig_md.target_id = hdr.internal.target_id;
        ig_md.nexthop_id = hdr.internal.nexthop_id;
        ig_md.aclport_id = hdr.internal.aclport_id;
        ig_md.ingress_id = 0;
        ig_md.port_md.portid = 0;
        transition prs_ethernet;
    }


#include "include/ig_prs_hdr.p4"


}

#endif	// _EGRESS_PARSER_P4_
