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

#ifndef _EGRESS_PARSER_P4_
#define _EGRESS_PARSER_P4_

/*------------------ E G R E S S  P A R S E R ------------------------------- */
parser eg_prs_main(packet_in pkt,
                   /* User */
                   out egress_headers_t eg_hdr, out egress_metadata_t eg_md,
                   /* Intrinsic */
                   out egress_intrinsic_metadata_t eg_intr_md)
{
    /* This is a mandatory state, required by Tofino Architecture */
    state start {
        pkt.extract(eg_intr_md);
        transition accept;
    }

}

#endif	// _EGRESS_PARSER_P4_
