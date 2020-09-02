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

#ifndef _EGRESS_DEPARSER_P4_
#define _EGRESS_DEPARSER_P4_

/*------------------ E G R E S S  D E P A R S E R --------------------------- */

control eg_ctl_dprs(packet_out pkt,
                    /* User */
                    inout egress_headers_t eg_hdr, in egress_metadata_t eg_md,
                    /* Intrinsic */
                    in egress_intrinsic_metadata_for_deparser_t eg_dprsr_md)
{
    apply {
        pkt.emit(eg_hdr);
    }
}

#endif // _EGRESS_DEPARSER_P4_
