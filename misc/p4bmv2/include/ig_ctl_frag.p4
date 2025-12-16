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

#ifndef _IG_CTL_FRAG_P4_
#define _IG_CTL_FRAG_P4_

control IngressControlFrag(inout headers hdr,
                           inout ingress_metadata_t ig_md,
                           inout standard_metadata_t ig_intr_md) {


    apply {
        ig_md.layer3_frag = 0;
        if (hdr.ipv4.isValid()) {
            if ((hdr.ipv4.flags&1) != 0) ig_md.layer3_frag = 1;
            if (hdr.ipv4.frag_offset != 0) ig_md.layer3_frag = 1;
        }
        if (hdr.ipv6.isValid()) {
            if (hdr.ipv6.next_hdr == IP_PROTOCOL_IPV6_FRAG) ig_md.layer3_frag = 1;
        }
        if (hdr.ipv4b.isValid()) {
            if ((hdr.ipv4b.flags&1) != 0) ig_md.layer3_frag = 1;
            if (hdr.ipv4b.frag_offset != 0) ig_md.layer3_frag = 1;
        }
        if (hdr.ipv6b.isValid()) {
            if (hdr.ipv6b.next_hdr == IP_PROTOCOL_IPV6_FRAG) ig_md.layer3_frag = 1;
        }
    }

}

#endif // _IG_CTL_FRAG_P4_
