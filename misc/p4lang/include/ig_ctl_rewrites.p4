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

#ifndef _IG_CTL_REWRITES_P4_
#define _IG_CTL_REWRITES_P4_

control IngressControlRewrites(inout headers hdr,
                               inout ingress_metadata_t ig_md,
                               inout standard_metadata_t ig_intr_md) {


    apply {
        if (hdr.vlan.isValid()) hdr.vlan.setInvalid();
        if (hdr.pppoeD.isValid()) hdr.pppoeD.setInvalid();
        if (hdr.pppoeB.isValid()) hdr.pppoeB.setInvalid();
        if (hdr.l2tpbr.isValid()) hdr.l2tpbr.setInvalid();

        if (ig_md.srv_op_type != 0) {
            hdr.ipv6.setInvalid();
        }
        if (ig_md.srv_op_type == 2) {
            hdr.eth3.setInvalid();
        }

        if (ig_md.sgt_remove == 1) {
            hdr.sgt.setInvalid();
        }

        if (ig_md.nsh_remove == 1) {
            hdr.nsh.setInvalid();
        }

        if (ig_md.polka_remove == 1) {
            hdr.polka.setInvalid();
        }

        if (ig_md.mpls1_remove == 1) {
            hdr.mpls1.setInvalid();
        }

        if (ig_md.mpls0_remove == 1) {
            hdr.mpls0.setInvalid();
            if (ig_md.ipv4_valid == 1) {
                ig_md.ethertype = ETHERTYPE_IPV4;
            } else {
                ig_md.ethertype = ETHERTYPE_IPV6;
            }
        }

        if (ig_md.target_id != 0) {
            return;
        }

        if (hdr.nsh.isValid()) {
            if (hdr.nsh.ttl < 2) ig_md.dropping = 1;
            hdr.nsh.ttl = hdr.nsh.ttl -1;
        }
        if (hdr.polka.isValid()) {
            if (hdr.polka.ttl < 2) ig_md.dropping = 1;
            hdr.polka.ttl = hdr.polka.ttl -1;
        }
        if (hdr.mpls0.isValid()) {
            if (hdr.mpls0.ttl < 2) ig_md.dropping = 1;
            hdr.mpls0.ttl = hdr.mpls0.ttl -1;
        }
        if (hdr.ipv4.isValid()) {
            if (hdr.ipv4.ttl < 2) ig_md.dropping = 1;
            hdr.ipv4.ttl = hdr.ipv4.ttl -1;
        }
        if (hdr.ipv6.isValid()) {
            if (hdr.ipv6.hop_limit < 2) ig_md.dropping = 1;
            hdr.ipv6.hop_limit = hdr.ipv6.hop_limit -1;
        }

    }
}

#endif // _IG_CTL_REWRITES_P4_

