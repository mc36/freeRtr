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

#ifndef _IG_CTL_REWRITES_P4_
#define _IG_CTL_REWRITES_P4_

control IngressControlRewrites(inout headers hdr, inout ingress_metadata_t ig_md,
                               inout ingress_intrinsic_metadata_for_deparser_t ig_dprsr_md,
                               inout ingress_intrinsic_metadata_for_tm_t ig_tm_md)
{

    action act_set_drop() {
        ig_dprsr_md.drop_ctl = 1;
    }

    apply {


        if (hdr.vlan.isValid()) hdr.vlan.setInvalid();
#ifdef HAVE_PPPOE
        if (hdr.pppoeD.isValid()) hdr.pppoeD.setInvalid();
#endif
        hdr.ethernet.ethertype = ig_md.ethertype;


        if (ig_md.target_id == 0) {
#ifdef HAVE_POLKA

            if (ig_md.polka_remove == 1) {
                hdr.polka.setInvalid();
            } else if (hdr.polka.isValid()) {
                if (hdr.polka.ttl < 2) act_set_drop();
                hdr.polka.ttl = hdr.polka.ttl - 1;
            } else {
#endif
#ifdef HAVE_NSH
                if (ig_md.nsh_remove == 1) {
                    hdr.nsh.setInvalid();
                } else if (hdr.nsh.isValid()) {
                    if (hdr.nsh.ttl < 2) act_set_drop();
                    //hdr.nsh.ttl = hdr.nsh.ttl - 1;
                } else {
#endif
#ifdef HAVE_MPLS
                    if (hdr.mpls0.isValid()) {
                        if (hdr.mpls0.ttl < 2) act_set_drop();
                        hdr.mpls0.ttl = hdr.mpls0.ttl - 1;
                    } else {
#endif
                        if (hdr.ipv4.isValid()) {
                            if (hdr.ipv4.ttl < 2) act_set_drop();
                            hdr.ipv4.ttl = hdr.ipv4.ttl - 1;
                        } else if (hdr.ipv6.isValid()) {
                            if (hdr.ipv6.hop_limit < 2) act_set_drop();
                            hdr.ipv6.hop_limit = hdr.ipv6.hop_limit - 1;
                        }
#ifdef HAVE_MPLS
                    }
#endif
#ifdef HAVE_NSH
                }
#endif
#ifdef HAVE_POLKA
            }
#endif
        }
    }

}

#endif // _IG_CTL_REWRITES_P4_

