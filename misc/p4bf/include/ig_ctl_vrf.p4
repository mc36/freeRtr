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

#ifndef _IG_CTL_VRF_P4_
#define _IG_CTL_VRF_P4_

control IngressControlVRF(inout headers hdr, inout ingress_metadata_t ig_md)
{

    action act_set_vrf(switch_vrf_t vrf) {
        ig_md.vrf = vrf;
    }

    action act_set_default_vrf() {
        ig_md.vrf = 0;
    }

#ifdef HAVE_MPLS
    action act_set_mpls_xconn_encap(NextHopId_t target, label_t tunlab,
                                    label_t svclab) {
        ig_md.vrf = 0;
#ifdef HAVE_POLKA
        ig_md.polka_valid = 0;
#endif
#ifdef HAVE_NSH
        ig_md.nsh_valid = 0;
#endif
        ig_md.mpls0_valid = 0;
        ig_md.mpls1_valid = 0;
        ig_md.mpls0_remove = 0;
        ig_md.mpls1_remove = 0;
        ig_md.arp_valid = 0;
        ig_md.ipv4_valid = 0;
        ig_md.ipv6_valid = 0;
        ig_md.mpls_encap_egress_label = tunlab;
        ig_md.mpls_encap_svc_label = svclab;
        ig_md.mpls_encap_decap_sap_type = 2;
        ig_md.mpls_encap_xconnect_valid = 1;
        ig_md.nexthop_id = target;
    }
#endif

#ifdef HAVE_BRIDGE
    action act_set_bridge(switch_vrf_t bridge) {
        ig_md.bridge_id = bridge;
    }
#endif

    table tbl_vrf {
        key = {
ig_md.source_id:
            exact;
        }
        actions = {
            act_set_vrf;
#ifdef HAVE_MPLS
            act_set_mpls_xconn_encap;
#endif
#ifdef HAVE_BRIDGE
            act_set_bridge;
#endif
            act_set_default_vrf;
        }
        size = PORT_TABLE_SIZE;
        default_action = act_set_default_vrf();
    }

    apply {
        tbl_vrf.apply();
    }

}
#endif // _IG_CTL_VRF_P4_
