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
#ifdef HAVE_BRIDGE
        ig_md.bridge_id = 0;
#endif
        ig_md.mpls_encap_egress_label = tunlab;
        ig_md.mpls_encap_svc_label = svclab;
        ig_md.mpls_encap_decap_sap_type = 2;
        ig_md.mpls_encap_l2vpn_valid = 1;
        ig_md.nexthop_id = target;
    }
#endif

#ifdef HAVE_NSH
    action act_set_nshconn(bit<24> sp, bit<8> si) {
        ig_md.vrf = 0;
#ifdef HAVE_POLKA
        ig_md.polka_valid = 0;
#endif
#ifdef HAVE_MPLS
        ig_md.mpls0_valid = 0;
        ig_md.mpls1_valid = 0;
#endif
        ig_md.arp_valid = 0;
        ig_md.ipv4_valid = 0;
        ig_md.ipv6_valid = 0;
        ig_md.nsh_valid = 1;
        hdr.eth2.setValid();
        hdr.eth2.dst_mac_addr = hdr.ethernet.dst_mac_addr;
        hdr.eth2.src_mac_addr = hdr.ethernet.src_mac_addr;
        hdr.eth2.ethertype = ig_md.ethertype;
        ig_md.ethertype = ETHERTYPE_NSH;
        hdr.nsh.setValid();
        hdr.nsh.version = 0;
        hdr.nsh.oam = 0;
        hdr.nsh.res1 = 0;
        hdr.nsh.ttl = 63;
        hdr.nsh.length = 2;
        hdr.nsh.res2 = 0;
        hdr.nsh.md_type = 2;
        hdr.nsh.next_proto = 3;
        hdr.nsh.sp = sp;
        hdr.nsh.si = si;
    }
#endif

#ifdef HAVE_LOCONN
    action act_set_loconn_ifc (SubIntId_t port) {
        ig_md.vrf = 0;
#ifdef HAVE_POLKA
        ig_md.polka_valid = 0;
#endif
#ifdef HAVE_NSH
        ig_md.nsh_valid = 0;
#endif
#ifdef HAVE_MPLS
        ig_md.mpls0_valid = 0;
        ig_md.mpls1_valid = 0;
#endif
        ig_md.mpls0_remove = 0;
        ig_md.mpls1_remove = 0;
        ig_md.arp_valid = 0;
        ig_md.ipv4_valid = 0;
        ig_md.ipv6_valid = 0;
#ifdef HAVE_BRIDGE
        ig_md.bridge_id = 0;
#endif
        ig_md.target_id = port;
        ig_md.nexthop_id = 0;
    }

    action act_set_loconn_nei (NextHopId_t nhop) {
        ig_md.vrf = 0;
#ifdef HAVE_POLKA
        ig_md.polka_valid = 0;
#endif
#ifdef HAVE_NSH
        ig_md.nsh_valid = 0;
#endif
#ifdef HAVE_MPLS
        ig_md.mpls0_valid = 0;
        ig_md.mpls1_valid = 0;
#endif
        ig_md.mpls0_remove = 0;
        ig_md.mpls1_remove = 0;
        ig_md.arp_valid = 0;
        ig_md.ipv4_valid = 0;
        ig_md.ipv6_valid = 0;
#ifdef HAVE_BRIDGE
        ig_md.bridge_id = 0;
#endif
        ig_md.target_id = 0;
        ig_md.nexthop_id = nhop;
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
#ifdef HAVE_NSH
            act_set_nshconn;
#endif
#ifdef HAVE_LOCONN
            act_set_loconn_ifc;
            act_set_loconn_nei;
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
