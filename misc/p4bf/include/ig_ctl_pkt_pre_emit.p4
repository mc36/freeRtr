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

#ifndef _IG_CTL_PKT_PRE_EMIT_P4_
#define _IG_CTL_PKT_PRE_EMIT_P4_

control IngressControlPktPreEmit(inout headers hdr,
                                 inout ingress_metadata_t ig_md,
                                 in ingress_intrinsic_metadata_t ig_intr_md,
                                 inout ingress_intrinsic_metadata_for_tm_t
                                 ig_tm_md)
{

    action act_pkt_send_to_cpu() {
        hdr.cpu.setValid();
        hdr.cpu._padding = 0;
        hdr.cpu.port = ig_md.ingress_id;
        ig_tm_md.ucast_egress_port = CPU_PORT;
        ig_tm_md.bypass_egress = 1;
    }

#ifdef HAVE_MPLS
    action act_pkt_mpls_swap_0() {
        ig_md.ethertype = ETHERTYPE_MPLS_UCAST;
    }
#endif

#ifdef HAVE_MPLS
    action act_pkt_mpls_swap_1() {
        hdr.mpls0.setInvalid();
        ig_md.ethertype = ETHERTYPE_MPLS_UCAST;
    }
#endif

#ifdef HAVE_MPLS
    action act_pkt_mpls_ipv4_decap_0_1() {
        hdr.mpls0.setInvalid();
        hdr.mpls1.setInvalid();
        ig_md.ethertype = ETHERTYPE_IPV4;
    }
#endif

#ifdef HAVE_MPLS
    action act_pkt_mpls_ipv4_decap_0() {
        hdr.mpls0.setInvalid();
        ig_md.ethertype = ETHERTYPE_IPV4;
    }
#endif

#ifdef HAVE_MPLS
    action act_pkt_mpls_ipv4_decap_1() {
        hdr.mpls1.setInvalid();
        ig_md.ethertype = ETHERTYPE_IPV4;
    }
#endif

#ifdef HAVE_MPLS
    action act_pkt_mpls_ipv6_decap_0_1() {
        hdr.mpls0.setInvalid();
        hdr.mpls1.setInvalid();
        ig_md.ethertype = ETHERTYPE_IPV6;
    }
#endif

#ifdef HAVE_MPLS
    action act_pkt_mpls_ipv6_decap_0() {
        hdr.mpls0.setInvalid();
        ig_md.ethertype = ETHERTYPE_IPV6;
    }
#endif

#ifdef HAVE_MPLS
    action act_pkt_mpls_ipv6_decap_1() {
        hdr.mpls1.setInvalid();
        ig_md.ethertype = ETHERTYPE_IPV6;
    }
#endif

#ifdef HAVE_MPLS
    action _act_pkt_mpls2_encap() {
        ig_md.ethertype = ETHERTYPE_MPLS_UCAST;
        hdr.mpls0.setValid();
        hdr.mpls0.label = ig_md.mpls_encap_egress_label;
        hdr.mpls0.exp = 0;
        hdr.mpls0.bos = 0;
        hdr.mpls0.ttl = 255;
        hdr.mpls1.setValid();
        hdr.mpls1.label = ig_md.mpls_encap_svc_label;
        hdr.mpls1.exp = 0;
        hdr.mpls1.bos = 1;
        hdr.mpls1.ttl = 255;
    }
#endif

#ifdef HAVE_MPLS
    action _act_pkt_mpls1_encap() {
        ig_md.ethertype = ETHERTYPE_MPLS_UCAST;
        hdr.mpls0.setValid();
        hdr.mpls0.label = ig_md.mpls_encap_egress_label;
        hdr.mpls0.exp = 0;
        hdr.mpls0.bos = 1;
        hdr.mpls0.ttl = 255;
    }
#endif

    action _act_pkt_inner_eth_encap() {
        hdr.eth2.setValid();
        hdr.eth2 = hdr.ethernet;
        hdr.eth2.ethertype = ig_md.ethertype;
    }

#ifdef HAVE_MPLS
    action act_pkt_mpls_rawip_ipv4_encap() {
        _act_pkt_mpls1_encap();
        hdr.mpls0.ttl = hdr.ipv4.ttl;
    }
#endif

#ifdef HAVE_MPLS
    action act_pkt_mpls_rawip_ipv6_encap() {
        _act_pkt_mpls1_encap();
        hdr.mpls0.ttl = hdr.ipv6.hop_limit;
    }
#endif


#ifdef HAVE_MPLS
    action act_pkt_mpls_rawip_ipv4_reencap() {
        hdr.mpls1.setInvalid();
        _act_pkt_mpls1_encap();
        hdr.mpls0.ttl = hdr.ipv4.ttl;
    }
#endif

#ifdef HAVE_MPLS
    action act_pkt_mpls_rawip_ipv6_reencap() {
        hdr.mpls1.setInvalid();
        _act_pkt_mpls1_encap();
        hdr.mpls0.ttl = hdr.ipv6.hop_limit;
    }
#endif


#ifdef HAVE_MPLS
    action act_pkt_mpls_l3vpn_ipv4_encap() {
        _act_pkt_mpls2_encap();
        hdr.mpls0.ttl = hdr.ipv4.ttl;
        hdr.mpls1.ttl = hdr.ipv4.ttl;
    }
#endif

#ifdef HAVE_MPLS
    action act_pkt_mpls_l3vpn_ipv6_encap() {
        _act_pkt_mpls2_encap();
        hdr.mpls0.ttl = hdr.ipv6.hop_limit;
        hdr.mpls1.ttl = hdr.ipv6.hop_limit;
    }
#endif

#ifdef HAVE_MPLS
#ifdef HAVE_BRIDGE
    action act_pkt_mpls_l2vpn_encap() {
        _act_pkt_inner_eth_encap();
        _act_pkt_mpls2_encap();
    }

    action act_pkt_mpls_xconnect_encap() {
        _act_pkt_inner_eth_encap();
        _act_pkt_mpls2_encap();
    }
#endif
#endif

#ifdef HAVE_SRV6
    action act_pkt_srv_l3vpn_encap() {
        hdr.ipv6.version = 6;
        hdr.ipv6.hop_limit = 255;
        hdr.ipv6.src_addr = ig_md.srv_target;
        hdr.ipv6.dst_addr = ig_md.srv_target;
        ig_md.ethertype = ETHERTYPE_IPV6;
    }
#endif

#ifdef HAVE_SRV6
    action act_pkt_srv_l3vpn_ipv4_encap() {
        hdr.ipv4b.setValid();
        hdr.ipv4b = hdr.ipv4;
        hdr.ipv4.setInvalid();
        hdr.ipv6.setValid();
        hdr.ipv6.payload_len = hdr.ipv4b.total_len;
        hdr.ipv6.next_hdr = IP_PROTOCOL_IPV4;
        act_pkt_srv_l3vpn_encap();
    }
#endif

#ifdef HAVE_SRV6
    action act_pkt_srv_l3vpn_ipv6_encap() {
        hdr.ipv6b.setValid();
        hdr.ipv6b = hdr.ipv6;
        hdr.ipv6.payload_len = hdr.ipv6b.payload_len + 40;
        hdr.ipv6.next_hdr = IP_PROTOCOL_IPV6;
        act_pkt_srv_l3vpn_encap();
    }
#endif

#ifdef HAVE_SRV6
    action act_pkt_srv_ipv4_decap() {
        hdr.ipv4.setValid();
        hdr.ipv4 = hdr.ipv4b;
        hdr.ipv4b.setInvalid();
        hdr.ipv6.setInvalid();
        ig_md.ethertype = ETHERTYPE_IPV4;
    }
#endif

#ifdef HAVE_SRV6
    action act_pkt_srv_ipv6_decap() {
        hdr.ipv6 = hdr.ipv6b;
        hdr.ipv6b.setInvalid();
        ig_md.ethertype = ETHERTYPE_IPV6;
    }
#endif

    table tbl_pkt_pre_emit {
        key = {
ig_md.srv_encap_l3vpn_valid:
            exact;
ig_md.mpls_encap_rawip_valid:
            exact;
ig_md.mpls_encap_l3vpn_valid:
            exact;
ig_md.mpls_encap_l2vpn_valid:
            exact;
ig_md.mpls_encap_xconnect_valid:
            exact;
ig_md.srv_remove:
            exact;
ig_md.mpls0_remove:
            exact;
ig_md.mpls1_remove:
            exact;
ig_md.mpls_encap_decap_sap_type:
            ternary;
ig_md.nexthop_id:
            ternary;
        }
        actions = {
#ifdef HAVE_MPLS
            act_pkt_mpls_l3vpn_ipv4_encap;
            act_pkt_mpls_l3vpn_ipv6_encap;
            act_pkt_mpls_rawip_ipv4_encap;
            act_pkt_mpls_rawip_ipv6_encap;
            act_pkt_mpls_rawip_ipv4_reencap;
            act_pkt_mpls_rawip_ipv6_reencap;
#ifdef HAVE_BRIDGE
            act_pkt_mpls_l2vpn_encap;
            act_pkt_mpls_xconnect_encap;
#endif
#endif
#ifdef HAVE_SRV6
            act_pkt_srv_l3vpn_ipv4_encap;
            act_pkt_srv_l3vpn_ipv6_encap;
#endif
            act_pkt_send_to_cpu;
#ifdef HAVE_MPLS
            act_pkt_mpls_ipv4_decap_0_1;
            act_pkt_mpls_ipv4_decap_0;
            act_pkt_mpls_ipv4_decap_1;
            act_pkt_mpls_ipv6_decap_0_1;
            act_pkt_mpls_ipv6_decap_0;
            act_pkt_mpls_ipv6_decap_1;
            act_pkt_mpls_swap_0;
            act_pkt_mpls_swap_1;
#endif
#ifdef HAVE_SRV6
            act_pkt_srv_ipv4_decap;
            act_pkt_srv_ipv6_decap;
#endif
            @defaultonly NoAction;
        }

        const entries = {
#ifdef HAVE_MPLS
            (0, 1, 0, 0, 0, 0, 0, 0, 4, _):act_pkt_mpls_rawip_ipv4_encap();
            (0, 1, 0, 0, 0, 0, 0, 0, 6, _):act_pkt_mpls_rawip_ipv6_encap();
            (0, 1, 0, 0, 0, 0, 0, 1, 4, _):act_pkt_mpls_rawip_ipv4_reencap();
            (0, 1, 0, 0, 0, 0, 0, 1, 6, _):act_pkt_mpls_rawip_ipv6_reencap();
            (0, 0, 1, 0, 0, 0, 0, 0, 4, _):act_pkt_mpls_l3vpn_ipv4_encap();
            (0, 0, 1, 0, 0, 0, 0, 0, 6, _):act_pkt_mpls_l3vpn_ipv6_encap();
#ifdef HAVE_BRIDGE
            (0, 0, 0, 1, 0, 0, 0, 0, _, _):act_pkt_mpls_l2vpn_encap();
            (0, 0, 0, 0, 1, 0, 0, 0, _, _):act_pkt_mpls_xconnect_encap();
#endif
#endif
#ifdef HAVE_SRV6
            (1, 0, 0, 0, 0, 0, 0, 0, 4, _):act_pkt_srv_l3vpn_ipv4_encap();
            (1, 0, 0, 0, 0, 0, 0, 0, 6, _):act_pkt_srv_l3vpn_ipv6_encap();
#endif
            (0, 0, 0, 0, 0, 0, 0, 0, _, CPU_PORT):act_pkt_send_to_cpu();
            (0, 0, 0, 0, 0, 0, 0, 1, _, CPU_PORT):act_pkt_send_to_cpu();
            (0, 0, 0, 0, 0, 0, 1, 0, _, CPU_PORT):act_pkt_send_to_cpu();
            (0, 0, 0, 0, 0, 0, 1, 1, _, CPU_PORT):act_pkt_send_to_cpu();
#ifdef HAVE_SRV6
            (1, 0, 0, 0, 0, 0, 0, 0, _, CPU_PORT):act_pkt_send_to_cpu();
            (0, 0, 0, 0, 0, 1, 0, 0, _, CPU_PORT):act_pkt_send_to_cpu();
#endif
#ifdef HAVE_MPLS
            (0, 0, 0, 0, 0, 0, 1, 1, 4, _):act_pkt_mpls_ipv4_decap_0_1();
            (0, 0, 0, 0, 0, 0, 1, 0, 4, _):act_pkt_mpls_ipv4_decap_0();
            (0, 0, 0, 0, 0, 0, 0, 1, 4, _):act_pkt_mpls_ipv4_decap_1();
            (0, 0, 0, 0, 0, 0, 1, 1, 6, _):act_pkt_mpls_ipv6_decap_0_1();
            (0, 0, 0, 0, 0, 0, 1, 0, 6, _):act_pkt_mpls_ipv6_decap_0();
            (0, 0, 0, 0, 0, 0, 0, 1, 6, _):act_pkt_mpls_ipv6_decap_1();
#endif
#ifdef HAVE_SRV6
            (0, 0, 0, 0, 0, 1, 0, 0, 4, _):act_pkt_srv_ipv4_decap();
            (0, 0, 0, 0, 0, 1, 0, 0, 6, _):act_pkt_srv_ipv6_decap();
#endif
#ifdef HAVE_MPLS
            (0, 0, 0, 0, 0, 0, 0, 0, 1, _):act_pkt_mpls_swap_0();
            (0, 0, 0, 0, 0, 0, 1, 0, 1, _):act_pkt_mpls_swap_1();
#endif
        }

        size = 20;
        default_action = NoAction();

    }

    apply {
        tbl_pkt_pre_emit.apply();
    }

}
#endif // _IG_CTL_PKT_PRE_EMIT_P4_
