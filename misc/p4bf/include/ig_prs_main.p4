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

#ifndef _INGRESS_PARSER_P4_
#define _INGRESS_PARSER_P4_

/*------------------ I N G R E S S   P A R S E R -----------------------------*/

parser ig_prs_main(packet_in pkt,
                   /* User */
                   out headers hdr, out ingress_metadata_t ig_md,
                   /* Intrinsic */
                   out ingress_intrinsic_metadata_t ig_intr_md)
{

    Checksum()ipv4_checksum;
#ifdef HAVE_NAT
    Checksum() tcp_checksum;
    Checksum() udp_checksum;
#endif

    state start {
        pkt.extract(ig_intr_md);
        pkt.advance(PORT_METADATA_SIZE);
        ig_md.always_zero = 0; // hack
#ifdef HAVE_INQOS
        ig_md.inqos_id = 0;
        ig_md.inqos_res = MeterColor_t.GREEN;
#endif
#ifdef HAVE_OUTQOS
        ig_md.outqos_id = 0;
        ig_md.outqos_res = MeterColor_t.GREEN;
#endif
#ifdef HAVE_FLOWSPEC
        ig_md.flowspec_id = 0;
        ig_md.flowspec_res = MeterColor_t.GREEN;
#endif
#ifdef HAVE_MCAST
        ig_md.clone_session = 0;
        ig_md.rpf_iface = 0;
#endif
#ifdef HAVE_PPPOE
        ig_md.pppoe_ctrl_valid = 0;
        ig_md.pppoe_data_valid = 0;
#endif
        ig_md.source_id = 0;
        ig_md.target_id = 0;
        ig_md.nexthop_id = 0;
#ifdef HAVE_BRIDGE
        ig_md.bridge_id = 0;
        ig_md.bridge_src = 0;
        ig_md.bridge_trg = 0;
#endif
#ifdef NEED_PKTLEN
        ig_md.pktlen = 0;
#endif
        ig_md.ethertype = 0;
        ig_md.vrf = 0;
        ig_md.saw_rsvp = 0;
#ifdef HAVE_MPLS
        ig_md.mpls_encap_egress_label = 0;
        ig_md.mpls_encap_svc_label = 0;
#endif
#ifdef HAVE_SRV6
        ig_md.srv_target = 0;
#endif
        ig_md.mpls_op_type = 0;
        ig_md.srv_op_type = 0;
        ig_md.mpls0_remove = 0;
        ig_md.mpls1_remove = 0;
        ig_md.srv_remove = 0;
#ifdef HAVE_MPLS
        ig_md.mpls0_valid = 0;
        ig_md.mpls1_valid = 0;
#endif
        ig_md.arp_valid = 0;
        ig_md.ipv4_valid = 0;
        ig_md.ipv6_valid = 0;
        ig_md.srv_encap_l3vpn_valid = 0;
        ig_md.mpls_encap_rawip_valid = 0;
        ig_md.mpls_encap_l3vpn_valid = 0;
        ig_md.mpls_encap_l2vpn_valid = 0;
        ig_md.mpls_encap_xconnect_valid = 0;
        ig_md.mpls_encap_decap_sap_type = 0;
        ig_md.l4_lookup = { 0, 0 };
        transition meta_init;
    }

    state meta_init {
        transition select(ig_intr_md.ingress_port) {
CPU_PORT:
            prs_cpu;
RECIR_PORT:
            prs_recir;
        default:
            prs_normal;
        }
    }

    state prs_cpu {
        ig_md.ingress_id = 0;
        pkt.extract(hdr.cpu);
        pkt.extract(hdr.ethernet);
        transition select(hdr.ethernet.ethertype) {
ETHERTYPE_VLAN:
            prs_cpu_vlan;
        default:
            accept;
        }
    }

    state prs_cpu_vlan {
        pkt.extract(hdr.vlan);
        transition accept;
    }

    state prs_recir {
        pkt.extract(hdr.cpu);
        ig_md.ingress_id = hdr.cpu.port;
        transition prs_ethernet;
    }

    state prs_normal {
        ig_md.ingress_id = (SubIntId_t)ig_intr_md.ingress_port;
        transition prs_ethernet;
    }

    state prs_ethernet {
        pkt.extract(hdr.ethernet);
        transition select(hdr.ethernet.ethertype) {
0 &&& 0xfe00:
            prs_llc;	/* LLC SAP frame */
0 &&& 0xfa00:
            prs_llc;	/* LLC SAP frame */
ETHERTYPE_VLAN:
            prs_vlan;
#ifdef HAVE_PPPOE
ETHERTYPE_PPPOE_CTRL :
            prs_pppoeCtrl;
ETHERTYPE_PPPOE_DATA :
            prs_pppoeData;
#endif
#ifdef HAVE_MPLS
ETHERTYPE_MPLS_UCAST:
            prs_mpls0;
#endif
ETHERTYPE_IPV4:
            prs_ipv4;
ETHERTYPE_IPV6:
            prs_ipv6;
#ifdef HAVE_TAP
ETHERTYPE_ROUTEDMAC:
            prs_eth6;
#endif
ETHERTYPE_ARP:
            prs_arp;
ETHERTYPE_LACP:
            prs_control;
ETHERTYPE_LLDP:
            prs_control;
        default:
            accept;
        }
    }

    state prs_vlan {
        pkt.extract(hdr.vlan);
        transition select(hdr.vlan.ethertype) {
0 &&& 0xfe00:
            prs_llc;		/* LLC SAP frame */
0 &&& 0xfa00:
            prs_llc;	/* LLC SAP frame */
#ifdef HAVE_PPPOE
ETHERTYPE_PPPOE_CTRL :
            prs_pppoeCtrl;
ETHERTYPE_PPPOE_DATA :
            prs_pppoeData;
#endif
#ifdef HAVE_MPLS
ETHERTYPE_MPLS_UCAST:
            prs_mpls0;
#endif
ETHERTYPE_IPV4:
            prs_ipv4;
ETHERTYPE_IPV6:
            prs_ipv6;
#ifdef HAVE_TAP
ETHERTYPE_ROUTEDMAC:
            prs_eth6;
#endif
ETHERTYPE_ARP:
            prs_arp;
ETHERTYPE_LACP:
            prs_control;
ETHERTYPE_LLDP:
            prs_control;
        default:
            accept;
        }
    }


#ifdef HAVE_PPPOE
    state prs_pppoeCtrl {
        pkt.extract(hdr.pppoeC);
        ig_md.pppoe_ctrl_valid = 1;
        transition accept;
    }

    state prs_pppoeData {
        pkt.extract(hdr.pppoeD);
        ig_md.pppoe_data_valid = 1;
        transition select(hdr.pppoeD.ppptyp) {
PPPTYPE_IPV4:
            prs_ipv4;
PPPTYPE_IPV6:
            prs_ipv6;
#ifdef HAVE_MPLS
PPPTYPE_MPLS_UCAST:
            prs_mpls0;
#endif
        default:
            prs_pppoeDataCtrl;
        }
    }

    state prs_pppoeDataCtrl {
        ig_md.pppoe_ctrl_valid = 1;
        transition accept;
    }
#endif



#ifdef HAVE_TAP
    state prs_eth6 {
        pkt.extract(hdr.eth6);
        transition accept;
    }
#endif


#ifdef HAVE_MPLS
    state prs_mpls0 {
        pkt.extract(hdr.mpls0);
        ig_md.mpls0_valid = 1;
        transition select(hdr.mpls0.bos) {
0:
            prs_mpls1;
1:
            prs_mpls_bos;
        default:
            accept;
        }
    }
#endif

#ifdef HAVE_MPLS
    state prs_mpls1 {
        pkt.extract(hdr.mpls1);
        ig_md.mpls1_valid = 1;
        transition select(hdr.mpls1.bos) {
1w0:
            accept;
1w1:
            prs_mpls_bos;
        default:
            accept;
        }
    }
#endif

#ifdef HAVE_MPLS
    state prs_mpls_bos {
        transition select((pkt.lookahead < bit < 4 >> ())[3:0]) {
0x4:
            prs_ipv4;		/* IPv4 only for now */
0x6:
            prs_ipv6;		/* IPv6 is in next lab */
        default:
            prs_eth2;		/* EoMPLS is pausing problem if we don't resubmit() */
        }
    }
#endif

    state prs_eth2 {
        pkt.extract(hdr.eth2);
        transition select(hdr.eth2.ethertype) {
ETHERTYPE_IPV4:
            prs_ipv4;
ETHERTYPE_IPV6:
            prs_ipv6;
        default:
            accept;
        }
    }
    state prs_ipv4 {
        pkt.extract(hdr.ipv4);
        ipv4_checksum.add(hdr.ipv4);
        ig_md.ipv4_valid = 1;
        ig_md.l4_lookup = pkt.lookahead<l4_lookup_t>();
#ifdef HAVE_NAT
        tcp_checksum.subtract({hdr.ipv4.src_addr});
        tcp_checksum.subtract({hdr.ipv4.dst_addr});
        udp_checksum.subtract({hdr.ipv4.src_addr});
        udp_checksum.subtract({hdr.ipv4.dst_addr});
#endif
#ifdef NEED_PKTLEN
        ig_md.pktlen = hdr.ipv4.total_len;
#endif
        transition select(hdr.ipv4.protocol) {
#ifdef HAVE_GRE
IP_PROTOCOL_GRE:
            prs_gre;
#endif
IP_PROTOCOL_UDP:
            prs_udp;
IP_PROTOCOL_TCP:
            prs_tcp;
#ifdef HAVE_SRV6
IP_PROTOCOL_IPV4:
            prs_ipv4b;
IP_PROTOCOL_IPV6:
            prs_ipv6b;
#endif
        default:
            accept;
        }
    }


    state prs_ipv6 {
        pkt.extract(hdr.ipv6);
        ig_md.ipv6_valid = 1;
        ig_md.l4_lookup = pkt.lookahead<l4_lookup_t>();
#ifdef HAVE_NAT
        tcp_checksum.subtract({hdr.ipv6.src_addr});
        tcp_checksum.subtract({hdr.ipv6.dst_addr});
        udp_checksum.subtract({hdr.ipv6.src_addr});
        udp_checksum.subtract({hdr.ipv6.dst_addr});
#endif
#ifdef NEED_PKTLEN
//        ig_md.pktlen = hdr.ipv6.payload_len + 40;
#endif
        transition select(hdr.ipv6.next_hdr) {
#ifdef HAVE_GRE
IP_PROTOCOL_GRE:
            prs_gre;
#endif
IP_PROTOCOL_UDP:
            prs_udp;
IP_PROTOCOL_TCP:
            prs_tcp;
#ifdef HAVE_SRV6
IP_PROTOCOL_IPV4:
            prs_ipv4b;
IP_PROTOCOL_IPV6:
            prs_ipv6b;
#endif
        default:
            accept;
        }
    }

    state prs_control {
        ig_md.nexthop_id = CPU_PORT;
        transition accept;
    }

    state prs_arp {
        pkt.extract(hdr.arp);
        ig_md.arp_valid = 1;
#ifdef NEED_PKTLEN
        ig_md.pktlen = 28;
#endif
        transition accept;
    }

    state prs_llc {
        pkt.extract(hdr.llc);
        ig_md.nexthop_id = CPU_PORT;
        transition accept;
    }

#ifdef HAVE_GRE
    state prs_gre {
        pkt.extract(hdr.gre);
        ig_md.layer4_srcprt = 0;
        ig_md.layer4_dstprt = 0;
        transition accept;
    }
#endif

    state prs_udp {
        pkt.extract(hdr.udp);
        ig_md.layer4_srcprt = hdr.udp.src_port;
        ig_md.layer4_dstprt = hdr.udp.dst_port;
#ifdef HAVE_NAT
        udp_checksum.subtract({hdr.udp.checksum});
        udp_checksum.subtract({hdr.udp.src_port});
        udp_checksum.subtract({hdr.udp.dst_port});
        ig_md.checksum_udp_tmp = udp_checksum.get();
#endif
        transition select(hdr.udp.src_port, hdr.udp.dst_port) {
#ifdef HAVE_L2TP
            (1701, 0 &&& 0):
                prs_l2tp;
            (0 &&& 0, 1701):
                prs_l2tp;
#endif
#ifdef HAVE_VXLAN
            (4789, 0 &&& 0):
                prs_vxlan;
            (0 &&& 0, 4789):
                prs_vxlan;
#endif
#ifdef HAVE_PCKOUDP
            (2554, 0 &&& 0):
                prs_pckoudp;
            (0 &&& 0, 2554):
                prs_pckoudp;
#endif
        default:
            accept;
        }
    }

    state prs_tcp {
        pkt.extract(hdr.tcp);
        ig_md.layer4_srcprt = hdr.tcp.src_port;
        ig_md.layer4_dstprt = hdr.tcp.dst_port;
#ifdef HAVE_NAT
        tcp_checksum.subtract({hdr.tcp.checksum});
        tcp_checksum.subtract({hdr.tcp.src_port});
        tcp_checksum.subtract({hdr.tcp.dst_port});
        ig_md.checksum_tcp_tmp = tcp_checksum.get();
#endif
        transition accept;
    }

#ifdef HAVE_L2TP
    state prs_l2tp {
        pkt.extract(hdr.l2tp);
        transition accept;
    }
#endif

#ifdef HAVE_VXLAN
    state prs_vxlan {
        pkt.extract(hdr.vxlan);
        transition accept;
    }
#endif

#ifdef HAVE_PCKOUDP
    state prs_pckoudp {
        transition accept;
    }
#endif

#ifdef HAVE_SRV6
    state prs_ipv4b {
        pkt.extract(hdr.ipv4b);
        transition accept;
    }

    state prs_ipv6b {
        pkt.extract(hdr.ipv6b);
        transition accept;
    }
#endif

}
#endif	// _INGRESS_PARSER_P4_
