/*
 * Copyright 2019-present GT RARE project
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

parser ig_prs_main(packet_in pkt,
                   /* User */
                   out headers hdr,
                   inout ingress_metadata_t ig_md,
                   /* Intrinsic */
                   inout standard_metadata_t ig_intr_md) {

    state start {
        ig_md.vlan_size = 0;
        transition select(ig_intr_md.ingress_port) {
CPU_PORT:
            prs_cpu;
        default:
            prs_data;
        }
    }

    state prs_data {
        ig_md.ingress_id = (SubIntId_t)ig_intr_md.ingress_port;
        transition select(ig_intr_md.instance_type) {
32w0:
            prs_ethernet;
        default:
            prs_cpu;
        }
    }

    state prs_cpu {
        ig_md.vlan_size = 2;
        pkt.extract(hdr.cpu);
        ig_md.ingress_id = hdr.cpu.port;
        transition prs_ethernet;
    }

    state prs_ethernet {
        pkt.extract(hdr.ethernet);
        transition select(hdr.ethernet.ethertype) {
0 &&& 0xfe00:
            prs_llc; /* LLC SAP frame */
0 &&& 0xfa00:
            prs_llc; /* LLC SAP frame */
ETHERTYPE_VLAN :
            prs_vlan;
ETHERTYPE_PPPOE_CTRL :
            prs_pppoeCtrl;
ETHERTYPE_PPPOE_DATA :
            prs_pppoeData;
ETHERTYPE_NSH :
            prs_nsh;
ETHERTYPE_MPLS_UCAST :
            prs_mpls0;
ETHERTYPE_IPV4:
            prs_ipv4;
ETHERTYPE_IPV6:
            prs_ipv6;
ETHERTYPE_ROUTEDMAC:
            prs_eth6;
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
            prs_llc; /* LLC SAP frame */
0 &&& 0xfa00:
            prs_llc; /* LLC SAP frame */
ETHERTYPE_PPPOE_CTRL :
            prs_pppoeCtrl;
ETHERTYPE_PPPOE_DATA :
            prs_pppoeData;
ETHERTYPE_NSH :
            prs_nsh;
ETHERTYPE_MPLS_UCAST :
            prs_mpls0;
ETHERTYPE_IPV4:
            prs_ipv4;
ETHERTYPE_IPV6:
            prs_ipv6;
ETHERTYPE_ROUTEDMAC:
            prs_eth6;
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
PPPTYPE_MPLS_UCAST:
            prs_mpls0;
PPPTYPE_ROUTEDMAC:
            prs_pppoeb;
        default:
            prs_pppoeDataCtrl;
        }
    }

    state prs_pppoeDataCtrl {
        ig_md.pppoe_ctrl_valid = 1;
        transition accept;
    }

    state prs_pppoeb {
        pkt.extract(hdr.pppoeB);
        transition prs_eth6;
    }

    state prs_eth6 {
        pkt.extract(hdr.eth6);
        transition accept;
    }

    state prs_nsh {
        ig_md.nsh_valid = 1;
        pkt.extract(hdr.nsh);
        transition select(hdr.nsh.next_proto) {
8w1:
            prs_ipv4;
8w2:
            prs_ipv6;
8w5:
            prs_mpls0;
        default:
            accept;
        }
    }

    state prs_mpls0 {
        pkt.extract(hdr.mpls0);
        ig_md.mpls0_valid = 1;
        transition select(hdr.mpls0.bos) {
1w0:
            prs_mpls1;
1w1:
            prs_mpls_bos;
        default:
            accept;
        }
    }

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

    state prs_mpls_bos {
        transition select((pkt.lookahead<bit<4>>())[3:0]) {
4w0x4:
            prs_ipv4; /* IPv4 only for now */
4w0x6:
            prs_ipv6; /* IPv6 is in next lab */
4w0x5:
            prs_bier; /* BIER is in next lab */
        default:
            prs_eth2; /* EoMPLS is pausing problem if we don't resubmit() */
        }
    }

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

    state prs_bier {
        pkt.extract(hdr.bier);
        transition select(hdr.bier.proto) {
6w0x4:
            prs_ipv4;
6w0x6:
            prs_ipv6;
        default:
            accept;
        }
    }


    state prs_ipv4 {
        pkt.extract(hdr.ipv4);
        ig_md.layer4_length = hdr.ipv4.total_len - 20;
        ig_md.ipv4_valid = 1;
        transition select(hdr.ipv4.protocol) {
IP_PROTOCOL_GRE:
            prs_gre;
IP_PROTOCOL_UDP:
            prs_udp;
IP_PROTOCOL_TCP:
            prs_tcp;
IP_PROTOCOL_IPV4:
            prs_ipv4b;
IP_PROTOCOL_IPV6:
            prs_ipv6b;
IP_PROTOCOL_SRL2:
            prs_eth3;
        default:
            accept;
        }
    }

    state prs_ipv6 {
        pkt.extract(hdr.ipv6);
        ig_md.layer4_length = hdr.ipv6.payload_len;
        ig_md.ipv6_valid = 1;
        transition select(hdr.ipv6.next_hdr) {
IP_PROTOCOL_GRE:
            prs_gre;
IP_PROTOCOL_UDP:
            prs_udp;
IP_PROTOCOL_TCP:
            prs_tcp;
IP_PROTOCOL_IPV4:
            prs_ipv4b;
IP_PROTOCOL_IPV6:
            prs_ipv6b;
IP_PROTOCOL_SRL2:
            prs_eth3;
        default:
            accept;
        }
    }


    state prs_gre {
        pkt.extract(hdr.gre);
        ig_md.layer4_srcprt = 0;
        ig_md.layer4_dstprt = 0;
        transition select(hdr.gre.gretyp) {
ETHERTYPE_ROUTEDMAC:
            prs_eth5;
        default:
            accept;
        }
    }

    state prs_eth5 {
        pkt.extract(hdr.eth5);
        transition accept;
    }

    state prs_udp {
        pkt.extract(hdr.udp);
        ig_md.layer4_srcprt = hdr.udp.src_port;
        ig_md.layer4_dstprt = hdr.udp.dst_port;
        transition select(hdr.udp.src_port, hdr.udp.dst_port) {
            (1701, 0 &&& 0):
                prs_l2tp;
            (0 &&& 0, 1701):
                prs_l2tp;
            (4789, 0 &&& 0):
                prs_vxlan;
            (0 &&& 0, 4789):
                prs_vxlan;
            (2554, 0 &&& 0):
                prs_pckoudp;
            (0 &&& 0, 2554):
                prs_pckoudp;
        default:
            accept;
        }
    }

    state prs_tcp {
        pkt.extract(hdr.tcp);
        ig_md.layer4_srcprt = hdr.tcp.src_port;
        ig_md.layer4_dstprt = hdr.tcp.dst_port;
        transition accept;
    }

    state prs_l2tp {
        pkt.extract(hdr.l2tp);
        transition select(hdr.l2tp.ppptyp) {
PPPTYPE_ROUTEDMAC:
            prs_l2tpbr;
        default:
            accept;
        }
    }


    state prs_vxlan {
        pkt.extract(hdr.vxlan);
        transition prs_eth5;
    }

    state prs_pckoudp {
        transition prs_eth5;
    }

    state prs_l2tpbr {
        pkt.extract(hdr.l2tpbr);
        transition prs_eth5;
    }

    state prs_eth3 {
        pkt.extract(hdr.eth3);
        transition accept;
    }

    state prs_ipv4b {
        pkt.extract(hdr.ipv4b);
        transition accept;
    }

    state prs_ipv6b {
        pkt.extract(hdr.ipv6b);
        transition accept;
    }

    state prs_arp {
        pkt.extract(hdr.arp);
        ig_md.arp_valid = 1;
        transition accept;
    }

    state prs_control {
        ig_md.llc_valid = 1;
        transition accept;
    }

    state prs_llc {
        pkt.extract(hdr.llc);
        ig_md.llc_valid = 1;
        transition select(hdr.llc.dsap, hdr.llc.ssap) {
            /*
             * (0xaa, 0xaa): prs_snap_header;
             * From switch.p4 this case should be processed.
             * We are not there yet :-)
             */
            (0xfe, 0xfe): accept;
        default:
            accept;
        }
    }

}

#endif // _INGRESS_PARSER_P4_
