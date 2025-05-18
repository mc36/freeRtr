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

#ifndef _IG_CTL_TUNNEL_P4_
#define _IG_CTL_TUNNEL_P4_


control IngressControlTunnel(inout headers hdr,
                             inout ingress_metadata_t ig_md,
                             inout standard_metadata_t ig_intr_md) {



    action act_tunnel_gre(SubIntId_t port) {
        hdr.ethernet.ethertype = hdr.gre.gretyp;
        ig_intr_md.egress_spec = (PortId_t)port;
        ig_md.need_recir = 1;
        ig_md.source_id = port;
    }


    action act_tunnel_tmux(SubIntId_t port) {
        if (hdr.tmux.proto == IP_PROTOCOL_SKIP) hdr.ethernet.ethertype = ETHERTYPE_SGT;
        if (hdr.tmux.proto == IP_PROTOCOL_NSH) hdr.ethernet.ethertype = ETHERTYPE_NSH;
        if (hdr.tmux.proto == IP_PROTOCOL_IPV4) hdr.ethernet.ethertype = ETHERTYPE_IPV4;
        if (hdr.tmux.proto == IP_PROTOCOL_IPV6) hdr.ethernet.ethertype = ETHERTYPE_IPV6;
        if (hdr.tmux.proto == IP_PROTOCOL_MPLS_IN_IP) hdr.ethernet.ethertype = ETHERTYPE_MPLS_UCAST;
        if (hdr.tmux.proto == IP_PROTOCOL_SRL2) hdr.ethernet.ethertype = ETHERTYPE_ROUTEDMAC;
        ig_intr_md.egress_spec = (PortId_t)port;
        ig_md.need_recir = 1;
        ig_md.source_id = port;
    }


    action act_tunnel_ipip(SubIntId_t port, ethertype_t ethtyp) {
        hdr.ethernet.ethertype = ethtyp;
        ig_intr_md.egress_spec = (PortId_t)port;
        ig_md.need_recir = 1;
        ig_md.source_id = port;
    }


    action act_tunnel_l2tp(SubIntId_t port) {
        if ((hdr.l2tp.flags & 0x8000) != 0) return;
        if ((hdr.l2tp.ppptyp & 0x8000) != 0) return;
        if (hdr.l2tp.ppptyp == PPPTYPE_SGT) hdr.ethernet.ethertype = ETHERTYPE_SGT;
        if (hdr.l2tp.ppptyp == PPPTYPE_NSH) hdr.ethernet.ethertype = ETHERTYPE_NSH;
        if (hdr.l2tp.ppptyp == PPPTYPE_IPV4) hdr.ethernet.ethertype = ETHERTYPE_IPV4;
        if (hdr.l2tp.ppptyp == PPPTYPE_IPV6) hdr.ethernet.ethertype = ETHERTYPE_IPV6;
        if (hdr.l2tp.ppptyp == PPPTYPE_MPLS_UCAST) hdr.ethernet.ethertype = ETHERTYPE_MPLS_UCAST;
        if (hdr.l2tp.ppptyp == PPPTYPE_ROUTEDMAC) hdr.ethernet.ethertype = ETHERTYPE_ROUTEDMAC;
        ig_intr_md.egress_spec = (PortId_t)port;
        ig_md.need_recir = 1;
        ig_md.source_id = port;
    }


    action act_tunnel_l3tp(SubIntId_t port) {
        if (hdr.l3tp.tidsid == 0) return;
        if ((hdr.l3tp.ppptyp & 0x8000) != 0) return;
        if (hdr.l3tp.ppptyp == PPPTYPE_SGT) hdr.ethernet.ethertype = ETHERTYPE_SGT;
        if (hdr.l3tp.ppptyp == PPPTYPE_NSH) hdr.ethernet.ethertype = ETHERTYPE_NSH;
        if (hdr.l3tp.ppptyp == PPPTYPE_IPV4) hdr.ethernet.ethertype = ETHERTYPE_IPV4;
        if (hdr.l3tp.ppptyp == PPPTYPE_IPV6) hdr.ethernet.ethertype = ETHERTYPE_IPV6;
        if (hdr.l3tp.ppptyp == PPPTYPE_MPLS_UCAST) hdr.ethernet.ethertype = ETHERTYPE_MPLS_UCAST;
        if (hdr.l3tp.ppptyp == PPPTYPE_ROUTEDMAC) hdr.ethernet.ethertype = ETHERTYPE_ROUTEDMAC;
        ig_intr_md.egress_spec = (PortId_t)port;
        ig_md.need_recir = 1;
        ig_md.source_id = port;
    }


    action act_tunnel_vxlan(SubIntId_t port) {
        ig_intr_md.egress_spec = (PortId_t)port;
        ig_md.need_recir = 1;
        ig_md.source_id = port;
    }


    action act_tunnel_etherip(SubIntId_t port) {
        ig_intr_md.egress_spec = (PortId_t)port;
        ig_md.need_recir = 1;
        ig_md.source_id = port;
    }

    action act_tunnel_pckoudp(SubIntId_t port) {
        ig_intr_md.egress_spec = (PortId_t)port;
        ig_md.need_recir = 1;
        ig_md.source_id = port;
    }


    action act_tunnel_amt(SubIntId_t port) {
        if (ig_md.amt_type == 4) hdr.ethernet.ethertype = ETHERTYPE_IPV4;
        if (ig_md.amt_type == 6) hdr.ethernet.ethertype = ETHERTYPE_IPV6;
        ig_intr_md.egress_spec = (PortId_t)port;
        ig_md.need_recir = 1;
        ig_md.source_id = port;
        if (hdr.amt.type != 6) ig_md.need_recir = 0;
    }


    action act_tunnel_gtp(SubIntId_t port) {
        if (ig_md.gtp_type == 4) hdr.ethernet.ethertype = ETHERTYPE_IPV4;
        if (ig_md.gtp_type == 6) hdr.ethernet.ethertype = ETHERTYPE_IPV6;
        ig_intr_md.egress_spec = (PortId_t)port;
        ig_md.need_recir = 1;
        ig_md.source_id = port;
    }



    table tbl_tunnel4 {
        key = {
ig_md.vrf:
            exact;
hdr.ipv4.protocol:
            exact;
hdr.ipv4.src_addr:
            exact;
hdr.ipv4.dst_addr:
            exact;
ig_md.layer4_srcprt:
            exact;
ig_md.layer4_dstprt:
            exact;
        }
        actions = {
            act_tunnel_gre;
            act_tunnel_tmux;
            act_tunnel_ipip;
            act_tunnel_l2tp;
            act_tunnel_l3tp;
            act_tunnel_vxlan;
            act_tunnel_etherip;
            act_tunnel_pckoudp;
            act_tunnel_amt;
            act_tunnel_gtp;
            @defaultonly NoAction;
        }
        size = IPV4_TUNNEL_TABLE_SIZE;
        const default_action = NoAction();
    }


    table tbl_tunnel6 {
        key = {
ig_md.vrf:
            exact;
hdr.ipv6.next_hdr:
            exact;
hdr.ipv6.src_addr:
            exact;
hdr.ipv6.dst_addr:
            exact;
ig_md.layer4_srcprt:
            exact;
ig_md.layer4_dstprt:
            exact;
        }
        actions = {
            act_tunnel_gre;
            act_tunnel_tmux;
            act_tunnel_ipip;
            act_tunnel_l2tp;
            act_tunnel_l3tp;
            act_tunnel_vxlan;
            act_tunnel_etherip;
            act_tunnel_pckoudp;
            act_tunnel_amt;
            act_tunnel_gtp;
            @defaultonly NoAction;
        }
        size = IPV6_TUNNEL_TABLE_SIZE;
        const default_action = NoAction();
    }


    apply {

        ig_md.need_recir = 0;

        if (hdr.ipv4.isValid()) {
            tbl_tunnel4.apply();
        }
        if (hdr.ipv6.isValid()) {
            tbl_tunnel6.apply();
        }

        if (ig_md.need_recir == 0) return;

        if (ig_md.layer3_frag != 0) {
            ig_md.dropping = 1;
        }

        hdr.vxlan.setInvalid();
        hdr.etherip.setInvalid();
        hdr.l2tp.setInvalid();
        hdr.l3tp.setInvalid();
        hdr.amt.setInvalid();
        hdr.gtp.setInvalid();
        hdr.udp.setInvalid();
        hdr.gre.setInvalid();
        hdr.tmux.setInvalid();
        hdr.ipv4.setInvalid();
        hdr.ipv6.setInvalid();
        if (hdr.eth5.isValid()) {
            hdr.eth6.setValid();
            hdr.eth6 = hdr.eth5;
            hdr.eth5.setInvalid();
            hdr.ethernet.ethertype = ETHERTYPE_ROUTEDMAC;
        }
        hdr.cpu.setValid();
        hdr.cpu.port = ig_md.source_id;

    }


}


#endif // _IG_CTL_TUNNEL_P4_
