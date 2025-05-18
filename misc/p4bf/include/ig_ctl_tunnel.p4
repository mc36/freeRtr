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

#ifdef HAVE_TUN

control IngressControlTunnel(inout headers hdr, inout ingress_metadata_t ig_md,
                             in ingress_intrinsic_metadata_t ig_intr_md,
                             inout ingress_intrinsic_metadata_for_deparser_t ig_dprsr_md,
                             inout ingress_intrinsic_metadata_for_tm_t ig_tm_md)
{


#ifdef HAVE_GTP
    SubIntId_t gtp_hit;
#endif

#ifdef HAVE_L2TP
    SubIntId_t l2tp_hit;
#endif

#ifdef HAVE_L3TP
    SubIntId_t l3tp_hit;
#endif

#ifdef HAVE_TMUX
    SubIntId_t tmux_hit;
#endif




#ifdef HAVE_GRE
    action act_tunnel_gre(SubIntId_t port) {
        hdr.ethernet.ethertype = hdr.gre.gretyp;
        ig_md.source_id = port;
        ig_md.ipv4_valid = 0;
        ig_md.ipv6_valid = 0;
        hdr.vlan.setInvalid();
        hdr.vlanq.setInvalid();
        ig_tm_md.ucast_egress_port = RECIR_PORT;
        ig_tm_md.bypass_egress = 1;
//        recirculate(RECIR_PORT);
        hdr.cpu.setValid();
        hdr.cpu._padding1 = 0;
        hdr.cpu._padding2 = 0;
        hdr.cpu.port = port;
        hdr.gre.setInvalid();
        hdr.ipv4.setInvalid();
        hdr.ipv6.setInvalid();
#ifdef HAVE_FRAG
        ig_dprsr_md.drop_ctl = ig_dprsr_md.drop_ctl | ig_md.layer3_frag;
#endif
    }
#endif


#ifdef HAVE_TMUX
    action act_tunnel_tmux(SubIntId_t port) {
        tmux_hit = port;
#ifdef HAVE_FRAG
        ig_dprsr_md.drop_ctl = ig_dprsr_md.drop_ctl | ig_md.layer3_frag;
#endif
    }
#endif




#ifdef HAVE_IPIP

    action act_tunnel_ipip(SubIntId_t port, ethertype_t ethtyp) {
        hdr.ethernet.ethertype = ethtyp;
        ig_md.source_id = port;
        ig_md.ipv4_valid = 0;
        ig_md.ipv6_valid = 0;
        hdr.vlan.setInvalid();
        hdr.vlanq.setInvalid();
        ig_tm_md.ucast_egress_port = RECIR_PORT;
        ig_tm_md.bypass_egress = 1;
//        recirculate(RECIR_PORT);
        hdr.cpu.setValid();
        hdr.cpu._padding1 = 0;
        hdr.cpu._padding2 = 0;
        hdr.cpu.port = port;
        hdr.ipv4.setInvalid();
        hdr.ipv6.setInvalid();
#ifdef HAVE_FRAG
        ig_dprsr_md.drop_ctl = ig_dprsr_md.drop_ctl | ig_md.layer3_frag;
#endif
    }

#endif



#ifdef HAVE_L2TP
    action act_tunnel_l2tp(SubIntId_t port) {
        l2tp_hit = port;
#ifdef HAVE_FRAG
        ig_dprsr_md.drop_ctl = ig_dprsr_md.drop_ctl | ig_md.layer3_frag;
#endif
    }
#endif


#ifdef HAVE_L3TP
    action act_tunnel_l3tp(SubIntId_t port) {
        l3tp_hit = port;
#ifdef HAVE_FRAG
        ig_dprsr_md.drop_ctl = ig_dprsr_md.drop_ctl | ig_md.layer3_frag;
#endif
    }
#endif



#ifdef HAVE_VXLAN
    action act_tunnel_vxlan(SubIntId_t port) {
        ig_md.source_id = port;
        ig_md.ipv4_valid = 0;
        ig_md.ipv6_valid = 0;
        hdr.ethernet.setInvalid();
        hdr.vlan.setInvalid();
        hdr.vlanq.setInvalid();
        ig_tm_md.ucast_egress_port = RECIR_PORT;
        ig_tm_md.bypass_egress = 1;
//        recirculate(RECIR_PORT);
        hdr.cpu.setValid();
        hdr.cpu._padding1 = 0;
        hdr.cpu._padding2 = 0;
        hdr.cpu.port = port;
        hdr.vxlan.setInvalid();
        hdr.udp.setInvalid();
        hdr.ipv4.setInvalid();
        hdr.ipv6.setInvalid();
#ifdef HAVE_FRAG
        ig_dprsr_md.drop_ctl = ig_dprsr_md.drop_ctl | ig_md.layer3_frag;
#endif
    }
#endif


#ifdef HAVE_ETHERIP
    action act_tunnel_etherip(SubIntId_t port) {
        ig_md.source_id = port;
        ig_md.ipv4_valid = 0;
        ig_md.ipv6_valid = 0;
        hdr.ethernet.setInvalid();
        hdr.vlan.setInvalid();
        hdr.vlanq.setInvalid();
        ig_tm_md.ucast_egress_port = RECIR_PORT;
        ig_tm_md.bypass_egress = 1;
//        recirculate(RECIR_PORT);
        hdr.cpu.setValid();
        hdr.cpu._padding1 = 0;
        hdr.cpu._padding2 = 0;
        hdr.cpu.port = port;
        hdr.etherip.setInvalid();
        hdr.ipv4.setInvalid();
        hdr.ipv6.setInvalid();
#ifdef HAVE_FRAG
        ig_dprsr_md.drop_ctl = ig_dprsr_md.drop_ctl | ig_md.layer3_frag;
#endif
    }
#endif



#ifdef HAVE_PCKOUDP
    action act_tunnel_pckoudp(SubIntId_t port) {
        ig_md.source_id = port;
        ig_md.ipv4_valid = 0;
        ig_md.ipv6_valid = 0;
        hdr.ethernet.setInvalid();
        hdr.vlan.setInvalid();
        hdr.vlanq.setInvalid();
        ig_tm_md.ucast_egress_port = RECIR_PORT;
        ig_tm_md.bypass_egress = 1;
//        recirculate(RECIR_PORT);
        hdr.cpu.setValid();
        hdr.cpu._padding1 = 0;
        hdr.cpu._padding2 = 0;
        hdr.cpu.port = port;
        hdr.udp.setInvalid();
        hdr.ipv4.setInvalid();
        hdr.ipv6.setInvalid();
#ifdef HAVE_FRAG
        ig_dprsr_md.drop_ctl = ig_dprsr_md.drop_ctl | ig_md.layer3_frag;
#endif
    }
#endif


#ifdef HAVE_GTP
    action act_tunnel_gtp(SubIntId_t port) {
        ig_md.source_id = port;
        gtp_hit = port;
#ifdef HAVE_FRAG
        ig_dprsr_md.drop_ctl = ig_dprsr_md.drop_ctl | ig_md.layer3_frag;
#endif
    }
#endif



    table tbl_tunnel4 {
        key = {
ig_md.layer4_srcprt:
            exact;
ig_md.layer4_dstprt:
            exact;
hdr.ipv4.src_addr:
            exact;
hdr.ipv4.dst_addr:
            exact;
ig_md.vrf:
            exact;
hdr.ipv4.protocol:
            exact;
        }
        actions = {
#ifdef HAVE_GRE
            act_tunnel_gre;
#endif
#ifdef HAVE_TMUX
            act_tunnel_tmux;
#endif
#ifdef HAVE_IPIP
            act_tunnel_ipip;
#endif
#ifdef HAVE_L2TP
            act_tunnel_l2tp;
#endif
#ifdef HAVE_L3TP
            act_tunnel_l3tp;
#endif
#ifdef HAVE_VXLAN
            act_tunnel_vxlan;
#endif
#ifdef HAVE_ETHERIP
            act_tunnel_etherip;
#endif
#ifdef HAVE_PCKOUDP
            act_tunnel_pckoudp;
#endif
#ifdef HAVE_GTP
            act_tunnel_gtp;
#endif
            @defaultonly NoAction;
        }
        size = IPV4_TUNNEL_TABLE_SIZE;
        const default_action = NoAction();
    }




    table tbl_tunnel6 {
        key = {
ig_md.layer4_srcprt:
            exact;
ig_md.layer4_dstprt:
            exact;
hdr.ipv6.src_addr:
            exact;
hdr.ipv6.dst_addr:
            exact;
ig_md.vrf:
            exact;
hdr.ipv6.next_hdr:
            exact;
        }
        actions = {
#ifdef HAVE_GRE
            act_tunnel_gre;
#endif
#ifdef HAVE_TMUX
            act_tunnel_tmux;
#endif
#ifdef HAVE_IPIP
            act_tunnel_ipip;
#endif
#ifdef HAVE_L2TP
            act_tunnel_l2tp;
#endif
#ifdef HAVE_L3TP
            act_tunnel_l3tp;
#endif
#ifdef HAVE_VXLAN
            act_tunnel_vxlan;
#endif
#ifdef HAVE_ETHERIP
            act_tunnel_etherip;
#endif
#ifdef HAVE_PCKOUDP
            act_tunnel_pckoudp;
#endif
#ifdef HAVE_GTP
            act_tunnel_gtp;
#endif
            @defaultonly NoAction;
        }
        size = IPV6_TUNNEL_TABLE_SIZE;
        const default_action = NoAction();
    }


    apply {
#ifdef HAVE_GTP
        gtp_hit = 0;
#endif
#ifdef HAVE_L2TP
        l2tp_hit = 0;
#endif
#ifdef HAVE_L3TP
        l3tp_hit = 0;
#endif
#ifdef HAVE_TMUX
        tmux_hit = 0;
#endif
        if (ig_md.ipv4_valid==1)  {
            tbl_tunnel4.apply();
        } else if (ig_md.ipv6_valid==1)  {
            tbl_tunnel6.apply();
        }
#ifdef HAVE_GTP
        if (gtp_hit != 0) {
            if (ig_md.gtp_type == 4) hdr.ethernet.ethertype = ETHERTYPE_IPV4;
            if (ig_md.gtp_type == 6) hdr.ethernet.ethertype = ETHERTYPE_IPV6;
            ig_md.ipv4_valid = 0;
            ig_md.ipv6_valid = 0;
            hdr.vlan.setInvalid();
            hdr.vlanq.setInvalid();
            ig_tm_md.ucast_egress_port = RECIR_PORT;
            ig_tm_md.bypass_egress = 1;
//        recirculate(RECIR_PORT);
            hdr.cpu.setValid();
            hdr.cpu._padding1 = 0;
            hdr.cpu._padding2 = 0;
            hdr.cpu.port = gtp_hit;
            hdr.gtp.setInvalid();
            hdr.udp.setInvalid();
            hdr.ipv4.setInvalid();
            hdr.ipv6.setInvalid();
        }
#endif
#ifdef HAVE_L2TP
        if ((l2tp_hit != 0) && ((hdr.l2tp.flags & 0x8000)==0) && ((hdr.l2tp.ppptyp & 0x8000)==0)) {
            if (hdr.l2tp.ppptyp == PPPTYPE_IPV4) hdr.ethernet.ethertype = ETHERTYPE_IPV4;
            else if (hdr.l2tp.ppptyp == PPPTYPE_IPV6) hdr.ethernet.ethertype = ETHERTYPE_IPV6;
#ifdef HAVE_SGT
            else if (hdr.l2tp.ppptyp == PPPTYPE_SGT) hdr.ethernet.ethertype = ETHERTYPE_SGT;
#endif
#ifdef HAVE_NSH
            else if (hdr.l2tp.ppptyp == PPPTYPE_NSH) hdr.ethernet.ethertype = ETHERTYPE_NSH;
#endif
#ifdef HAVE_MPLS
            else if (hdr.l2tp.ppptyp == PPPTYPE_MPLS_UCAST) hdr.ethernet.ethertype = ETHERTYPE_MPLS_UCAST;
#endif
#ifdef HAVE_TAP
            else if (hdr.l2tp.ppptyp == PPPTYPE_ROUTEDMAC) hdr.ethernet.ethertype = ETHERTYPE_ROUTEDMAC;
#endif
            ig_md.ipv4_valid = 0;
            ig_md.ipv6_valid = 0;
            hdr.vlan.setInvalid();
            hdr.vlanq.setInvalid();
            ig_tm_md.ucast_egress_port = RECIR_PORT;
            ig_tm_md.bypass_egress = 1;
//        recirculate(RECIR_PORT);
            hdr.cpu.setValid();
            hdr.cpu._padding1 = 0;
            hdr.cpu._padding2 = 0;
            hdr.cpu.port = l2tp_hit;
            hdr.l2tp.setInvalid();
            hdr.udp.setInvalid();
            hdr.ipv4.setInvalid();
            hdr.ipv6.setInvalid();
            ig_md.source_id = l2tp_hit;
        }
#endif

#ifdef HAVE_L3TP
        if ((l3tp_hit != 0) && (hdr.l3tp.tidsid != 0) && ((hdr.l3tp.ppptyp & 0x8000)==0)) {
            if (hdr.l3tp.ppptyp == PPPTYPE_IPV4) hdr.ethernet.ethertype = ETHERTYPE_IPV4;
            else if (hdr.l3tp.ppptyp == PPPTYPE_IPV6) hdr.ethernet.ethertype = ETHERTYPE_IPV6;
#ifdef HAVE_SGT
            else if (hdr.l3tp.ppptyp == PPPTYPE_SGT) hdr.ethernet.ethertype = ETHERTYPE_SGT;
#endif
#ifdef HAVE_NSH
            else if (hdr.l3tp.ppptyp == PPPTYPE_NSH) hdr.ethernet.ethertype = ETHERTYPE_NSH;
#endif
#ifdef HAVE_MPLS
            else if (hdr.l3tp.ppptyp == PPPTYPE_MPLS_UCAST) hdr.ethernet.ethertype = ETHERTYPE_MPLS_UCAST;
#endif
#ifdef HAVE_TAP
            else if (hdr.l3tp.ppptyp == PPPTYPE_ROUTEDMAC) hdr.ethernet.ethertype = ETHERTYPE_ROUTEDMAC;
#endif
            ig_md.ipv4_valid = 0;
            ig_md.ipv6_valid = 0;
            hdr.vlan.setInvalid();
            hdr.vlanq.setInvalid();
            ig_tm_md.ucast_egress_port = RECIR_PORT;
            ig_tm_md.bypass_egress = 1;
//        recirculate(RECIR_PORT);
            hdr.cpu.setValid();
            hdr.cpu._padding1 = 0;
            hdr.cpu._padding2 = 0;
            hdr.cpu.port = l3tp_hit;
            hdr.l3tp.setInvalid();
            hdr.ipv4.setInvalid();
            hdr.ipv6.setInvalid();
            ig_md.source_id = l3tp_hit;
        }
#endif


#ifdef HAVE_TMUX
        if (tmux_hit != 0) {
            if (hdr.tmux.proto == IP_PROTOCOL_IPV4) hdr.ethernet.ethertype = ETHERTYPE_IPV4;
            else if (hdr.tmux.proto == IP_PROTOCOL_IPV6) hdr.ethernet.ethertype = ETHERTYPE_IPV6;
#ifdef HAVE_SGT
            else if (hdr.tmux.proto == IP_PROTOCOL_SKIP) hdr.ethernet.ethertype = ETHERTYPE_SGT;
#endif
#ifdef HAVE_NSH
            else if (hdr.tmux.proto == IP_PROTOCOL_NSH) hdr.ethernet.ethertype = ETHERTYPE_NSH;
#endif
#ifdef HAVE_MPLS
            else if (hdr.tmux.proto == IP_PROTOCOL_MPLS_IN_IP) hdr.ethernet.ethertype = ETHERTYPE_MPLS_UCAST;
#endif
#ifdef HAVE_TAP
            else if (hdr.tmux.proto == IP_PROTOCOL_SRL2) hdr.ethernet.ethertype = ETHERTYPE_ROUTEDMAC;
#endif
            ig_md.ipv4_valid = 0;
            ig_md.ipv6_valid = 0;
            hdr.vlan.setInvalid();
            hdr.vlanq.setInvalid();
            ig_tm_md.ucast_egress_port = RECIR_PORT;
            ig_tm_md.bypass_egress = 1;
//        recirculate(RECIR_PORT);
            hdr.cpu.setValid();
            hdr.cpu._padding1 = 0;
            hdr.cpu._padding2 = 0;
            hdr.cpu.port = tmux_hit;
            hdr.tmux.setInvalid();
            hdr.ipv4.setInvalid();
            hdr.ipv6.setInvalid();
            ig_md.source_id = tmux_hit;
        }
#endif


    }


}

#endif

#endif // _IG_CTL_TUNNEL_P4_
