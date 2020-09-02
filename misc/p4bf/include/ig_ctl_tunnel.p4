#ifndef _IG_CTL_TUNNEL_P4_
#define _IG_CTL_TUNNEL_P4_

#ifdef HAVE_TUN

control IngressControlTunnel(inout headers hdr, inout ingress_metadata_t ig_md,
                             in ingress_intrinsic_metadata_t ig_intr_md,
                             inout ingress_intrinsic_metadata_for_deparser_t ig_dprsr_md,
                             inout ingress_intrinsic_metadata_for_tm_t ig_tm_md)
{

    SubIntId_t hit;

#ifdef HAVE_GRE
    action act_tunnel_gre(SubIntId_t port) {
        hdr.ethernet.ethertype = hdr.gre.gretyp;
        ig_md.ipv4_valid = 0;
        ig_md.ipv6_valid = 0;
        hdr.vlan.setInvalid();
        ig_tm_md.ucast_egress_port = RECIR_PORT;
        ig_tm_md.bypass_egress = 1;
//        recirculate(RECIR_PORT);
        hdr.cpu.setValid();
        hdr.cpu.port = port;
        hdr.gre.setInvalid();
        hdr.ipv4.setInvalid();
        hdr.ipv6.setInvalid();
    }
#endif

#ifdef HAVE_L2TP
    action act_tunnel_l2tp(SubIntId_t port) {
        hit = port;
    }
#endif

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
#ifdef HAVE_GRE
            act_tunnel_gre;
#endif
#ifdef HAVE_L2TP
            act_tunnel_l2tp;
#endif
            @defaultonly NoAction;
        }
        size = 1024;
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
#ifdef HAVE_GRE
            act_tunnel_gre;
#endif
#ifdef HAVE_L2TP
            act_tunnel_l2tp;
#endif
            @defaultonly NoAction;
        }
        size = 1024;
        const default_action = NoAction();
    }


    apply {
#ifdef HAVE_L2TP
        hit = 0;
#endif
        if (hdr.ipv4.isValid()) {
            tbl_tunnel4.apply();
        }
        else        if (hdr.ipv6.isValid()) {
            tbl_tunnel6.apply();
        }
#ifdef HAVE_L2TP
        if ((hit != 0) && ((hdr.l2tp.flags & 0x8000)==0) && ((hdr.l2tp.ppptyp & 0x8000)==0)) {
            if (hdr.l2tp.ppptyp == PPPTYPE_IPV4) hdr.ethernet.ethertype = ETHERTYPE_IPV4;
            else        if (hdr.l2tp.ppptyp == PPPTYPE_IPV6) hdr.ethernet.ethertype = ETHERTYPE_IPV6;
            else        if (hdr.l2tp.ppptyp == PPPTYPE_MPLS_UCAST) hdr.ethernet.ethertype = ETHERTYPE_MPLS_UCAST;
            else        if (hdr.l2tp.ppptyp == PPPTYPE_ROUTEDMAC) hdr.ethernet.ethertype = ETHERTYPE_ROUTEDMAC;
            ig_md.ipv4_valid = 0;
            ig_md.ipv6_valid = 0;
            hdr.vlan.setInvalid();
            ig_tm_md.ucast_egress_port = RECIR_PORT;
            ig_tm_md.bypass_egress = 1;
//        recirculate(RECIR_PORT);
            hdr.cpu.setValid();
            hdr.cpu.port = hit;
            hdr.l2tp.setInvalid();
            hdr.udp.setInvalid();
            hdr.ipv4.setInvalid();
            hdr.ipv6.setInvalid();
        }
#endif

    }


}

#endif

#endif // _IG_CTL_TUNNEL_P4_
