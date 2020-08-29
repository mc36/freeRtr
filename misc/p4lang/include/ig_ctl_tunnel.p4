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


    action act_tunnel_l2tp(SubIntId_t port) {
        if (hdr.l2tp.ppptyp == PPPTYPE_IPV4) hdr.ethernet.ethertype = ETHERTYPE_IPV4;
        if (hdr.l2tp.ppptyp == PPPTYPE_IPV6) hdr.ethernet.ethertype = ETHERTYPE_IPV6;
        if (hdr.l2tp.ppptyp == PPPTYPE_MPLS_UCAST) hdr.ethernet.ethertype = ETHERTYPE_MPLS_UCAST;
        if (hdr.l2tp.ppptyp == PPPTYPE_ROUTEDMAC) hdr.ethernet.ethertype = ETHERTYPE_ROUTEDMAC;
        ig_intr_md.egress_spec = (PortId_t)port;
        ig_md.need_recir = 1;
        ig_md.source_id = port;
        if ((hdr.l2tp.flags & 0x8000) != 0) ig_md.need_recir = 0;
        if ((hdr.l2tp.ppptyp & 0x8000) != 0) ig_md.need_recir = 0;
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
            act_tunnel_l2tp;
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
            act_tunnel_gre;
            act_tunnel_l2tp;
            @defaultonly NoAction;
        }
        size = 1024;
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

        hdr.l2tp.setInvalid();
        hdr.udp.setInvalid();
        hdr.gre.setInvalid();
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
