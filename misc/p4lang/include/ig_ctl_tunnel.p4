#ifndef _IG_CTL_TUNNEL_P4_
#define _IG_CTL_TUNNEL_P4_


control IngressControlTunnel(inout headers hdr,
                             inout ingress_metadata_t ig_md,
                             inout standard_metadata_t ig_intr_md) {



    action act_tunnel4_gre(SubIntId_t port) {
        hdr.ethernet.ethertype = hdr.gre.gretyp;
        hdr.gre.setInvalid();
        hdr.ipv4.setInvalid();
        hdr.cpu.setValid();
        hdr.cpu.port = port;
        ig_intr_md.egress_spec = (PortId_t)port;
        ig_md.need_recir = 1;
    }


    action act_tunnel6_gre(SubIntId_t port) {
        hdr.ethernet.ethertype = hdr.gre.gretyp;
        hdr.gre.setInvalid();
        hdr.ipv6.setInvalid();
        hdr.cpu.setValid();
        hdr.cpu.port = port;
        ig_intr_md.egress_spec = (PortId_t)port;
        ig_md.need_recir = 1;
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
            act_tunnel4_gre;
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
            act_tunnel6_gre;
            @defaultonly NoAction;
        }
        size = 1024;
        const default_action = NoAction();
    }


    apply {

        if (hdr.ipv4.isValid()) {
            tbl_tunnel4.apply();
        }
        if (hdr.ipv6.isValid()) {
            tbl_tunnel6.apply();
        }

    }


}


#endif // _IG_CTL_TUNNEL_P4_
