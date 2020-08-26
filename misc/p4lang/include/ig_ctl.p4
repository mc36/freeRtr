#ifndef _INGRESS_CONTROL_P4_
#define _INGRESS_CONTROL_P4_


/*------------------ I N G R E S S - M A T C H - A C T I O N ---------------- */

control ig_ctl(inout headers hdr,
               inout ingress_metadata_t ig_md,
               inout standard_metadata_t ig_intr_md) {

    IngressControlARP() ig_ctl_arp;
    IngressControlPPPOE() ig_ctl_pppoe;
    IngressControlMPLS() ig_ctl_mpls;
    IngressControlMPLS2() ig_ctl_mpls2;
    IngressControlBridge() ig_ctl_bridge;
    IngressControlIPv4() ig_ctl_ipv4;
    IngressControlIPv6() ig_ctl_ipv6;
    IngressControlIPv4b() ig_ctl_ipv4b;
    IngressControlIPv6b() ig_ctl_ipv6b;
    IngressControlNexthop() ig_ctl_nexthop;
    IngressControlVlanIn() ig_ctl_vlan_in;
    IngressControlVlanOut() ig_ctl_vlan_out;
    IngressControlBundle() ig_ctl_bundle;
    IngressControlVRF() ig_ctl_vrf;
    IngressControlLLC() ig_ctl_llc;
    IngressControlCoPP() ig_ctl_copp;
    IngressControlTunnel() ig_ctl_tunnel;
    IngressControlAclIn() ig_ctl_acl_in;
    IngressControlAclOut() ig_ctl_acl_out;
    IngressControlNAT() ig_ctl_nat;


    apply {
        if (ig_intr_md.ingress_port == CPU_PORT) {
            /*
             * pkt received from the controlled has a cpu header
             * that containes egress port id. Once retrieve
             * we remove the cpu header (setInvalid)
             * So it will not be taken into accoiunt by deparser
             */
            ig_intr_md.egress_spec = (PortId_t)hdr.cpu.port;
            hdr.cpu.setInvalid();
            return;
        }
        /*
         * So it is a dataplane packet
         */
        hdr.cpu.setInvalid();
        ig_ctl_vlan_in.apply(hdr,ig_md,ig_intr_md);
        ig_ctl_pppoe.apply(hdr,ig_md,ig_intr_md);
        ig_ctl_acl_in.apply(hdr,ig_md,ig_intr_md);
        if (ig_md.dropping == 1) {
            return;
        }
        ig_ctl_vrf.apply(hdr,ig_md,ig_intr_md);
        ig_ctl_arp.apply(hdr,ig_md,ig_intr_md);
        ig_ctl_llc.apply(hdr,ig_md,ig_intr_md);
        ig_ctl_mpls.apply(hdr,ig_md,ig_intr_md);
        ig_ctl_nat.apply(hdr,ig_md,ig_intr_md);
        if ( ig_md.dropping == 1) {
            hdr.cpu.setValid();
            hdr.cpu.port = ig_md.ingress_id;
            ig_intr_md.egress_spec = CPU_PORT;
            return;
        }
        ig_ctl_ipv4.apply(hdr,ig_md,ig_intr_md);
        ig_ctl_ipv6.apply(hdr,ig_md,ig_intr_md);
        ig_ctl_bridge.apply(hdr,ig_md,ig_intr_md);
        ig_ctl_ipv4b.apply(hdr,ig_md,ig_intr_md);
        ig_ctl_ipv6b.apply(hdr,ig_md,ig_intr_md);
        if ( ig_md.nexthop_id == CPU_PORT) {
            ig_ctl_tunnel.apply(hdr,ig_md,ig_intr_md);
            if (ig_md.need_recir == 1) {
                if (hdr.vlan.isValid()) {
                    hdr.vlan.setInvalid();
                }
                if (hdr.pppoeD.isValid()) {
                    hdr.pppoeD.setInvalid();
                }
                return;
            }
            ig_ctl_copp.apply(hdr,ig_md,ig_intr_md);
            if (ig_md.dropping == 1) {
                return;
            }
            hdr.cpu.setValid();
            hdr.cpu.port = ig_md.ingress_id;
            ig_intr_md.egress_spec = CPU_PORT;
            return;
        }

        if (hdr.vlan.isValid()) {
            hdr.vlan.setInvalid();
        }

        if (hdr.pppoeD.isValid()) {
            hdr.pppoeD.setInvalid();
        }

        if (ig_md.srv_op_type != 0) {
            hdr.ipv6.setInvalid();
        }
        if (ig_md.srv_op_type == 2) {
            hdr.eth3.setInvalid();
        }

        ig_ctl_mpls2.apply(hdr,ig_md,ig_intr_md);
        ig_ctl_nexthop.apply(hdr,ig_md,ig_intr_md);
        ig_ctl_acl_out.apply(hdr,ig_md,ig_intr_md);
        if (ig_md.dropping == 1) {
            return;
        }
        ig_ctl_vlan_out.apply(hdr,ig_md,ig_intr_md);
        ig_ctl_bundle.apply(hdr,ig_md,ig_intr_md);
    }
}

#endif // _INGRESS_CONTROL_P4_
