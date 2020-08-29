#ifndef _NEXTHOP_P4_
#define _NEXTHOP_P4_

control IngressControlNexthop(inout headers hdr,
                              inout ingress_metadata_t ig_md,
                              inout standard_metadata_t ig_intr_md) {


    action act_ipv4_fib_hit(mac_addr_t dst_mac_addr, mac_addr_t src_mac_addr, SubIntId_t egress_port) {
        /*
         * the packet header src_mac is now set to the previous header dst_mac
         */
        hdr.ethernet.src_mac_addr = src_mac_addr;

        /*
         * the new packet header dst_mac is now the dst_mac
         * set by the control plane entry
         */
        hdr.ethernet.dst_mac_addr = dst_mac_addr;

        /*
         * the egress_spec port is set now the egress_port
         * set by the control plane entry
         */
        ig_md.target_id = egress_port;
        ig_md.aclport_id = egress_port;
    }


    action act_ipv4_pppoe(mac_addr_t dst_mac_addr, mac_addr_t src_mac_addr, SubIntId_t egress_port, SubIntId_t acl_port, bit<16> session) {
        /*
         * the packet header src_mac is now set to the previous header dst_mac
         */
        hdr.ethernet.src_mac_addr = src_mac_addr;

        /*
         * the new packet header dst_mac is now the dst_mac
         * set by the control plane entry
         */
        hdr.ethernet.dst_mac_addr = dst_mac_addr;

        /*
         * the egress_spec port is set now the egress_port
         * set by the control plane entry
         */
        ig_md.target_id = egress_port;
        ig_md.aclport_id = acl_port;

        hdr.pppoeD.setValid();
        hdr.pppoeD.ver = 1;
        hdr.pppoeD.type = 1;
        hdr.pppoeD.code = 0;
        hdr.pppoeD.session = session;
        hdr.pppoeD.length = (bit<16>)ig_intr_md.packet_length - ig_md.vlan_size - 12;
        hdr.pppoeD.ppptyp = 0;
        if (ig_md.ethertype == ETHERTYPE_IPV4) hdr.pppoeD.ppptyp = PPPTYPE_IPV4;
        if (ig_md.ethertype == ETHERTYPE_IPV6) hdr.pppoeD.ppptyp = PPPTYPE_IPV6;
        if (ig_md.ethertype == ETHERTYPE_MPLS_UCAST) hdr.pppoeD.ppptyp = PPPTYPE_MPLS_UCAST;
        if (ig_md.ethertype == ETHERTYPE_ROUTEDMAC) hdr.pppoeD.ppptyp = PPPTYPE_ROUTEDMAC;
        ig_md.ethertype = ETHERTYPE_PPPOE_DATA;
    }

    action act_ipv4_gre4(mac_addr_t dst_mac_addr, mac_addr_t src_mac_addr, SubIntId_t egress_port, SubIntId_t acl_port, ipv4_addr_t dst_ip_addr, ipv4_addr_t src_ip_addr) {
        /*
         * the packet header src_mac is now set to the previous header dst_mac
         */
        hdr.ethernet.src_mac_addr = src_mac_addr;

        /*
         * the new packet header dst_mac is now the dst_mac
         * set by the control plane entry
         */
        hdr.ethernet.dst_mac_addr = dst_mac_addr;

        /*
         * the egress_spec port is set now the egress_port
         * set by the control plane entry
         */
        ig_md.target_id = egress_port;
        ig_md.aclport_id = acl_port;

        hdr.gre2.setValid();
        hdr.gre2.flags = 0;
        hdr.gre2.gretyp = ig_md.ethertype;

        hdr.ipv4d.setValid();
        hdr.ipv4d.version = 4;
        hdr.ipv4d.ihl = 5;
        hdr.ipv4d.diffserv = 0;
        hdr.ipv4d.total_len = (bit<16>)ig_intr_md.packet_length - ig_md.vlan_size - 14 + 24;
        hdr.ipv4d.identification = 0;
        hdr.ipv4d.flags = 0;
        hdr.ipv4d.frag_offset = 0;
        hdr.ipv4d.ttl = 255;
        hdr.ipv4d.protocol = IP_PROTOCOL_GRE;
        hdr.ipv4d.hdr_checksum = 0;
        hdr.ipv4d.src_addr = src_ip_addr;
        hdr.ipv4d.dst_addr = dst_ip_addr;
        ig_md.ethertype = ETHERTYPE_IPV4;
    }

    action act_ipv4_gre6(mac_addr_t dst_mac_addr, mac_addr_t src_mac_addr, SubIntId_t egress_port, SubIntId_t acl_port, ipv6_addr_t dst_ip_addr, ipv6_addr_t src_ip_addr) {
        /*
         * the packet header src_mac is now set to the previous header dst_mac
         */
        hdr.ethernet.src_mac_addr = src_mac_addr;

        /*
         * the new packet header dst_mac is now the dst_mac
         * set by the control plane entry
         */
        hdr.ethernet.dst_mac_addr = dst_mac_addr;

        /*
         * the egress_spec port is set now the egress_port
         * set by the control plane entry
         */
        ig_md.target_id = egress_port;
        ig_md.aclport_id = acl_port;

        hdr.gre2.setValid();
        hdr.gre2.flags = 0;
        hdr.gre2.gretyp = ig_md.ethertype;

        hdr.ipv6d.setValid();
        hdr.ipv6d.version = 6;
        hdr.ipv6d.traffic_class = 0;
        hdr.ipv6d.flow_label = 0;
        hdr.ipv6d.payload_len = (bit<16>)ig_intr_md.packet_length - ig_md.vlan_size - 14 + 4;
        hdr.ipv6d.next_hdr = IP_PROTOCOL_GRE;
        hdr.ipv6d.hop_limit = 255;
        hdr.ipv6d.src_addr = src_ip_addr;
        hdr.ipv6d.dst_addr = dst_ip_addr;
        ig_md.ethertype = ETHERTYPE_IPV6;
    }


    action act_ipv4_l2tp4(mac_addr_t dst_mac_addr, mac_addr_t src_mac_addr, SubIntId_t egress_port, SubIntId_t acl_port, ipv4_addr_t dst_ip_addr, ipv4_addr_t src_ip_addr, bit<16> src_port, bit<16> dst_port, bit<32> tunnel_id) {
        /*
         * the packet header src_mac is now set to the previous header dst_mac
         */
        hdr.ethernet.src_mac_addr = src_mac_addr;

        /*
         * the new packet header dst_mac is now the dst_mac
         * set by the control plane entry
         */
        hdr.ethernet.dst_mac_addr = dst_mac_addr;

        /*
         * the egress_spec port is set now the egress_port
         * set by the control plane entry
         */
        ig_md.target_id = egress_port;
        ig_md.aclport_id = acl_port;

        hdr.l2tp2.setValid();
        hdr.l2tp2.flags = 0x202;
        hdr.l2tp2.tidsid = tunnel_id;
        hdr.l2tp2.offset = 0;
        hdr.l2tp2.pppflags = 0xff03;
        hdr.l2tp2.ppptyp = 0;
        if (ig_md.ethertype == ETHERTYPE_IPV4) hdr.l2tp2.ppptyp = PPPTYPE_IPV4;
        if (ig_md.ethertype == ETHERTYPE_IPV6) hdr.l2tp2.ppptyp = PPPTYPE_IPV6;
        if (ig_md.ethertype == ETHERTYPE_MPLS_UCAST) hdr.l2tp2.ppptyp = PPPTYPE_MPLS_UCAST;
        if (ig_md.ethertype == ETHERTYPE_ROUTEDMAC) hdr.l2tp2.ppptyp = PPPTYPE_ROUTEDMAC;

        hdr.udp2.setValid();
        hdr.udp2.src_port = src_port;
        hdr.udp2.dst_port = dst_port;
        hdr.udp2.length = (bit<16>)ig_intr_md.packet_length - ig_md.vlan_size - 14 + 20;
        hdr.udp2.checksum = 0;

        hdr.ipv4d.setValid();
        hdr.ipv4d.version = 4;
        hdr.ipv4d.ihl = 5;
        hdr.ipv4d.diffserv = 0;
        hdr.ipv4d.total_len = (bit<16>)ig_intr_md.packet_length - ig_md.vlan_size - 14 + 40;
        hdr.ipv4d.identification = 0;
        hdr.ipv4d.flags = 0;
        hdr.ipv4d.frag_offset = 0;
        hdr.ipv4d.ttl = 255;
        hdr.ipv4d.protocol = IP_PROTOCOL_UDP;
        hdr.ipv4d.hdr_checksum = 0;
        hdr.ipv4d.src_addr = src_ip_addr;
        hdr.ipv4d.dst_addr = dst_ip_addr;
        ig_md.ethertype = ETHERTYPE_IPV4;
    }


    action act_ipv4_l2tp6(mac_addr_t dst_mac_addr, mac_addr_t src_mac_addr, SubIntId_t egress_port, SubIntId_t acl_port, ipv6_addr_t dst_ip_addr, ipv6_addr_t src_ip_addr, bit<16> src_port, bit<16> dst_port, bit<32> tunnel_id) {
        /*
         * the packet header src_mac is now set to the previous header dst_mac
         */
        hdr.ethernet.src_mac_addr = src_mac_addr;

        /*
         * the new packet header dst_mac is now the dst_mac
         * set by the control plane entry
         */
        hdr.ethernet.dst_mac_addr = dst_mac_addr;

        /*
         * the egress_spec port is set now the egress_port
         * set by the control plane entry
         */
        ig_md.target_id = egress_port;
        ig_md.aclport_id = acl_port;

        hdr.l2tp2.setValid();
        hdr.l2tp2.flags = 0x202;
        hdr.l2tp2.tidsid = tunnel_id;
        hdr.l2tp2.offset = 0;
        hdr.l2tp2.pppflags = 0xff03;
        hdr.l2tp2.ppptyp = 0;
        if (ig_md.ethertype == ETHERTYPE_IPV4) hdr.l2tp2.ppptyp = PPPTYPE_IPV4;
        if (ig_md.ethertype == ETHERTYPE_IPV6) hdr.l2tp2.ppptyp = PPPTYPE_IPV6;
        if (ig_md.ethertype == ETHERTYPE_MPLS_UCAST) hdr.l2tp2.ppptyp = PPPTYPE_MPLS_UCAST;
        if (ig_md.ethertype == ETHERTYPE_ROUTEDMAC) hdr.l2tp2.ppptyp = PPPTYPE_ROUTEDMAC;

        hdr.udp2.setValid();
        hdr.udp2.src_port = src_port;
        hdr.udp2.dst_port = dst_port;
        hdr.udp2.length = (bit<16>)ig_intr_md.packet_length - ig_md.vlan_size - 14 + 20;
        hdr.udp2.checksum = 0;

        hdr.ipv6d.setValid();
        hdr.ipv6d.version = 6;
        hdr.ipv6d.traffic_class = 0;
        hdr.ipv6d.flow_label = 0;
        hdr.ipv6d.payload_len = (bit<16>)ig_intr_md.packet_length - ig_md.vlan_size - 14 + 20;
        hdr.ipv6d.next_hdr = IP_PROTOCOL_UDP;
        hdr.ipv6d.hop_limit = 255;
        hdr.ipv6d.src_addr = src_ip_addr;
        hdr.ipv6d.dst_addr = dst_ip_addr;
        ig_md.ethertype = ETHERTYPE_IPV6;
    }


    action act_ipv4_fib_discard() {
        mark_to_drop(ig_intr_md);
    }

    table tbl_nexthop {
        /*
         * custom metadat is used for the lookup key
         */
        key = {
ig_md.nexthop_id:
            exact;
        }
        actions = {
            act_ipv4_fib_hit;
            act_ipv4_pppoe;
            act_ipv4_gre4;
            act_ipv4_gre6;
            act_ipv4_l2tp4;
            act_ipv4_l2tp6;
            act_ipv4_fib_discard;
        }
        size = NEXTHOP_TABLE_SIZE;
        default_action = act_ipv4_fib_discard();
    }

    apply {
        if (ig_md.target_id != 0) {
            return;
        }
        tbl_nexthop.apply();

        /*
         * We decrement the TTL
         */
        if (hdr.ipv4.isValid()) {
            hdr.ipv4.ttl = hdr.ipv4.ttl -1;
        }
        if (hdr.ipv6.isValid()) {
            hdr.ipv6.hop_limit = hdr.ipv6.hop_limit -1;
        }
    }

}

#endif // _NEXTHOP_P4
