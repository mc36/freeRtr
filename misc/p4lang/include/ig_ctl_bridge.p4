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

#ifndef _IG_CTL_BRIDGE_P4_
#define _IG_CTL_BRIDGE_P4_

control IngressControlBridge(inout headers hdr,
                             inout ingress_metadata_t ig_md,
                             inout standard_metadata_t ig_intr_md) {

    direct_counter(CounterType.packets_and_bytes) statsRx;

    direct_counter(CounterType.packets_and_bytes) statsTx;

    action send_to_cpu() {
        ig_md.nexthop_id = CPU_PORT;
        // Packets sent to the controller needs to be prepended with the
        // packet-in header. By setting it valid we make sure it will be
        // deparsed on the wire (see c_deparser).
    }


    action act_set_bridge_port() {
        ig_md.bridge_src = 1;
    }

    action act_bridge_miss() {
        ig_md.bridge_src = 0;
    }

    table tbl_bridge_learn {
        key = {
ig_md.bridge_id:
            exact;
hdr.ethernet.src_mac_addr:
            exact;
        }
        actions = {
            act_set_bridge_port;
            act_bridge_miss;
        }
        size = MAC_TABLE_SIZE;
        default_action = act_bridge_miss();
        counters = statsRx;
    }


    action act_set_bridge_out(SubIntId_t port) {
        ig_md.bridge_trg = port;
        ig_md.target_id = port;
    }

    action act_set_bridge_ppprouted(NextHopId_t nexthop) {
        ig_md.bridge_trg = MAX_PORT;
        ig_md.vrf = 0;
        ig_md.nsh_valid = 0;
        ig_md.polka_valid = 0;
        ig_md.mpls0_valid = 0;
        ig_md.mpls1_valid = 0;
        ig_md.arp_valid = 0;
        ig_md.llc_valid = 0;
        ig_md.ipv4_valid = 0;
        ig_md.ipv6_valid = 0;
        hdr.pppbr.setValid();
        hdr.pppbr.flags = 1;
        hdr.eth4.setValid();
        hdr.eth4.dst_mac_addr = hdr.ethernet.dst_mac_addr;
        hdr.eth4.src_mac_addr = hdr.ethernet.src_mac_addr;
        hdr.eth4.ethertype = ig_md.ethertype;
        ig_md.nexthop_id = nexthop;
        ig_md.ethertype = ETHERTYPE_ROUTEDMAC;
        ig_md.vlan_size = ig_md.vlan_size - 16;
    }

    action act_set_bridge_routed(NextHopId_t nexthop) {
        ig_md.bridge_trg = MAX_PORT;
        ig_md.vrf = 0;
        ig_md.nsh_valid = 0;
        ig_md.polka_valid = 0;
        ig_md.mpls0_valid = 0;
        ig_md.mpls1_valid = 0;
        ig_md.arp_valid = 0;
        ig_md.llc_valid = 0;
        ig_md.ipv4_valid = 0;
        ig_md.ipv6_valid = 0;
        hdr.eth4.setValid();
        hdr.eth4.dst_mac_addr = hdr.ethernet.dst_mac_addr;
        hdr.eth4.src_mac_addr = hdr.ethernet.src_mac_addr;
        hdr.eth4.ethertype = ig_md.ethertype;
        ig_md.nexthop_id = nexthop;
        ig_md.ethertype = ETHERTYPE_ROUTEDMAC;
        ig_md.vlan_size = ig_md.vlan_size - 14;
    }

    action act_set_bridge_vpls(NextHopId_t port, label_t lab_tun, label_t lab_svc) {
        ig_md.bridge_trg = MAX_PORT;
        ig_md.vrf = 0;
        ig_md.nsh_valid = 0;
        ig_md.polka_valid = 0;
        ig_md.mpls0_valid = 0;
        ig_md.mpls1_valid = 0;
        ig_md.nsh_remove = 0;
        ig_md.polka_valid = 0;
        ig_md.mpls0_remove = 0;
        ig_md.mpls1_remove = 0;
        ig_md.arp_valid = 0;
        ig_md.llc_valid = 0;
        ig_md.ipv4_valid = 0;
        ig_md.ipv6_valid = 0;
        hdr.eth2.setValid();
        hdr.eth2.dst_mac_addr = hdr.ethernet.dst_mac_addr;
        hdr.eth2.src_mac_addr = hdr.ethernet.src_mac_addr;
        hdr.eth2.ethertype = ig_md.ethertype;
        ig_md.nexthop_id = port;
        ig_md.ethertype = ETHERTYPE_MPLS_UCAST;
        hdr.mpls0.setValid();
        hdr.mpls0.label = lab_tun;
        hdr.mpls0.ttl = 255;
        hdr.mpls1.setValid();
        hdr.mpls1.label = lab_svc;
        hdr.mpls1.ttl = 255;
        hdr.mpls1.bos = 1;
        ig_md.mpls_op_type = 3;
    }

    action act_set_bridge_srv(NextHopId_t port, ipv6_addr_t target) {
        ig_md.bridge_trg = MAX_PORT;
        ig_md.vrf = 0;
        ig_md.nsh_valid = 0;
        ig_md.polka_valid = 0;
        ig_md.mpls0_valid = 0;
        ig_md.mpls1_valid = 0;
        ig_md.arp_valid = 0;
        ig_md.llc_valid = 0;
        ig_md.ipv4_valid = 0;
        ig_md.ipv6_valid = 0;
        hdr.vlan.setInvalid();
        hdr.eth2.setValid();
        hdr.eth2 = hdr.ethernet;
        hdr.eth2.ethertype = ig_md.ethertype;
        ig_md.nexthop_id = port;
        ig_md.ethertype = ETHERTYPE_IPV6;
        hdr.ipv6c.setValid();
        hdr.ipv6c.version = 6;
        hdr.ipv6c.payload_len = (bit<16>)ig_intr_md.packet_length - ig_md.vlan_size;
        hdr.ipv6c.next_hdr = IP_PROTOCOL_SRL2;
        hdr.ipv6c.hop_limit = 255;
        hdr.ipv6c.src_addr = target;
        hdr.ipv6c.dst_addr = target;
    }




    action act_set_bridge_vxlan4(NextHopId_t nexthop, ipv4_addr_t dst_ip_addr, ipv4_addr_t src_ip_addr, bit<24> instance) {
        ig_md.bridge_trg = MAX_PORT;
        ig_md.vrf = 0;
        ig_md.nsh_valid = 0;
        ig_md.polka_valid = 0;
        ig_md.mpls0_valid = 0;
        ig_md.mpls1_valid = 0;
        ig_md.arp_valid = 0;
        ig_md.llc_valid = 0;
        ig_md.ipv4_valid = 0;
        ig_md.ipv6_valid = 0;
        hdr.vlan.setInvalid();
        hdr.eth2.setValid();
        hdr.eth2 = hdr.ethernet;
        hdr.eth2.ethertype = ig_md.ethertype;
        ig_md.nexthop_id = nexthop;

        hdr.vxlan2.setValid();
        hdr.vxlan2.flags = 0x800;
        hdr.vxlan2.policy = 0;
        hdr.vxlan2.instance = instance;
        hdr.vxlan2.reserved = 0;

        hdr.udp2.setValid();
        hdr.udp2.src_port = 4789;
        hdr.udp2.dst_port = 4789;
        hdr.udp2.length = (bit<16>)ig_intr_md.packet_length - ig_md.vlan_size - 14 + 30;
        hdr.udp2.checksum = 0;

        hdr.ipv4d.setValid();
        hdr.ipv4d.version = 4;
        hdr.ipv4d.ihl = 5;
        hdr.ipv4d.diffserv = 0;
        hdr.ipv4d.total_len = (bit<16>)ig_intr_md.packet_length - ig_md.vlan_size - 14 + 50;
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




    action act_set_bridge_vxlan6(NextHopId_t nexthop, ipv6_addr_t dst_ip_addr, ipv6_addr_t src_ip_addr, bit<24> instance) {
        ig_md.bridge_trg = MAX_PORT;
        ig_md.vrf = 0;
        ig_md.nsh_valid = 0;
        ig_md.polka_valid = 0;
        ig_md.mpls0_valid = 0;
        ig_md.mpls1_valid = 0;
        ig_md.arp_valid = 0;
        ig_md.llc_valid = 0;
        ig_md.ipv4_valid = 0;
        ig_md.ipv6_valid = 0;
        hdr.vlan.setInvalid();
        hdr.eth2.setValid();
        hdr.eth2 = hdr.ethernet;
        hdr.eth2.ethertype = ig_md.ethertype;
        ig_md.nexthop_id = nexthop;

        hdr.vxlan2.setValid();
        hdr.vxlan2.flags = 0x800;
        hdr.vxlan2.policy = 0;
        hdr.vxlan2.instance = instance;
        hdr.vxlan2.reserved = 0;

        hdr.udp2.setValid();
        hdr.udp2.src_port = 4789;
        hdr.udp2.dst_port = 4789;
        hdr.udp2.length = (bit<16>)ig_intr_md.packet_length - ig_md.vlan_size - 14 + 30;
        hdr.udp2.checksum = 0;

        hdr.ipv6d.setValid();
        hdr.ipv6d.version = 6;
        hdr.ipv6d.traffic_class = 0;
        hdr.ipv6d.flow_label = 0;
        hdr.ipv6d.payload_len = (bit<16>)ig_intr_md.packet_length - ig_md.vlan_size - 14 + 30;
        hdr.ipv6d.next_hdr = IP_PROTOCOL_UDP;
        hdr.ipv6d.hop_limit = 255;
        hdr.ipv6d.src_addr = src_ip_addr;
        hdr.ipv6d.dst_addr = dst_ip_addr;
        ig_md.ethertype = ETHERTYPE_IPV6;
    }





    action act_set_bridge_pckoudp4(NextHopId_t nexthop, ipv4_addr_t dst_ip_addr, ipv4_addr_t src_ip_addr, bit<16> src_port, bit<16> dst_port) {
        ig_md.bridge_trg = MAX_PORT;
        ig_md.vrf = 0;
        ig_md.nsh_valid = 0;
        ig_md.polka_valid = 0;
        ig_md.mpls0_valid = 0;
        ig_md.mpls1_valid = 0;
        ig_md.arp_valid = 0;
        ig_md.llc_valid = 0;
        ig_md.ipv4_valid = 0;
        ig_md.ipv6_valid = 0;
        hdr.vlan.setInvalid();
        hdr.eth2.setValid();
        hdr.eth2 = hdr.ethernet;
        hdr.eth2.ethertype = ig_md.ethertype;
        ig_md.nexthop_id = nexthop;

        hdr.udp2.setValid();
        hdr.udp2.src_port = src_port;
        hdr.udp2.dst_port = dst_port;
        hdr.udp2.length = (bit<16>)ig_intr_md.packet_length - ig_md.vlan_size - 14 + 22;
        hdr.udp2.checksum = 0;

        hdr.ipv4d.setValid();
        hdr.ipv4d.version = 4;
        hdr.ipv4d.ihl = 5;
        hdr.ipv4d.diffserv = 0;
        hdr.ipv4d.total_len = (bit<16>)ig_intr_md.packet_length - ig_md.vlan_size - 14 + 42;
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




    action act_set_bridge_pckoudp6(NextHopId_t nexthop, ipv6_addr_t dst_ip_addr, ipv6_addr_t src_ip_addr, bit<16> src_port, bit<16> dst_port) {
        ig_md.bridge_trg = MAX_PORT;
        ig_md.vrf = 0;
        ig_md.nsh_valid = 0;
        ig_md.polka_valid = 0;
        ig_md.mpls0_valid = 0;
        ig_md.mpls1_valid = 0;
        ig_md.arp_valid = 0;
        ig_md.llc_valid = 0;
        ig_md.ipv4_valid = 0;
        ig_md.ipv6_valid = 0;
        hdr.vlan.setInvalid();
        hdr.eth2.setValid();
        hdr.eth2 = hdr.ethernet;
        hdr.eth2.ethertype = ig_md.ethertype;
        ig_md.nexthop_id = nexthop;

        hdr.udp2.setValid();
        hdr.udp2.src_port = src_port;
        hdr.udp2.dst_port = dst_port;
        hdr.udp2.length = (bit<16>)ig_intr_md.packet_length - ig_md.vlan_size - 14 + 22;
        hdr.udp2.checksum = 0;

        hdr.ipv6d.setValid();
        hdr.ipv6d.version = 6;
        hdr.ipv6d.traffic_class = 0;
        hdr.ipv6d.flow_label = 0;
        hdr.ipv6d.payload_len = (bit<16>)ig_intr_md.packet_length - ig_md.vlan_size - 14 + 22;
        hdr.ipv6d.next_hdr = IP_PROTOCOL_UDP;
        hdr.ipv6d.hop_limit = 255;
        hdr.ipv6d.src_addr = src_ip_addr;
        hdr.ipv6d.dst_addr = dst_ip_addr;
        ig_md.ethertype = ETHERTYPE_IPV6;
    }







    action act_bridge_punt() {
        ig_md.bridge_trg = 0;
    }

    table tbl_bridge_target {
        key = {
ig_md.bridge_id:
            exact;
hdr.ethernet.dst_mac_addr:
            exact;
        }
        actions = {
            act_set_bridge_out;
            act_set_bridge_routed;
            act_set_bridge_ppprouted;
            act_set_bridge_vpls;
            act_set_bridge_srv;
            act_set_bridge_vxlan4;
            act_set_bridge_vxlan6;
            act_set_bridge_pckoudp4;
            act_set_bridge_pckoudp6;
            act_bridge_punt;
        }
        size = MAC_TABLE_SIZE;
        default_action = act_bridge_punt();
        counters = statsTx;
    }


    apply {
        if (ig_md.bridge_id == 0) {
            return;
        }
        ig_md.vrf = 0;
        ig_md.nsh_valid = 0;
        ig_md.polka_valid = 0;
        ig_md.mpls0_valid = 0;
        ig_md.mpls1_valid = 0;
        ig_md.arp_valid = 0;
        ig_md.llc_valid = 0;
        ig_md.ipv4_valid = 0;
        ig_md.ipv6_valid = 0;
        if (hdr.eth6.isValid()) {
            hdr.ethernet.dst_mac_addr = hdr.eth6.dst_mac_addr;
            hdr.ethernet.src_mac_addr = hdr.eth6.src_mac_addr;
        }
        if (tbl_bridge_learn.apply().hit) tbl_bridge_target.apply();
        if ((ig_md.bridge_src == 0) || (ig_md.bridge_trg == 0)) {
            send_to_cpu();
            return;
        }
        if (hdr.eth6.isValid()) {
            ig_md.ethertype = hdr.eth6.ethertype;
            hdr.eth6.setInvalid();
            hdr.eth5.setInvalid();
        }
        if (hdr.mpls1.isValid() && (ig_md.mpls_op_type != 3)) {
            hdr.eth2.setInvalid();
            hdr.mpls1.setInvalid();
            hdr.mpls0.setInvalid();
            hdr.vlan.setInvalid();
            ig_md.mpls0_remove = 0;
            ig_md.mpls1_remove = 0;
        }
    }
}

#endif // _IG_CTL_BRIDGE_P4_

