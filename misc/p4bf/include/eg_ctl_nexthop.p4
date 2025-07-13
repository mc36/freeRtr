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

#ifndef _EG_CTL_NEXTHOP_P4_
#define _EG_CTL_NEXTHOP_P4_

control EgressControlNexthop(inout headers hdr, inout ingress_metadata_t eg_md,
                             in egress_intrinsic_metadata_t eg_intr_md,
                             inout egress_intrinsic_metadata_for_deparser_t eg_dprsr_md)
{


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
        eg_md.target_id = egress_port;
        eg_md.aclport_id = egress_port;

    }


    action act_ipv4_fib_discard() {
        eg_dprsr_md.drop_ctl = 1;
    }


#ifdef HAVE_MPLS
#ifdef HAVE_BRIDGE
    action act_ipv4_pwhe(mac_addr_t dst_mac_addr, mac_addr_t src_mac_addr, SubIntId_t egress_port, SubIntId_t acl_port, mac_addr_t core_dst_mac, mac_addr_t core_src_mac, label_t egress_label, label_t vpn_label) {
        eg_md.target_id = egress_port;
        eg_md.aclport_id = acl_port;
        hdr.ethernet.src_mac_addr = core_src_mac;
        hdr.ethernet.dst_mac_addr = core_dst_mac;
        hdr.eth9.setValid();
        hdr.eth9.src_mac_addr = src_mac_addr;
        hdr.eth9.dst_mac_addr = dst_mac_addr;
        hdr.eth9.ethertype = eg_md.ethertype;
        hdr.mpls90.setValid();
        hdr.mpls90.label = egress_label;
        hdr.mpls90.ttl = 255;
        hdr.mpls90.bos = 0;
        hdr.mpls91.setValid();
        hdr.mpls91.label = vpn_label;
        hdr.mpls91.ttl = 255;
        hdr.mpls91.bos = 1;
        eg_md.ethertype = ETHERTYPE_MPLS_UCAST;
    }
#endif
#endif


#ifdef HAVE_MPLS
    action act_ipv4_labs9(mac_addr_t dst_mac_addr, mac_addr_t src_mac_addr, SubIntId_t egress_port, SubIntId_t acl_port, label_t lab0, label_t lab1, label_t lab2, label_t lab3, label_t lab4, label_t lab5, label_t lab6, label_t lab7, label_t lab8, label_t lab9) {
        eg_md.target_id = egress_port;
        eg_md.aclport_id = acl_port;
        hdr.ethernet.src_mac_addr = src_mac_addr;
        hdr.ethernet.dst_mac_addr = dst_mac_addr;
        hdr.mpls80.setValid();
        hdr.mpls80.label = lab0;
        hdr.mpls80.ttl = 255;
        hdr.mpls80.bos = 1;
        hdr.mpls81.setValid();
        hdr.mpls81.label = lab1;
        hdr.mpls81.ttl = 255;
        hdr.mpls81.bos = 0;
        hdr.mpls82.setValid();
        hdr.mpls82.label = lab2;
        hdr.mpls82.ttl = 255;
        hdr.mpls82.bos = 0;
        hdr.mpls83.setValid();
        hdr.mpls83.label = lab3;
        hdr.mpls83.ttl = 255;
        hdr.mpls83.bos = 0;
        hdr.mpls84.setValid();
        hdr.mpls84.label = lab4;
        hdr.mpls84.ttl = 255;
        hdr.mpls84.bos = 0;
        hdr.mpls85.setValid();
        hdr.mpls85.label = lab5;
        hdr.mpls85.ttl = 255;
        hdr.mpls85.bos = 0;
        hdr.mpls86.setValid();
        hdr.mpls86.label = lab6;
        hdr.mpls86.ttl = 255;
        hdr.mpls86.bos = 0;
        hdr.mpls87.setValid();
        hdr.mpls87.label = lab7;
        hdr.mpls87.ttl = 255;
        hdr.mpls87.bos = 0;
        hdr.mpls88.setValid();
        hdr.mpls88.label = lab8;
        hdr.mpls88.ttl = 255;
        hdr.mpls88.bos = 0;
        hdr.mpls89.setValid();
        hdr.mpls89.label = lab9;
        hdr.mpls89.ttl = 255;
        hdr.mpls89.bos = 0;
        eg_md.ethertype = ETHERTYPE_MPLS_UCAST;
    }


    action act_ipv4_labs8(mac_addr_t dst_mac_addr, mac_addr_t src_mac_addr, SubIntId_t egress_port, SubIntId_t acl_port, label_t lab0, label_t lab1, label_t lab2, label_t lab3, label_t lab4, label_t lab5, label_t lab6, label_t lab7, label_t lab8) {
        eg_md.target_id = egress_port;
        eg_md.aclport_id = acl_port;
        hdr.ethernet.src_mac_addr = src_mac_addr;
        hdr.ethernet.dst_mac_addr = dst_mac_addr;
        hdr.mpls80.setValid();
        hdr.mpls80.label = lab0;
        hdr.mpls80.ttl = 255;
        hdr.mpls80.bos = 1;
        hdr.mpls81.setValid();
        hdr.mpls81.label = lab1;
        hdr.mpls81.ttl = 255;
        hdr.mpls81.bos = 0;
        hdr.mpls82.setValid();
        hdr.mpls82.label = lab2;
        hdr.mpls82.ttl = 255;
        hdr.mpls82.bos = 0;
        hdr.mpls83.setValid();
        hdr.mpls83.label = lab3;
        hdr.mpls83.ttl = 255;
        hdr.mpls83.bos = 0;
        hdr.mpls84.setValid();
        hdr.mpls84.label = lab4;
        hdr.mpls84.ttl = 255;
        hdr.mpls84.bos = 0;
        hdr.mpls85.setValid();
        hdr.mpls85.label = lab5;
        hdr.mpls85.ttl = 255;
        hdr.mpls85.bos = 0;
        hdr.mpls86.setValid();
        hdr.mpls86.label = lab6;
        hdr.mpls86.ttl = 255;
        hdr.mpls86.bos = 0;
        hdr.mpls87.setValid();
        hdr.mpls87.label = lab7;
        hdr.mpls87.ttl = 255;
        hdr.mpls87.bos = 0;
        hdr.mpls88.setValid();
        hdr.mpls88.label = lab8;
        hdr.mpls88.ttl = 255;
        hdr.mpls88.bos = 0;
        eg_md.ethertype = ETHERTYPE_MPLS_UCAST;
    }


    action act_ipv4_labs7(mac_addr_t dst_mac_addr, mac_addr_t src_mac_addr, SubIntId_t egress_port, SubIntId_t acl_port, label_t lab0, label_t lab1, label_t lab2, label_t lab3, label_t lab4, label_t lab5, label_t lab6, label_t lab7) {
        eg_md.target_id = egress_port;
        eg_md.aclport_id = acl_port;
        hdr.ethernet.src_mac_addr = src_mac_addr;
        hdr.ethernet.dst_mac_addr = dst_mac_addr;
        hdr.mpls80.setValid();
        hdr.mpls80.label = lab0;
        hdr.mpls80.ttl = 255;
        hdr.mpls80.bos = 1;
        hdr.mpls81.setValid();
        hdr.mpls81.label = lab1;
        hdr.mpls81.ttl = 255;
        hdr.mpls81.bos = 0;
        hdr.mpls82.setValid();
        hdr.mpls82.label = lab2;
        hdr.mpls82.ttl = 255;
        hdr.mpls82.bos = 0;
        hdr.mpls83.setValid();
        hdr.mpls83.label = lab3;
        hdr.mpls83.ttl = 255;
        hdr.mpls83.bos = 0;
        hdr.mpls84.setValid();
        hdr.mpls84.label = lab4;
        hdr.mpls84.ttl = 255;
        hdr.mpls84.bos = 0;
        hdr.mpls85.setValid();
        hdr.mpls85.label = lab5;
        hdr.mpls85.ttl = 255;
        hdr.mpls85.bos = 0;
        hdr.mpls86.setValid();
        hdr.mpls86.label = lab6;
        hdr.mpls86.ttl = 255;
        hdr.mpls86.bos = 0;
        hdr.mpls87.setValid();
        hdr.mpls87.label = lab7;
        hdr.mpls87.ttl = 255;
        hdr.mpls87.bos = 0;
        eg_md.ethertype = ETHERTYPE_MPLS_UCAST;
    }


    action act_ipv4_labs6(mac_addr_t dst_mac_addr, mac_addr_t src_mac_addr, SubIntId_t egress_port, SubIntId_t acl_port, label_t lab0, label_t lab1, label_t lab2, label_t lab3, label_t lab4, label_t lab5, label_t lab6) {
        eg_md.target_id = egress_port;
        eg_md.aclport_id = acl_port;
        hdr.ethernet.src_mac_addr = src_mac_addr;
        hdr.ethernet.dst_mac_addr = dst_mac_addr;
        hdr.mpls80.setValid();
        hdr.mpls80.label = lab0;
        hdr.mpls80.ttl = 255;
        hdr.mpls80.bos = 1;
        hdr.mpls81.setValid();
        hdr.mpls81.label = lab1;
        hdr.mpls81.ttl = 255;
        hdr.mpls81.bos = 0;
        hdr.mpls82.setValid();
        hdr.mpls82.label = lab2;
        hdr.mpls82.ttl = 255;
        hdr.mpls82.bos = 0;
        hdr.mpls83.setValid();
        hdr.mpls83.label = lab3;
        hdr.mpls83.ttl = 255;
        hdr.mpls83.bos = 0;
        hdr.mpls84.setValid();
        hdr.mpls84.label = lab4;
        hdr.mpls84.ttl = 255;
        hdr.mpls84.bos = 0;
        hdr.mpls85.setValid();
        hdr.mpls85.label = lab5;
        hdr.mpls85.ttl = 255;
        hdr.mpls85.bos = 0;
        hdr.mpls86.setValid();
        hdr.mpls86.label = lab6;
        hdr.mpls86.ttl = 255;
        hdr.mpls86.bos = 0;
        eg_md.ethertype = ETHERTYPE_MPLS_UCAST;
    }


    action act_ipv4_labs5(mac_addr_t dst_mac_addr, mac_addr_t src_mac_addr, SubIntId_t egress_port, SubIntId_t acl_port, label_t lab0, label_t lab1, label_t lab2, label_t lab3, label_t lab4, label_t lab5) {
        eg_md.target_id = egress_port;
        eg_md.aclport_id = acl_port;
        hdr.ethernet.src_mac_addr = src_mac_addr;
        hdr.ethernet.dst_mac_addr = dst_mac_addr;
        hdr.mpls80.setValid();
        hdr.mpls80.label = lab0;
        hdr.mpls80.ttl = 255;
        hdr.mpls80.bos = 1;
        hdr.mpls81.setValid();
        hdr.mpls81.label = lab1;
        hdr.mpls81.ttl = 255;
        hdr.mpls81.bos = 0;
        hdr.mpls82.setValid();
        hdr.mpls82.label = lab2;
        hdr.mpls82.ttl = 255;
        hdr.mpls82.bos = 0;
        hdr.mpls83.setValid();
        hdr.mpls83.label = lab3;
        hdr.mpls83.ttl = 255;
        hdr.mpls83.bos = 0;
        hdr.mpls84.setValid();
        hdr.mpls84.label = lab4;
        hdr.mpls84.ttl = 255;
        hdr.mpls84.bos = 0;
        hdr.mpls85.setValid();
        hdr.mpls85.label = lab5;
        hdr.mpls85.ttl = 255;
        hdr.mpls85.bos = 0;
        eg_md.ethertype = ETHERTYPE_MPLS_UCAST;
    }


    action act_ipv4_labs4(mac_addr_t dst_mac_addr, mac_addr_t src_mac_addr, SubIntId_t egress_port, SubIntId_t acl_port, label_t lab0, label_t lab1, label_t lab2, label_t lab3, label_t lab4) {
        eg_md.target_id = egress_port;
        eg_md.aclport_id = acl_port;
        hdr.ethernet.src_mac_addr = src_mac_addr;
        hdr.ethernet.dst_mac_addr = dst_mac_addr;
        hdr.mpls80.setValid();
        hdr.mpls80.label = lab0;
        hdr.mpls80.ttl = 255;
        hdr.mpls80.bos = 1;
        hdr.mpls81.setValid();
        hdr.mpls81.label = lab1;
        hdr.mpls81.ttl = 255;
        hdr.mpls81.bos = 0;
        hdr.mpls82.setValid();
        hdr.mpls82.label = lab2;
        hdr.mpls82.ttl = 255;
        hdr.mpls82.bos = 0;
        hdr.mpls83.setValid();
        hdr.mpls83.label = lab3;
        hdr.mpls83.ttl = 255;
        hdr.mpls83.bos = 0;
        hdr.mpls84.setValid();
        hdr.mpls84.label = lab4;
        hdr.mpls84.ttl = 255;
        hdr.mpls84.bos = 0;
        eg_md.ethertype = ETHERTYPE_MPLS_UCAST;
    }


    action act_ipv4_labs3(mac_addr_t dst_mac_addr, mac_addr_t src_mac_addr, SubIntId_t egress_port, SubIntId_t acl_port, label_t lab0, label_t lab1, label_t lab2, label_t lab3) {
        eg_md.target_id = egress_port;
        eg_md.aclport_id = acl_port;
        hdr.ethernet.src_mac_addr = src_mac_addr;
        hdr.ethernet.dst_mac_addr = dst_mac_addr;
        hdr.mpls80.setValid();
        hdr.mpls80.label = lab0;
        hdr.mpls80.ttl = 255;
        hdr.mpls80.bos = 1;
        hdr.mpls81.setValid();
        hdr.mpls81.label = lab1;
        hdr.mpls81.ttl = 255;
        hdr.mpls81.bos = 0;
        hdr.mpls82.setValid();
        hdr.mpls82.label = lab2;
        hdr.mpls82.ttl = 255;
        hdr.mpls82.bos = 0;
        hdr.mpls83.setValid();
        hdr.mpls83.label = lab3;
        hdr.mpls83.ttl = 255;
        hdr.mpls83.bos = 0;
        eg_md.ethertype = ETHERTYPE_MPLS_UCAST;
    }


    action act_ipv4_labs2(mac_addr_t dst_mac_addr, mac_addr_t src_mac_addr, SubIntId_t egress_port, SubIntId_t acl_port, label_t lab0, label_t lab1, label_t lab2) {
        eg_md.target_id = egress_port;
        eg_md.aclport_id = acl_port;
        hdr.ethernet.src_mac_addr = src_mac_addr;
        hdr.ethernet.dst_mac_addr = dst_mac_addr;
        hdr.mpls80.setValid();
        hdr.mpls80.label = lab0;
        hdr.mpls80.ttl = 255;
        hdr.mpls80.bos = 1;
        hdr.mpls81.setValid();
        hdr.mpls81.label = lab1;
        hdr.mpls81.ttl = 255;
        hdr.mpls81.bos = 0;
        hdr.mpls82.setValid();
        hdr.mpls82.label = lab2;
        hdr.mpls82.ttl = 255;
        hdr.mpls82.bos = 0;
        eg_md.ethertype = ETHERTYPE_MPLS_UCAST;
    }


    action act_ipv4_labs1(mac_addr_t dst_mac_addr, mac_addr_t src_mac_addr, SubIntId_t egress_port, SubIntId_t acl_port, label_t lab0, label_t lab1) {
        eg_md.target_id = egress_port;
        eg_md.aclport_id = acl_port;
        hdr.ethernet.src_mac_addr = src_mac_addr;
        hdr.ethernet.dst_mac_addr = dst_mac_addr;
        hdr.mpls80.setValid();
        hdr.mpls80.label = lab0;
        hdr.mpls80.ttl = 255;
        hdr.mpls80.bos = 1;
        hdr.mpls81.setValid();
        hdr.mpls81.label = lab1;
        hdr.mpls81.ttl = 255;
        hdr.mpls81.bos = 0;
        eg_md.ethertype = ETHERTYPE_MPLS_UCAST;
    }


    action act_ipv4_labs0(mac_addr_t dst_mac_addr, mac_addr_t src_mac_addr, SubIntId_t egress_port, SubIntId_t acl_port, label_t lab0) {
        eg_md.target_id = egress_port;
        eg_md.aclport_id = acl_port;
        hdr.ethernet.src_mac_addr = src_mac_addr;
        hdr.ethernet.dst_mac_addr = dst_mac_addr;
        hdr.mpls80.setValid();
        hdr.mpls80.label = lab0;
        hdr.mpls80.ttl = 255;
        hdr.mpls80.bos = 1;
        eg_md.ethertype = ETHERTYPE_MPLS_UCAST;
    }
#endif


#ifdef HAVE_GRE


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
        eg_md.target_id = egress_port;
        eg_md.aclport_id = acl_port;

        hdr.gre2.setValid();
        hdr.gre2.flags = 0;
        hdr.gre2.gretyp = eg_md.ethertype;

        hdr.ipv4d.setValid();
        hdr.ipv4d.version = 4;
        hdr.ipv4d.ihl = 5;
        hdr.ipv4d.diffserv = 0;
        hdr.ipv4d.total_len = eg_md.pktlen + 24;
        hdr.ipv4d.identification = 0;
        hdr.ipv4d.flags = 0;
        hdr.ipv4d.frag_offset = 0;
        hdr.ipv4d.ttl = 255;
        hdr.ipv4d.protocol = IP_PROTOCOL_GRE;
        hdr.ipv4d.hdr_checksum = 0;
        hdr.ipv4d.src_addr = src_ip_addr;
        hdr.ipv4d.dst_addr = dst_ip_addr;
        eg_md.ethertype = ETHERTYPE_IPV4;
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
        eg_md.target_id = egress_port;
        eg_md.aclport_id = acl_port;

        hdr.gre2.setValid();
        hdr.gre2.flags = 0;
        hdr.gre2.gretyp = eg_md.ethertype;

        hdr.ipv6d.setValid();
        hdr.ipv6d.version = 6;
        hdr.ipv6d.traffic_class = 0;
        hdr.ipv6d.flow_label = 0;
        hdr.ipv6d.payload_len = eg_md.pktlen + 4;
        hdr.ipv6d.next_hdr = IP_PROTOCOL_GRE;
        hdr.ipv6d.hop_limit = 255;
        hdr.ipv6d.src_addr = src_ip_addr;
        hdr.ipv6d.dst_addr = dst_ip_addr;
        eg_md.ethertype = ETHERTYPE_IPV6;
    }


#endif


#ifdef HAVE_TMUX

    Hash<bit<8>>(HashAlgorithm_t.XOR8) tmux_checksum;

    action act_ipv4_tmux4(mac_addr_t dst_mac_addr, mac_addr_t src_mac_addr, SubIntId_t egress_port, SubIntId_t acl_port, ipv4_addr_t dst_ip_addr, ipv4_addr_t src_ip_addr) {
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
        eg_md.target_id = egress_port;
        eg_md.aclport_id = acl_port;

        hdr.tmux2.setValid();
        hdr.tmux2.length = eg_md.pktlen + 4;

        hdr.ipv4d.setValid();
        hdr.ipv4d.version = 4;
        hdr.ipv4d.ihl = 5;
        hdr.ipv4d.diffserv = 0;
        hdr.ipv4d.total_len = eg_md.pktlen + 24;
        hdr.ipv4d.identification = 0;
        hdr.ipv4d.flags = 0;
        hdr.ipv4d.frag_offset = 0;
        hdr.ipv4d.ttl = 255;
        hdr.ipv4d.protocol = IP_PROTOCOL_TMUX;
        hdr.ipv4d.hdr_checksum = 0;
        hdr.ipv4d.src_addr = src_ip_addr;
        hdr.ipv4d.dst_addr = dst_ip_addr;
    }

    action act_ipv4_tmux6(mac_addr_t dst_mac_addr, mac_addr_t src_mac_addr, SubIntId_t egress_port, SubIntId_t acl_port, ipv6_addr_t dst_ip_addr, ipv6_addr_t src_ip_addr) {
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
        eg_md.target_id = egress_port;
        eg_md.aclport_id = acl_port;

        hdr.tmux2.setValid();
        hdr.tmux2.length = eg_md.pktlen + 4;

        hdr.ipv6d.setValid();
        hdr.ipv6d.version = 6;
        hdr.ipv6d.traffic_class = 0;
        hdr.ipv6d.flow_label = 0;
        hdr.ipv6d.payload_len = eg_md.pktlen + 4;
        hdr.ipv6d.next_hdr = IP_PROTOCOL_TMUX;
        hdr.ipv6d.hop_limit = 255;
        hdr.ipv6d.src_addr = src_ip_addr;
        hdr.ipv6d.dst_addr = dst_ip_addr;
    }


#endif



#ifdef HAVE_IPIP

    action act_ipv4_ipip4(mac_addr_t dst_mac_addr, mac_addr_t src_mac_addr, SubIntId_t egress_port, SubIntId_t acl_port, ipv4_addr_t dst_ip_addr, ipv4_addr_t src_ip_addr) {
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
        eg_md.target_id = egress_port;
        eg_md.aclport_id = acl_port;

        hdr.ipv4d.setValid();
        hdr.ipv4d.version = 4;
        hdr.ipv4d.ihl = 5;
        hdr.ipv4d.diffserv = 0;
        hdr.ipv4d.total_len = eg_md.pktlen + 20;
        hdr.ipv4d.identification = 0;
        hdr.ipv4d.flags = 0;
        hdr.ipv4d.frag_offset = 0;
        hdr.ipv4d.ttl = 255;
        hdr.ipv4d.protocol = 0;
        hdr.ipv4d.hdr_checksum = 0;
        hdr.ipv4d.src_addr = src_ip_addr;
        hdr.ipv4d.dst_addr = dst_ip_addr;
    }

    action act_ipv4_ipip6(mac_addr_t dst_mac_addr, mac_addr_t src_mac_addr, SubIntId_t egress_port, SubIntId_t acl_port, ipv6_addr_t dst_ip_addr, ipv6_addr_t src_ip_addr) {
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
        eg_md.target_id = egress_port;
        eg_md.aclport_id = acl_port;

        hdr.ipv6d.setValid();
        hdr.ipv6d.version = 6;
        hdr.ipv6d.traffic_class = 0;
        hdr.ipv6d.flow_label = 0;
        hdr.ipv6d.payload_len = eg_md.pktlen;
        hdr.ipv6d.next_hdr = 0;
        hdr.ipv6d.hop_limit = 255;
        hdr.ipv6d.src_addr = src_ip_addr;
        hdr.ipv6d.dst_addr = dst_ip_addr;
    }

#endif



#ifdef HAVE_PPPOE


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
        eg_md.target_id = egress_port;
        eg_md.aclport_id = acl_port;

        hdr.pppoeD.setValid();
        hdr.pppoeD.ver = 1;
        hdr.pppoeD.type = 1;
        hdr.pppoeD.code = 0;
        hdr.pppoeD.session = session;
        hdr.pppoeD.length = eg_md.pktlen + 2;
        hdr.pppoeD.ppptyp = 0;
    }

#endif


#ifdef HAVE_L2TP


    action act_ipv4_l2tp4(mac_addr_t dst_mac_addr, mac_addr_t src_mac_addr, SubIntId_t egress_port, SubIntId_t acl_port, ipv4_addr_t dst_ip_addr, ipv4_addr_t src_ip_addr, layer4_port_t src_port, layer4_port_t dst_port, bit<32> tunnel_id) {
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
        eg_md.target_id = egress_port;
        eg_md.aclport_id = acl_port;

        hdr.l2tp2.setValid();
        hdr.l2tp2.flags = 0x202;
        hdr.l2tp2.tidsid = tunnel_id;
        hdr.l2tp2.offset = 0;
        hdr.l2tp2.pppflags = 0xff03;
        hdr.l2tp2.ppptyp = 0;

        hdr.udp2.setValid();
        hdr.udp2.src_port = src_port;
        hdr.udp2.dst_port = dst_port;
        hdr.udp2.length = eg_md.pktlen + 20;
        hdr.udp2.checksum = 0;

        hdr.ipv4d.setValid();
        hdr.ipv4d.version = 4;
        hdr.ipv4d.ihl = 5;
        hdr.ipv4d.diffserv = 0;
        hdr.ipv4d.total_len = eg_md.pktlen + 40;
        hdr.ipv4d.identification = 0;
        hdr.ipv4d.flags = 0;
        hdr.ipv4d.frag_offset = 0;
        hdr.ipv4d.ttl = 255;
        hdr.ipv4d.protocol = IP_PROTOCOL_UDP;
        hdr.ipv4d.hdr_checksum = 0;
        hdr.ipv4d.src_addr = src_ip_addr;
        hdr.ipv4d.dst_addr = dst_ip_addr;
    }


    action act_ipv4_l2tp6(mac_addr_t dst_mac_addr, mac_addr_t src_mac_addr, SubIntId_t egress_port, SubIntId_t acl_port, ipv6_addr_t dst_ip_addr, ipv6_addr_t src_ip_addr, layer4_port_t src_port, layer4_port_t dst_port, bit<32> tunnel_id) {
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
        eg_md.target_id = egress_port;
        eg_md.aclport_id = acl_port;

        hdr.l2tp2.setValid();
        hdr.l2tp2.flags = 0x202;
        hdr.l2tp2.tidsid = tunnel_id;
        hdr.l2tp2.offset = 0;
        hdr.l2tp2.pppflags = 0xff03;
        hdr.l2tp2.ppptyp = 0;

        hdr.udp2.setValid();
        hdr.udp2.src_port = src_port;
        hdr.udp2.dst_port = dst_port;
        hdr.udp2.length = eg_md.pktlen + 20;
        hdr.udp2.checksum = 0;

        hdr.ipv6d.setValid();
        hdr.ipv6d.version = 6;
        hdr.ipv6d.traffic_class = 0;
        hdr.ipv6d.flow_label = 0;
        hdr.ipv6d.payload_len = eg_md.pktlen + 20;
        hdr.ipv6d.next_hdr = IP_PROTOCOL_UDP;
        hdr.ipv6d.hop_limit = 255;
        hdr.ipv6d.src_addr = src_ip_addr;
        hdr.ipv6d.dst_addr = dst_ip_addr;
    }


#endif



#ifdef HAVE_L3TP


    action act_ipv4_l3tp4(mac_addr_t dst_mac_addr, mac_addr_t src_mac_addr, SubIntId_t egress_port, SubIntId_t acl_port, ipv4_addr_t dst_ip_addr, ipv4_addr_t src_ip_addr, bit<32> tunnel_id) {
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
        eg_md.target_id = egress_port;
        eg_md.aclport_id = acl_port;

        hdr.l3tp2.setValid();
        hdr.l3tp2.tidsid = tunnel_id;
        hdr.l3tp2.ppptyp = 0;

        hdr.ipv4d.setValid();
        hdr.ipv4d.version = 4;
        hdr.ipv4d.ihl = 5;
        hdr.ipv4d.diffserv = 0;
        hdr.ipv4d.total_len = eg_md.pktlen + 26;
        hdr.ipv4d.identification = 0;
        hdr.ipv4d.flags = 0;
        hdr.ipv4d.frag_offset = 0;
        hdr.ipv4d.ttl = 255;
        hdr.ipv4d.protocol = IP_PROTOCOL_L2TP;
        hdr.ipv4d.hdr_checksum = 0;
        hdr.ipv4d.src_addr = src_ip_addr;
        hdr.ipv4d.dst_addr = dst_ip_addr;
    }


    action act_ipv4_l3tp6(mac_addr_t dst_mac_addr, mac_addr_t src_mac_addr, SubIntId_t egress_port, SubIntId_t acl_port, ipv6_addr_t dst_ip_addr, ipv6_addr_t src_ip_addr, bit<32> tunnel_id) {
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
        eg_md.target_id = egress_port;
        eg_md.aclport_id = acl_port;

        hdr.l3tp2.setValid();
        hdr.l3tp2.tidsid = tunnel_id;
        hdr.l3tp2.ppptyp = 0;

        hdr.ipv6d.setValid();
        hdr.ipv6d.version = 6;
        hdr.ipv6d.traffic_class = 0;
        hdr.ipv6d.flow_label = 0;
        hdr.ipv6d.payload_len = eg_md.pktlen + 6;
        hdr.ipv6d.next_hdr = IP_PROTOCOL_L2TP;
        hdr.ipv6d.hop_limit = 255;
        hdr.ipv6d.src_addr = src_ip_addr;
        hdr.ipv6d.dst_addr = dst_ip_addr;
    }

#endif




#ifdef HAVE_GTP


    action act_ipv4_gtp4(mac_addr_t dst_mac_addr, mac_addr_t src_mac_addr, SubIntId_t egress_port, SubIntId_t acl_port, ipv4_addr_t dst_ip_addr, ipv4_addr_t src_ip_addr, layer4_port_t src_port, layer4_port_t dst_port, bit<32> teid) {
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
        eg_md.target_id = egress_port;
        eg_md.aclport_id = acl_port;

        hdr.gtp2.setValid();
        hdr.gtp2.flag = 0x30;
        hdr.gtp2.type = 0xff;
        hdr.gtp2.length = eg_md.pktlen;
        hdr.gtp2.teid = teid;

        hdr.udp2.setValid();
        hdr.udp2.src_port = src_port;
        hdr.udp2.dst_port = dst_port;
        hdr.udp2.length = eg_md.pktlen + 16;
        hdr.udp2.checksum = 0;

        hdr.ipv4d.setValid();
        hdr.ipv4d.version = 4;
        hdr.ipv4d.ihl = 5;
        hdr.ipv4d.diffserv = 0;
        hdr.ipv4d.total_len = eg_md.pktlen + 36;
        hdr.ipv4d.identification = 0;
        hdr.ipv4d.flags = 0;
        hdr.ipv4d.frag_offset = 0;
        hdr.ipv4d.ttl = 255;
        hdr.ipv4d.protocol = IP_PROTOCOL_UDP;
        hdr.ipv4d.hdr_checksum = 0;
        hdr.ipv4d.src_addr = src_ip_addr;
        hdr.ipv4d.dst_addr = dst_ip_addr;
        eg_md.ethertype = ETHERTYPE_IPV4;
    }


    action act_ipv4_gtp6(mac_addr_t dst_mac_addr, mac_addr_t src_mac_addr, SubIntId_t egress_port, SubIntId_t acl_port, ipv6_addr_t dst_ip_addr, ipv6_addr_t src_ip_addr, layer4_port_t src_port, layer4_port_t dst_port, bit<32> teid) {
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
        eg_md.target_id = egress_port;
        eg_md.aclport_id = acl_port;

        hdr.gtp2.setValid();
        hdr.gtp2.flag = 0x30;
        hdr.gtp2.type = 0xff;
        hdr.gtp2.length = eg_md.pktlen;
        hdr.gtp2.teid = teid;

        hdr.udp2.setValid();
        hdr.udp2.src_port = src_port;
        hdr.udp2.dst_port = dst_port;
        hdr.udp2.length = eg_md.pktlen + 16;
        hdr.udp2.checksum = 0;

        hdr.ipv6d.setValid();
        hdr.ipv6d.version = 6;
        hdr.ipv6d.traffic_class = 0;
        hdr.ipv6d.flow_label = 0;
        hdr.ipv6d.payload_len = eg_md.pktlen + 16;
        hdr.ipv6d.next_hdr = IP_PROTOCOL_UDP;
        hdr.ipv6d.hop_limit = 255;
        hdr.ipv6d.src_addr = src_ip_addr;
        hdr.ipv6d.dst_addr = dst_ip_addr;
        eg_md.ethertype = ETHERTYPE_IPV6;
    }


#endif



    table tbl_nexthop {
        key = {
eg_md.nexthop_id:
            exact;
        }
        actions = {
            act_ipv4_fib_hit;
            act_ipv4_fib_discard;
#ifdef HAVE_MPLS
#ifdef HAVE_BRIDGE
            act_ipv4_pwhe;
#endif
#endif
#ifdef HAVE_MPLS
            act_ipv4_labs0;
            act_ipv4_labs1;
            act_ipv4_labs2;
            act_ipv4_labs3;
            act_ipv4_labs4;
            act_ipv4_labs5;
            act_ipv4_labs6;
            act_ipv4_labs7;
            act_ipv4_labs8;
            act_ipv4_labs9;
#endif
#ifdef HAVE_GRE
            act_ipv4_gre4;
            act_ipv4_gre6;
#endif
#ifdef HAVE_TMUX
            act_ipv4_tmux4;
            act_ipv4_tmux6;
#endif
#ifdef HAVE_PPPOE
            act_ipv4_pppoe;
#endif
#ifdef HAVE_L2TP
            act_ipv4_l2tp4;
            act_ipv4_l2tp6;
#endif
#ifdef HAVE_L3TP
            act_ipv4_l3tp4;
            act_ipv4_l3tp6;
#endif
#ifdef HAVE_IPIP
            act_ipv4_ipip4;
            act_ipv4_ipip6;
#endif
#ifdef HAVE_GTP
            act_ipv4_gtp4;
            act_ipv4_gtp6;
#endif
            @defaultonly NoAction;
        }
        size = NEXTHOP_TABLE_SIZE;
        const default_action = NoAction();
    }

    apply {
        if (eg_md.target_id == 0) {
            tbl_nexthop.apply();
#ifdef HAVE_PPPOE
            if (hdr.pppoeD.isValid()) {
                if (eg_md.ethertype == ETHERTYPE_IPV4) hdr.pppoeD.ppptyp = PPPTYPE_IPV4;
                else if (eg_md.ethertype == ETHERTYPE_IPV6) hdr.pppoeD.ppptyp = PPPTYPE_IPV6;
#ifdef HAVE_SGT
                else if (eg_md.ethertype == ETHERTYPE_SGT) hdr.pppoeD.ppptyp = PPPTYPE_SGT;
#endif
#ifdef HAVE_NSH
                else if (eg_md.ethertype == ETHERTYPE_NSH) hdr.pppoeD.ppptyp = PPPTYPE_NSH;
#endif
#ifdef HAVE_MPLS
                else if (eg_md.ethertype == ETHERTYPE_MPLS_UCAST) hdr.pppoeD.ppptyp = PPPTYPE_MPLS_UCAST;
#endif
#ifdef HAVE_TAP
                else if (eg_md.ethertype == ETHERTYPE_ROUTEDMAC) hdr.pppoeD.ppptyp = PPPTYPE_ROUTEDMAC;
#endif
                eg_md.ethertype = ETHERTYPE_PPPOE_DATA;
            }
#endif
#ifdef HAVE_L2TP
            if (hdr.l2tp2.isValid()) {
                if (eg_md.ethertype == ETHERTYPE_IPV4) hdr.l2tp2.ppptyp = PPPTYPE_IPV4;
                else if (eg_md.ethertype == ETHERTYPE_IPV6) hdr.l2tp2.ppptyp = PPPTYPE_IPV6;
#ifdef HAVE_SGT
                else if (eg_md.ethertype == ETHERTYPE_SGT) hdr.l2tp2.ppptyp = PPPTYPE_SGT;
#endif
#ifdef HAVE_NSH
                else if (eg_md.ethertype == ETHERTYPE_NSH) hdr.l2tp2.ppptyp = PPPTYPE_NSH;
#endif
#ifdef HAVE_MPLS
                else if (eg_md.ethertype == ETHERTYPE_MPLS_UCAST) hdr.l2tp2.ppptyp = PPPTYPE_MPLS_UCAST;
#endif
#ifdef HAVE_TAP
                else if (eg_md.ethertype == ETHERTYPE_ROUTEDMAC) hdr.l2tp2.ppptyp = PPPTYPE_ROUTEDMAC;
#endif

                if (hdr.ipv4d.isValid()) eg_md.ethertype = ETHERTYPE_IPV4;
                else if (hdr.ipv6d.isValid()) eg_md.ethertype = ETHERTYPE_IPV6;
            }
#endif
#ifdef HAVE_L3TP
            if (hdr.l3tp2.isValid()) {
                if (eg_md.ethertype == ETHERTYPE_IPV4) hdr.l3tp2.ppptyp = PPPTYPE_IPV4;
                else if (eg_md.ethertype == ETHERTYPE_IPV6) hdr.l3tp2.ppptyp = PPPTYPE_IPV6;
#ifdef HAVE_SGT
                else if (eg_md.ethertype == ETHERTYPE_SGT) hdr.l3tp2.ppptyp = PPPTYPE_SGT;
#endif
#ifdef HAVE_NSH
                else if (eg_md.ethertype == ETHERTYPE_NSH) hdr.l3tp2.ppptyp = PPPTYPE_NSH;
#endif
#ifdef HAVE_MPLS
                else if (eg_md.ethertype == ETHERTYPE_MPLS_UCAST) hdr.l3tp2.ppptyp = PPPTYPE_MPLS_UCAST;
#endif
#ifdef HAVE_TAP
                else if (eg_md.ethertype == ETHERTYPE_ROUTEDMAC) hdr.l3tp2.ppptyp = PPPTYPE_ROUTEDMAC;
#endif

                if (hdr.ipv4d.isValid()) eg_md.ethertype = ETHERTYPE_IPV4;
                else if (hdr.ipv6d.isValid()) eg_md.ethertype = ETHERTYPE_IPV6;
            }
#endif
#ifdef HAVE_IPIP
            if (hdr.ipv4d.isValid() && (hdr.ipv4d.protocol == 0)) {
                if (eg_md.ethertype == ETHERTYPE_IPV4) hdr.ipv4d.protocol = IP_PROTOCOL_IPV4;
                else if (eg_md.ethertype == ETHERTYPE_IPV6) hdr.ipv4d.protocol = IP_PROTOCOL_IPV6;
#ifdef HAVE_SGT
                else if (eg_md.ethertype == ETHERTYPE_SGT) hdr.ipv4d.protocol = IP_PROTOCOL_SKIP;
#endif
#ifdef HAVE_NSH
                else if (eg_md.ethertype == ETHERTYPE_NSH) hdr.ipv4d.protocol = IP_PROTOCOL_NSH;
#endif
#ifdef HAVE_MPLS
                else if (eg_md.ethertype == ETHERTYPE_MPLS_UCAST) hdr.ipv4d.protocol = IP_PROTOCOL_MPLS_IN_IP;
#endif
#ifdef HAVE_TAP
                else if (eg_md.ethertype == ETHERTYPE_ROUTEDMAC) hdr.ipv4d.protocol = IP_PROTOCOL_SRL2;
#endif
                eg_md.ethertype = ETHERTYPE_IPV4;
            } else if (hdr.ipv6d.isValid() && (hdr.ipv6d.next_hdr == 0)) {
                if (eg_md.ethertype == ETHERTYPE_IPV4) hdr.ipv6d.next_hdr = IP_PROTOCOL_IPV4;
                else if (eg_md.ethertype == ETHERTYPE_IPV6) hdr.ipv6d.next_hdr = IP_PROTOCOL_IPV6;
#ifdef HAVE_SGT
                else if (eg_md.ethertype == ETHERTYPE_SGT) hdr.ipv6d.next_hdr = IP_PROTOCOL_SKIP;
#endif
#ifdef HAVE_NSH
                else if (eg_md.ethertype == ETHERTYPE_NSH) hdr.ipv6d.next_hdr = IP_PROTOCOL_NSH;
#endif
#ifdef HAVE_MPLS
                else if (eg_md.ethertype == ETHERTYPE_MPLS_UCAST) hdr.ipv6d.next_hdr = IP_PROTOCOL_MPLS_IN_IP;
#endif
#ifdef HAVE_TAP
                else if (eg_md.ethertype == ETHERTYPE_ROUTEDMAC) hdr.ipv6d.next_hdr = IP_PROTOCOL_SRL2;
#endif
                eg_md.ethertype = ETHERTYPE_IPV6;
            }
#endif
#ifdef HAVE_TMUX
            if (hdr.tmux2.isValid()) {
                if (eg_md.ethertype == ETHERTYPE_IPV4) hdr.tmux2.proto = IP_PROTOCOL_IPV4;
                else if (eg_md.ethertype == ETHERTYPE_IPV6) hdr.tmux2.proto = IP_PROTOCOL_IPV6;
#ifdef HAVE_SGT
                else if (eg_md.ethertype == ETHERTYPE_SGT) hdr.tmux2.proto = IP_PROTOCOL_SKIP;
#endif
#ifdef HAVE_NSH
                else if (eg_md.ethertype == ETHERTYPE_NSH) hdr.tmux2.proto = IP_PROTOCOL_NSH;
#endif
#ifdef HAVE_MPLS
                else if (eg_md.ethertype == ETHERTYPE_MPLS_UCAST) hdr.tmux2.proto = IP_PROTOCOL_MPLS_IN_IP;
#endif
#ifdef HAVE_TAP
                else if (eg_md.ethertype == ETHERTYPE_ROUTEDMAC) hdr.tmux2.proto = IP_PROTOCOL_SRL2;
#endif

                hdr.tmux2.chksum = tmux_checksum.get({ hdr.tmux2.length, hdr.tmux2.proto });

                if (hdr.ipv4d.isValid()) eg_md.ethertype = ETHERTYPE_IPV4;
                else if (hdr.ipv6d.isValid()) eg_md.ethertype = ETHERTYPE_IPV6;
            }
#endif
        }
    }
}

#endif // _NEXTHOP_P4
