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

#ifndef _EG_CTL_MCAST_P4_
#define _EG_CTL_MCAST_P4_


control EgressControlMcast(inout headers hdr,
                           inout ingress_metadata_t eg_md,
                           inout standard_metadata_t eg_intr_md) {


    action act_drop() {
        eg_md.dropping = 1;
    }

    action act_rawip(mac_addr_t dst_mac_addr, mac_addr_t src_mac_addr) {
        hdr.ethernet.src_mac_addr = src_mac_addr;
        hdr.ethernet.dst_mac_addr = dst_mac_addr;
        eg_md.target_id = (SubIntId_t)eg_intr_md.egress_rid;
    }

    action act_neigh(NextHopId_t nhop) {
        eg_md.nexthop_id = nhop;
    }

    action act_duplab(NextHopId_t hop, label_t label) {
        hdr.mpls0.label = label;
        eg_md.nexthop_id = hop;
    }

    action act_encap_ipv4_mpls(NextHopId_t hop, label_t label) {
        hdr.mpls0.setValid();
        hdr.mpls0.label = label;
        hdr.mpls0.ttl = hdr.ipv4.ttl;
        hdr.mpls0.bos = 1;
        eg_md.ethertype = ETHERTYPE_MPLS_UCAST;
        eg_md.nexthop_id = hop;
    }

    action act_encap_ipv6_mpls(NextHopId_t hop, label_t label) {
        hdr.mpls0.setValid();
        hdr.mpls0.label = label;
        hdr.mpls0.ttl = hdr.ipv6.hop_limit;
        hdr.mpls0.bos = 1;
        eg_md.ethertype = ETHERTYPE_MPLS_UCAST;
        eg_md.nexthop_id = hop;
    }

    action act_decap_mpls_ipv4() {
        eg_md.need_recir = 1;
        hdr.mpls0.setInvalid();
        hdr.mpls1.setInvalid();
        hdr.cpu.setValid();
        hdr.cpu.port = eg_md.source_id;
        hdr.ethernet.ethertype = ETHERTYPE_IPV4;
    }

    action act_decap_mpls_ipv6() {
        eg_md.need_recir = 1;
        hdr.mpls0.setInvalid();
        hdr.mpls1.setInvalid();
        hdr.cpu.setValid();
        hdr.cpu.port = eg_md.source_id;
        hdr.ethernet.ethertype = ETHERTYPE_IPV6;
    }



    action and_bier_bs(bit<32>bs0, bit<32>bs1, bit<32>bs2, bit<32>bs3,
                       bit<32>bs4, bit<32>bs5, bit<32>bs6, bit<32>bs7) {
        hdr.bier.bs0 = hdr.bier.bs0 & bs0;
        hdr.bier.bs1 = hdr.bier.bs1 & bs1;
        hdr.bier.bs2 = hdr.bier.bs2 & bs2;
        hdr.bier.bs3 = hdr.bier.bs3 & bs3;
        hdr.bier.bs4 = hdr.bier.bs4 & bs4;
        hdr.bier.bs5 = hdr.bier.bs5 & bs5;
        hdr.bier.bs6 = hdr.bier.bs6 & bs6;
        hdr.bier.bs7 = hdr.bier.bs7 & bs7;
        if ((hdr.bier.bs0 | hdr.bier.bs1 | hdr.bier.bs2 | hdr.bier.bs3 |
                hdr.bier.bs4 | hdr.bier.bs5 | hdr.bier.bs6 | hdr.bier.bs7) != 0) return;
        eg_md.dropping = 1;
        eg_md.need_recir = 0;
    }

    action act_bier(NextHopId_t hop, label_t label, bit<32>bs0, bit<32>bs1, bit<32>bs2, bit<32>bs3,
                    bit<32>bs4, bit<32>bs5, bit<32>bs6, bit<32>bs7) {
        and_bier_bs(bs0, bs1, bs2, bs3, bs4, bs5, bs6, bs7);
        hdr.mpls0.label = label;
        eg_md.nexthop_id = hop;
    }

    action act_encap_ipv4_bier(NextHopId_t hop, label_t label, bit<16> bfir,
                               bit<32>bs0, bit<32>bs1, bit<32>bs2, bit<32>bs3,
                               bit<32>bs4, bit<32>bs5, bit<32>bs6, bit<32>bs7) {
        hdr.mpls0.setValid();
        hdr.mpls0.label = label;
        hdr.mpls0.ttl = hdr.ipv4.ttl;
        hdr.mpls0.bos = 1;
        hdr.bier.setValid();
        hdr.bier.idver = 0x50;
        hdr.bier.bsl = 3;
        hdr.bier.proto = 4;
        hdr.bier.bfir = bfir;
        hdr.bier.bs0 = bs0;
        hdr.bier.bs1 = bs1;
        hdr.bier.bs2 = bs2;
        hdr.bier.bs3 = bs3;
        hdr.bier.bs4 = bs4;
        hdr.bier.bs5 = bs5;
        hdr.bier.bs6 = bs6;
        hdr.bier.bs7 = bs7;
        eg_md.ethertype = ETHERTYPE_MPLS_UCAST;
        eg_md.nexthop_id = hop;
    }

    action act_encap_ipv6_bier(NextHopId_t hop, label_t label, bit<16> bfir,
                               bit<32>bs0, bit<32>bs1, bit<32>bs2, bit<32>bs3,
                               bit<32>bs4, bit<32>bs5, bit<32>bs6, bit<32>bs7) {
        hdr.mpls0.setValid();
        hdr.mpls0.label = label;
        hdr.mpls0.ttl = hdr.ipv6.hop_limit;
        hdr.mpls0.bos = 1;
        hdr.bier.setValid();
        hdr.bier.idver = 0x50;
        hdr.bier.bsl = 3;
        hdr.bier.proto = 6;
        hdr.bier.bfir = bfir;
        hdr.bier.bs0 = bs0;
        hdr.bier.bs1 = bs1;
        hdr.bier.bs2 = bs2;
        hdr.bier.bs3 = bs3;
        hdr.bier.bs4 = bs4;
        hdr.bier.bs5 = bs5;
        hdr.bier.bs6 = bs6;
        hdr.bier.bs7 = bs7;
        eg_md.ethertype = ETHERTYPE_MPLS_UCAST;
        eg_md.nexthop_id = hop;
    }

    action act_encap_vpn4_bier(NextHopId_t hop, label_t label, label_t vpnlab, bit<16> bfir,
                               bit<32>bs0, bit<32>bs1, bit<32>bs2, bit<32>bs3,
                               bit<32>bs4, bit<32>bs5, bit<32>bs6, bit<32>bs7) {
        hdr.mpls0.setValid();
        hdr.mpls0.label = label;
        hdr.mpls0.ttl = hdr.ipv4.ttl;
        hdr.mpls0.bos = 1;
        hdr.bier.setValid();
        hdr.bier.idver = 0x50;
        hdr.bier.bsl = 3;
        hdr.bier.proto = 2;
        hdr.bier.bfir = bfir;
        hdr.bier.bs0 = bs0;
        hdr.bier.bs1 = bs1;
        hdr.bier.bs2 = bs2;
        hdr.bier.bs3 = bs3;
        hdr.bier.bs4 = bs4;
        hdr.bier.bs5 = bs5;
        hdr.bier.bs6 = bs6;
        hdr.bier.bs7 = bs7;
        hdr.mpls8.setValid();
        hdr.mpls8.label = vpnlab;
        hdr.mpls8.ttl = hdr.ipv4.ttl;
        hdr.mpls8.bos = 1;
        eg_md.ethertype = ETHERTYPE_MPLS_UCAST;
        eg_md.nexthop_id = hop;
    }

    action act_encap_vpn6_bier(NextHopId_t hop, label_t label, label_t vpnlab, bit<16> bfir,
                               bit<32>bs0, bit<32>bs1, bit<32>bs2, bit<32>bs3,
                               bit<32>bs4, bit<32>bs5, bit<32>bs6, bit<32>bs7) {
        hdr.mpls0.setValid();
        hdr.mpls0.label = label;
        hdr.mpls0.ttl = hdr.ipv6.hop_limit;
        hdr.mpls0.bos = 1;
        hdr.bier.setValid();
        hdr.bier.idver = 0x50;
        hdr.bier.bsl = 3;
        hdr.bier.proto = 2;
        hdr.bier.bfir = bfir;
        hdr.bier.bs0 = bs0;
        hdr.bier.bs1 = bs1;
        hdr.bier.bs2 = bs2;
        hdr.bier.bs3 = bs3;
        hdr.bier.bs4 = bs4;
        hdr.bier.bs5 = bs5;
        hdr.bier.bs6 = bs6;
        hdr.bier.bs7 = bs7;
        hdr.mpls8.setValid();
        hdr.mpls8.label = vpnlab;
        hdr.mpls8.ttl = hdr.ipv6.hop_limit;
        hdr.mpls8.bos = 1;
        eg_md.ethertype = ETHERTYPE_MPLS_UCAST;
        eg_md.nexthop_id = hop;
    }

    action act_decap_bier_ipv4(bit<32>bs0, bit<32>bs1, bit<32>bs2, bit<32>bs3,
                               bit<32>bs4, bit<32>bs5, bit<32>bs6, bit<32>bs7) {
        hdr.ethernet.ethertype = ETHERTYPE_IPV4;
        if (hdr.mpls8.isValid()) hdr.ethernet.ethertype = ETHERTYPE_MPLS_UCAST;
        eg_md.need_recir = 1;
        and_bier_bs(bs0, bs1, bs2, bs3, bs4, bs5, bs6, bs7);
        hdr.mpls0.setInvalid();
        hdr.mpls1.setInvalid();
        hdr.bier.setInvalid();
        hdr.cpu.setValid();
        hdr.cpu.port = eg_md.source_id;
    }

    action act_decap_bier_ipv6(bit<32>bs0, bit<32>bs1, bit<32>bs2, bit<32>bs3,
                               bit<32>bs4, bit<32>bs5, bit<32>bs6, bit<32>bs7) {
        hdr.ethernet.ethertype = ETHERTYPE_IPV6;
        if (hdr.mpls8.isValid()) hdr.ethernet.ethertype = ETHERTYPE_MPLS_UCAST;
        eg_md.need_recir = 1;
        and_bier_bs(bs0, bs1, bs2, bs3, bs4, bs5, bs6, bs7);
        hdr.mpls0.setInvalid();
        hdr.mpls1.setInvalid();
        hdr.bier.setInvalid();
        hdr.cpu.setValid();
        hdr.cpu.port = eg_md.source_id;
    }



    table tbl_mcast {
        key = {
eg_md.clone_session:
            exact;
eg_intr_md.egress_rid:
            exact;
        }
        actions = {
            act_drop;
            act_rawip;
            act_neigh;
            act_bier;
            act_decap_bier_ipv4;
            act_decap_bier_ipv6;
            act_encap_ipv4_bier;
            act_encap_ipv6_bier;
            act_encap_vpn4_bier;
            act_encap_vpn6_bier;
            act_duplab;
            act_decap_mpls_ipv4;
            act_decap_mpls_ipv6;
            act_encap_ipv4_mpls;
            act_encap_ipv6_mpls;
            @defaultonly NoAction;
        }
        size = IPV4_MCAST_TABLE_SIZE + IPV6_MCAST_TABLE_SIZE;
        const default_action = NoAction();
    }


    apply {

        tbl_mcast.apply();

    }


}


#endif // _EG_CTL_MCAST_P4_
