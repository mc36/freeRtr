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
