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

#ifdef NEED_REPLICA

control EgressControlMcast(inout headers hdr, inout ingress_metadata_t eg_md,
                           in egress_intrinsic_metadata_t eg_intr_md,
                           inout egress_intrinsic_metadata_for_deparser_t eg_dprsr_md)
{

#ifdef HAVE_MCAST
    action act_rawip(mac_addr_t dst_mac_addr, mac_addr_t src_mac_addr) {
        hdr.ethernet.src_mac_addr = src_mac_addr;
        hdr.ethernet.dst_mac_addr = dst_mac_addr;
        eg_md.target_id = (SubIntId_t)eg_intr_md.egress_rid;
    }
#endif

#ifdef HAVE_DUPLAB
    action act_duplab(NextHopId_t hop, label_t label) {
        hdr.mpls0.label = label;
        eg_md.nexthop_id = hop;
    }
#endif

    action act_drop() {
        eg_dprsr_md.drop_ctl = 1;
    }


#ifdef HAVE_DUPLAB
#ifdef HAVE_MCAST
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
        //eg_md.need_recir = 1;
        hdr.mpls0.setInvalid();
        hdr.mpls1.setInvalid();
        hdr.cpu.setValid();
        hdr.cpu.port = hdr.internal.source_id;
        eg_md.ethertype = ETHERTYPE_IPV4;
    }

    action act_decap_mpls_ipv6() {
        //eg_md.need_recir = 1;
        hdr.mpls0.setInvalid();
        hdr.mpls1.setInvalid();
        hdr.cpu.setValid();
        hdr.cpu.port = hdr.internal.source_id;
        eg_md.ethertype = ETHERTYPE_IPV6;
    }
#endif
#endif




    table tbl_mcast {
        key = {
hdr.internal.clone_session:
            exact;
eg_intr_md.egress_rid:
            exact;
        }
        actions = {
#ifdef HAVE_MCAST
            act_rawip;
#endif
#ifdef HAVE_DUPLAB
            act_duplab;
#endif
#ifdef HAVE_DUPLAB
#ifdef HAVE_MCAST
            act_decap_mpls_ipv4;
            act_decap_mpls_ipv6;
            act_encap_ipv4_mpls;
            act_encap_ipv6_mpls;
#endif
#endif
            act_drop;
        }
        size = IPV4_MCAST_TABLE_SIZE + IPV6_MCAST_TABLE_SIZE;
        const default_action = act_drop();
    }


    apply {

        if (hdr.internal.reason == INTREAS_MCAST) {
            tbl_mcast.apply();
            if (eg_intr_md.egress_rid_first == 0) {
                eg_dprsr_md.drop_ctl = 1;
            }
        }

    }


}

#endif

#endif // _EG_CTL_MCAST_P4_
