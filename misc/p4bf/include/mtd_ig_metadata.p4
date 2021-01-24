/*
 * Copyright 2019-present GÉANT RARE project
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

#ifndef _INGRESS_METADATA_P4_
#define _INGRESS_METADATA_P4_



#undef NEED_PKTLEN

#ifdef HAVE_TUN
#define NEED_PKTLEN
#endif

#ifdef HAVE_PPPOE
#define NEED_PKTLEN
#endif







/*
 * User defined metadata type
 */
struct ingress_metadata_t {

    SubIntId_t ingress_id;
    SubIntId_t source_id;
    SubIntId_t target_id;
    SubIntId_t aclport_id;
    NextHopId_t nexthop_id;
    SubIntId_t outport_id;
#ifdef HAVE_BRIDGE
    SubIntId_t bridge_id;
    SubIntId_t bridge_src;
    SubIntId_t bridge_trg;
#endif
    SubIntId_t output_id;
    ethertype_t ethertype;
    switch_vrf_t vrf;
#ifdef NEED_PKTLEN
    bit<16> pktlen;
#endif
#ifdef HAVE_MPLS
    label_t mpls_encap_egress_label;
    label_t mpls_encap_svc_label;
#endif
#ifdef HAVE_SRV6
    ipv6_addr_t srv_target;
#endif
    bit <3> mpls_op_type;
    bit <3> srv_op_type;
    bit <1> srv_remove;
    bit <1> saw_rsvp;
    bit <1> mpls0_remove;
    bit <1> mpls1_remove;
#ifdef HAVE_MCAST
    bit<16> clone_session;
    SubIntId_t rpf_iface;
#endif
#ifdef HAVE_INQOS
    SubIntId_t inqos_id;
    bit <8> inqos_res;
#endif
#ifdef HAVE_OUTQOS
    SubIntId_t outqos_id;
    bit <8> outqos_res;
#endif
#ifdef HAVE_FLOWSPEC
    SubIntId_t flowspec_id;
    bit <8> flowspec_res;
#endif
#ifdef HAVE_PPPOE
    bit<1>  pppoe_ctrl_valid;
    bit<1>  pppoe_data_valid;
#endif
#ifdef HAVE_MPLS
    bit <1> mpls0_valid;
    bit <1> mpls1_valid;
#endif
    bit <1> arp_valid;
    bit <1> ipv4_valid;
    bit <1> ipv6_valid;
    bit <16> layer4_srcprt;
    bit <16> layer4_dstprt;
    bit <1> srv_encap_l3vpn_valid;
    bit <1> mpls_encap_rawip_valid;
    bit <1> mpls_encap_l3vpn_valid;
    bit <1> mpls_encap_l2vpn_valid;
    bit <1> mpls_encap_xconnect_valid;
    bit <4> mpls_encap_decap_sap_type;	// service access point type
#ifdef HAVE_NAT
    bit<16> checksum_tcp_tmp;
    bit<16> checksum_udp_tmp;
#endif
    l4_lookup_t   l4_lookup;
    bit<16> always_zero;
}
#endif	// _INGRESS_METADATA_P4_
