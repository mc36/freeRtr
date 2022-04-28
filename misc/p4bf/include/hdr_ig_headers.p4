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

#ifndef _HEADERS_P4_
#define _HEADERS_P4_

struct headers {
    internal_header_t internal;
    cpu_header_t cpu;
    ethernet_t ethernet;
    vlan_t vlan;
#ifdef HAVE_PPPOE
    pppoe_t pppoeC;
    pppoe_t pppoeD;
#endif
#ifdef HAVE_TAP
    ethernet_t eth6;
#endif
#ifdef HAVE_TUN
    ipv4_t ipv4d;
    ipv6_t ipv6d;
#endif
#ifdef HAVE_GRE
    gre_t gre2;
#endif
#ifdef NEED_UDP2
    udp_t udp2;
#endif
#ifdef HAVE_L2TP
    l2tp_t l2tp2;
#endif
#ifdef HAVE_GTP
    gtp_t gtp2;
#endif
#ifdef HAVE_VXLAN
    vxlan_t vxlan2;
#endif
#ifdef HAVE_POLKA
    polka_t polka;
#endif
#ifdef HAVE_SGT
    sgt_t sgt;
#endif
#ifdef HAVE_NSH
    nsh_t nsh;
#endif
#ifdef HAVE_MPLS
    mpls_t mpls0;
    mpls_t mpls1;
#endif
#ifdef HAVE_MPLS
#ifdef HAVE_BIER
    bier_t bier;
#endif
#endif
    ethernet_t eth2;
    vlan_t vlan2;
    arp_t arp;
    llc_t llc;
    ipv4_t ipv4;
    ipv6_t ipv6;
#ifdef HAVE_SRV6
    ipv4_t ipv4b;
    ipv6_t ipv6b;
#endif
#ifdef HAVE_GRE
    gre_t gre;
#endif
    tcp_t tcp;
    udp_t udp;
#ifdef HAVE_L2TP
    l2tp_t l2tp;
#endif
#ifdef HAVE_GTP
    gtp_t gtp;
#endif
#ifdef HAVE_VXLAN
    vxlan_t vxlan;
#endif
}

#endif	// _HEADERS_P4_
