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

#ifndef _HEADERS_P4_
#define _HEADERS_P4_

struct headers {
    internal_header_t internal;
    cpu_header_t cpu;
    ethernet_t ethernet;
    vlan_t vlan;
    vlan_t vlanq;
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
#ifdef HAVE_TMUX
    tmux_t tmux2;
#endif
#ifdef HAVE_L3TP
    l3tp_t l3tp2;
#endif
#ifdef HAVE_ETHERIP
    etherip_t etherip2;
#endif
#ifdef HAVE_EOIP
    eoip_t eoip2;
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
#ifdef HAVE_MPLS
#ifdef HAVE_BRIDGE
    mpls_t mpls90;
    mpls_t mpls91;
    ethernet_t eth9;
#endif
#endif
#ifdef HAVE_MPLS
    mpls_t mpls89;
    mpls_t mpls88;
    mpls_t mpls87;
    mpls_t mpls86;
    mpls_t mpls85;
    mpls_t mpls84;
    mpls_t mpls83;
    mpls_t mpls82;
    mpls_t mpls81;
    mpls_t mpls80;
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
    mpls_t mpls8;
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
#ifdef HAVE_TMUX
    tmux_t tmux;
#endif
#ifdef HAVE_L3TP
    l3tp_t l3tp;
#endif
#ifdef HAVE_ETHERIP
    etherip_t etherip;
#endif
#ifdef HAVE_EOIP
    eoip_t eoip;
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
