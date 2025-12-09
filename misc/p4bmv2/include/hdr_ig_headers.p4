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
    cpu_header_t	cpu;
    ethernet_t		ethernet;
    vlan_t		vlan;
    vlan_t		vlanq;
    pppoe_t		pppoeC;
    pppoe_t		pppoeD;
    pppbr_t		pppoeB;
    ethernet_t		eth6;
    ipv4_t		ipv4d;
    ipv6_t		ipv6d;
    l3tp_t		l3tp2;
    gre_t		gre2;
    tmux_t		tmux2;
    etherip_t		etherip2;
    eoip_t		eoip2;
    udp_t		udp2;
    vxlan_t		vxlan2;
    mpls_t		mpls90;
    mpls_t		mpls91;
    ethernet_t		eth9;
    mpls_t		mpls89;
    mpls_t		mpls88;
    mpls_t		mpls87;
    mpls_t		mpls86;
    mpls_t		mpls85;
    mpls_t		mpls84;
    mpls_t		mpls83;
    mpls_t		mpls82;
    mpls_t		mpls81;
    mpls_t		mpls80;
    l2tp_t		l2tp2;
    amt_t		amt2;
    gtp_t		gtp2;
    pppbr_t		pppbr;
    ethernet_t		eth4;
    polka_t		polka;
    sgt_t		sgt;
    nsh_t		nsh;
    mpls_t		mpls0;
    mpls_t		mpls1;
    bier_t		bier;
    mpls_t		mpls8;
    ipv4_t		ipv4c;
    ipv6_t		ipv6c;
    ethernet_t		eth2;
    vlan_t		vlan2;
    arp_t		arp;
    llc_t		llc;
    ipv4_t		ipv4;
    ipv6_t		ipv6;
    ethernet_t		eth3;
    ipv4_t		ipv4b;
    ipv6_t		ipv6b;
    l3tp_t		l3tp;
    gre_t		gre;
    tmux_t		tmux;
    etherip_t		etherip;
    eoip_t		eoip;
    udp_t		udp;
    l2tp_t		l2tp;
    amt_t		amt;
    gtp_t		gtp;
    vxlan_t		vxlan;
    tcp_t		tcp;
    pppbr_t		l2tpbr;
    ethernet_t		eth5;
}

#endif // _HEADERS_P4_
