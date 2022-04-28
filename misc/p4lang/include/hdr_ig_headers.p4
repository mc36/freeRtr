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

#ifndef _HEADERS_P4_
#define _HEADERS_P4_

struct headers {
    cpu_header_t	cpu;
    ethernet_t		ethernet;
    vlan_t		vlan;
    pppoe_t		pppoeC;
    pppoe_t		pppoeD;
    pppbr_t		pppoeB;
    ethernet_t		eth6;
    ipv4_t		ipv4d;
    ipv6_t		ipv6d;
    gre_t		gre2;
    udp_t		udp2;
    vxlan_t		vxlan2;
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
    gre_t		gre;
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
