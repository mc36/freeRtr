#ifndef _HEADERS_P4_
#define _HEADERS_P4_

struct headers {
    cpu_header_t	cpu;
    ethernet_t		ethernet;
    vlan_t              vlan;
    pppoe_t		pppoeC;
    pppoe_t		pppoeD;
    pppbr_t		pppoeB;
    ethernet_t		eth6;
    ipv4_t		ipv4d;
    ipv6_t		ipv6d;
    gre_t		gre2;
    udp_t		udp2;
    l2tp_t		l2tp2;
    pppbr_t		pppbr;
    ethernet_t		eth4;
    mpls_t		mpls0;
    mpls_t		mpls1;
    ipv4_t		ipv4c;
    ipv6_t		ipv6c;
    ethernet_t		eth2;
    arp_t		arp;
    llc_t               llc;
    ipv4_t		ipv4;
    ipv6_t		ipv6;
    ethernet_t		eth3;
    ipv4_t		ipv4b;
    ipv6_t		ipv6b;
    gre_t		gre;
    udp_t		udp;
    l2tp_t		l2tp;
    tcp_t		tcp;
    pppbr_t		l2tpbr;
    ethernet_t		eth5;
}

#endif // _HEADERS_P4_
