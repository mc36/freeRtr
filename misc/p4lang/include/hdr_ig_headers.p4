#ifndef _HEADERS_P4_
#define _HEADERS_P4_

struct headers {
   pkt_out_header_t	pkt_out;
   pkt_in_header_t	pkt_in; 
   ethernet_t		ethernet;
   vlan_t               vlan;  
   mpls_t		mpls0;
   mpls_t		mpls1;
   ipv4_t		ipv4c;
   ipv6_t		ipv6c;
   ethernet_t		eth2;
   arp_t		arp; 
   llc_t                llc; 
   ipv4_t		ipv4;
   ipv6_t		ipv6;
   ethernet_t		eth3;
   ipv4_t		ipv4b;
   ipv6_t		ipv6b;
   udp_t		udp;
   tcp_t		tcp;
}

#endif // _HEADERS_P4_
