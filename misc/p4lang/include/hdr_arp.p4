#ifndef _ARP_P4_
#define _ARP_P4_

/*                                             
 * ICMP header: as a header type, order matters
 */                                            
header arp_t {                                 
    bit<16> hrd;                               
    bit<16> pro;                               
    bit<8> hln;                                
    bit<8> pln;                                
    bit<16> op;                                
    mac_addr_t sha;                            
    ipv4_addr_t tpa;                           
    mac_addr_t tha;                            
    ipv4_addr_t spa;                           
}                                              

#endif // _ARP_P4_
