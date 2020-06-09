#ifndef _ETHERNET_P4_
#define _ETHERNET_P4_

/*                                                  
 * Ethernet header: as a header type, order matters 
 */                                                 
header ethernet_t {                                 
   mac_addr_t   dst_mac_addr;                       
   mac_addr_t   src_mac_addr;                       
   ethertype_t  ethertype;                          
}                                                   

#endif // _ETHERNET_P4_
