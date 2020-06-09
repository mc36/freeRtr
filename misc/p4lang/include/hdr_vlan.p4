#ifndef _VLAN_P4_
#define _VLAN_P4_

/*                                             
 * VLAN header: as a header type, order matters
 */                                            
 header vlan_t {        
     bit<3> pcp;          
     bit<1> cfi;          
     vlan_id_t vid;          
     ethertype_t ethertype;   
 }                          

#endif // _VLAN_P4_
