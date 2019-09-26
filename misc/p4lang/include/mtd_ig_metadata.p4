#ifndef _INGRESS_METADATA_P4_
#define _INGRESS_METADATA_P4_

/*                           
 * User defined metadata type
 */                          
struct ingress_metadata_t {          
   PortId_t source_id;
   PortId_t target_id;
   PortId_t nexthop_id;
   PortId_t bridge_id;
   PortId_t bridge_src;
   PortId_t bridge_trg;
   ethertype_t ethertype;
   switch_vrf_t vrf;
   label_t mpls_label;
    bit<3>  mpls_op_type;
    bit<1>  mpls0_remove;
    bit<1>  mpls1_remove;
    bit<1>  mpls0_valid;
    bit<1>  mpls1_valid;
    bit<1>  llc_valid;
    bit<1>  arp_valid;
    bit<1>  ipv4_valid;
    bit<1>  ipv6_valid;
}                            

#endif // _INGRESS_METADATA_P4_
