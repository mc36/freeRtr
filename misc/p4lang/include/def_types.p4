#ifndef _TYPES_P4_
#define _TYPES_P4_

typedef bit<16> ethertype_t;
typedef bit<48> mac_addr_t; 
typedef bit<20> label_t;
typedef bit<32> ipv4_addr_t;
typedef bit<128> ipv6_addr_t;
typedef bit<12> vlan_id_t;
typedef bit<16> switch_vrf_t;
typedef bit<9> PortId_t;
typedef bit<16> NextHopId_t;
typedef bit<10> SubIntId_t;

#endif // _TYPES_P4_
