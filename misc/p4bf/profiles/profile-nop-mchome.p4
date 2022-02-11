#ifdef PROFILE_NOP_MCHOME
#define HAVE_MPLS
#define HAVE_DUPLAB
#define HAVE_BIER
#define HAVE_POLKA

#define PORT_TABLE_SIZE                        512

#define BUNDLE_TABLE_SIZE                      128

#define VLAN_TABLE_SIZE                        512

#define IPV4_LPM_TABLE_SIZE            8188

#define IPV6_LPM_TABLE_SIZE            2042

#define IPV4_HOST_TABLE_SIZE                   256
#define IPV6_HOST_TABLE_SIZE                   256

#define NEXTHOP_TABLE_SIZE                     512


#define MPLS_TABLE_SIZE                64304

#define POLKA_TABLE_SIZE               4144

#define IPV4_MCAST_TABLE_SIZE          3072
#define IPV6_MCAST_TABLE_SIZE          3072
#define _TABLE_SIZE_P4_
#endif
