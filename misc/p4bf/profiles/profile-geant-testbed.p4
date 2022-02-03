#ifdef PROFILE_GEANT_TESTBED
#define HAVE_MPLS
#define HAVE_BIER
#define HAVE_POLKA
#define HAVE_PBR

#define PORT_TABLE_SIZE                        512

#define BUNDLE_TABLE_SIZE                      128

#define VLAN_TABLE_SIZE                        512

#define IPV4_LPM_TABLE_SIZE            8192

#define IPV6_LPM_TABLE_SIZE            2048

#define IPV4_HOST_TABLE_SIZE                   256
#define IPV6_HOST_TABLE_SIZE                   256

#define NEXTHOP_TABLE_SIZE                     512


#define MPLS_TABLE_SIZE                110592

#define POLKA_TABLE_SIZE               6912

#define IPV4_MCAST_TABLE_SIZE          3456
#define IPV6_MCAST_TABLE_SIZE          3456

#define IPV4_PBRACL_TABLE_SIZE         1024
#define IPV6_PBRACL_TABLE_SIZE         512
#define _TABLE_SIZE_P4_
#endif
