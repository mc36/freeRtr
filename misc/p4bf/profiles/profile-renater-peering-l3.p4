#ifdef PROFILE_RENATER_PEERING_L3
#define HAVE_MPLS
#define HAVE_COPP

#define PORT_TABLE_SIZE                        512

#define BUNDLE_TABLE_SIZE                      128

#define VLAN_TABLE_SIZE                        512

#define IPV4_LPM_TABLE_SIZE            20480

#define IPV6_LPM_TABLE_SIZE            5120

#define IPV4_HOST_TABLE_SIZE                   256
#define IPV6_HOST_TABLE_SIZE                   256

#define NEXTHOP_TABLE_SIZE                     512


#define MPLS_TABLE_SIZE                81920

#define IPV4_COPP_TABLE_SIZE                   512
#define IPV6_COPP_TABLE_SIZE                   256
#define _TABLE_SIZE_P4_
#endif
