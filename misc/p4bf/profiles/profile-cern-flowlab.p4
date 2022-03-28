#ifdef PROFILE_CERN_FLOWSPEC
#define HAVE_ALPM
#define HAVE_PBR

#define PORT_TABLE_SIZE                        512

#define BUNDLE_TABLE_SIZE                      128

#define VLAN_TABLE_SIZE                        512

#define IPV4_LPM_TABLE_SIZE            1536

#define IPV6_LPM_TABLE_SIZE            12288

#define IPV4_HOST_TABLE_SIZE                   256
#define IPV6_HOST_TABLE_SIZE                   256

#define NEXTHOP_TABLE_SIZE                     512

#define IPV4_PBRACL_TABLE_SIZE         384

#define IPV6_PBRACL_TABLE_SIZE         3072
#define _TABLE_SIZE_P4_
#endif
