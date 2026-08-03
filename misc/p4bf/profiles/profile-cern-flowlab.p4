#ifdef PROFILE_CERN_FLOWLAB
#define HAVE_ALPM
#define HAVE_PBR
#define HAVE_INACL
#define HAVE_OUTACL

#define PORT_TABLE_SIZE                        512

#define BUNDLE_TABLE_SIZE                      128

#define VLAN_TABLE_SIZE                        512

#define IPV4_LPM_TABLE_SIZE            512

#define IPV6_LPM_TABLE_SIZE            4096

#define IPV4_HOST_TABLE_SIZE                   256
#define IPV6_HOST_TABLE_SIZE                   256

#define NEXTHOP_TABLE_SIZE                     512

#define IPV4_PBRACL_TABLE_SIZE         128

#define IPV6_PBRACL_TABLE_SIZE         1024

#define IPV4_INACL_TABLE_SIZE          128

#define IPV6_INACL_TABLE_SIZE          1024

#define IPV4_OUTACL_TABLE_SIZE         128

#define IPV6_OUTACL_TABLE_SIZE         1024
#define _TABLE_SIZE_P4_
#endif
