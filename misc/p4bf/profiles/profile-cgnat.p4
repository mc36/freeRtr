#ifdef PROFILE_CGNAT
#define HAVE_ALPM
#define HAVE_NAT
##define HAVE_MPLS
##define HAVE_FLOWSPEC
##define HAVE_COPP
##define HAVE_INACL
##define HAVE_INQOS
##define HAVE_OUTACL
##define HAVE_OUTQOS
##define HAVE_PBR

#define PORT_TABLE_SIZE                        512

#define BUNDLE_TABLE_SIZE                      128

#define VLAN_TABLE_SIZE                        512

#define IPV4_LPM_TABLE_SIZE            10240

#define IPV6_LPM_TABLE_SIZE            2560

#define IPV4_HOST_TABLE_SIZE                   256
#define IPV6_HOST_TABLE_SIZE                   256

#define NEXTHOP_TABLE_SIZE                     512


#define IPV4_NATTRNS_TABLE_SIZE        40960

#define IPV6_NATTRNS_TABLE_SIZE        20480

#define IPV4_NATACL_TABLE_SIZE         640

#define IPV6_NATACL_TABLE_SIZE         640
#define _TABLE_SIZE_P4_
#endif
