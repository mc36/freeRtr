#ifdef PROFILE_KIFU_LNS
##define HAVE_ALPM
#define HAVE_L2TP
#define HAVE_TUN
#define HAVE_MPLS

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

#define IPV4_LPM_TABLE_SIZE            16384

#define IPV6_LPM_TABLE_SIZE            4096

#define IPV4_HOST_TABLE_SIZE                   256
#define IPV6_HOST_TABLE_SIZE                   256

#define NEXTHOP_TABLE_SIZE             24576

#define MPLS_TABLE_SIZE                24576

#define IPV4_TUNNEL_TABLE_SIZE         24576
#define IPV6_TUNNEL_TABLE_SIZE         12288
#define _TABLE_SIZE_P4_
#endif
