#ifdef PROFILE_TOR
#define HAVE_VXLAN
#define HAVE_TUN
#define HAVE_BRIDGE
##define HAVE_LOCONN
##define HAVE_COPP
##define HAVE_INACL
##define HAVE_INQOS
##define HAVE_OUTACL
##define HAVE_OUTQOS
##define HAVE_PBR

#define PORT_TABLE_SIZE                13312

#define BUNDLE_TABLE_SIZE                      128

#define VLAN_TABLE_SIZE                        512

#define IPV4_LPM_TABLE_SIZE            20480

#define IPV6_LPM_TABLE_SIZE            5120

#define IPV4_HOST_TABLE_SIZE                   256
#define IPV6_HOST_TABLE_SIZE                   256

#define NEXTHOP_TABLE_SIZE             3328


#define IPV4_TUNNEL_TABLE_SIZE         3328
#define IPV6_TUNNEL_TABLE_SIZE         1664

#define MAC_TABLE_SIZE                 13312
#define _TABLE_SIZE_P4_
#endif
