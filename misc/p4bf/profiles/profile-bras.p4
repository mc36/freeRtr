#ifdef PROFILE_BRAS
#define HAVE_ALPM
#define HAVE_L2TP
#define HAVE_TUN
##define HAVE_LOCONN
##define HAVE_MPLS
##define HAVE_FLOWSPEC
##define HAVE_COPP
##define HAVE_INACL
##define HAVE_INQOS
##define HAVE_OUTACL
##define HAVE_OUTQOS
##define HAVE_PBR

#define PORT_TABLE_SIZE                14336

#define BUNDLE_TABLE_SIZE                      128

#define VLAN_TABLE_SIZE                        512

#define IPV4_LPM_TABLE_SIZE            163840

#define IPV6_LPM_TABLE_SIZE            40960

#define IPV4_HOST_TABLE_SIZE                   256
#define IPV6_HOST_TABLE_SIZE                   256

#define NEXTHOP_TABLE_SIZE             14336


#define IPV4_TUNNEL_TABLE_SIZE         14336
#define IPV6_TUNNEL_TABLE_SIZE         7168
#define _TABLE_SIZE_P4_
#endif
