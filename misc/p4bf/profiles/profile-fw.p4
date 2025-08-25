#ifdef PROFILE_FW
#define HAVE_INACL
#define HAVE_OUTACL
#define HAVE_RACL
#define HAVE_INSPECT
#define HAVE_SGT
##define HAVE_LOCONN
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

#define IPV4_LPM_TABLE_SIZE            4096

#define IPV6_LPM_TABLE_SIZE            1024

#define IPV4_HOST_TABLE_SIZE                   256
#define IPV6_HOST_TABLE_SIZE                   256

#define NEXTHOP_TABLE_SIZE                     512


#define IPV4_INACL_TABLE_SIZE          256
#define IPV6_INACL_TABLE_SIZE          64
#define IPV4_OUTACL_TABLE_SIZE         256
#define IPV6_OUTACL_TABLE_SIZE         64

#define IPV4_ININSP_TABLE_SIZE         65536
#define IPV6_ININSP_TABLE_SIZE         16384
#define IPV4_OUTINSP_TABLE_SIZE        65536
#define IPV6_OUTINSP_TABLE_SIZE        16384
#define _TABLE_SIZE_P4_
#endif
