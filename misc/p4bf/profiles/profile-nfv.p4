#ifdef PROFILE_NFV
#define HAVE_ALPM
#define HAVE_NSH
##define HAVE_MCAST
##define HAVE_FLOWSPEC
##define HAVE_BRIDGE
##define HAVE_COPP
##define HAVE_INACL
##define HAVE_INQOS
##define HAVE_OUTACL
##define HAVE_OUTQOS
##define HAVE_PBR

#define PORT_TABLE_SIZE                        512

#define BUNDLE_TABLE_SIZE                      128

#define VLAN_TABLE_SIZE                        512

#define IPV4_LPM_TABLE_SIZE            172032

#define IPV6_LPM_TABLE_SIZE            43008

#define IPV4_HOST_TABLE_SIZE                   256
#define IPV6_HOST_TABLE_SIZE                   256

#define NEXTHOP_TABLE_SIZE                     512


#define NSH_TABLE_SIZE                 28672
#define _TABLE_SIZE_P4_
#endif
