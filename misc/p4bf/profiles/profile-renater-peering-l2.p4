#ifdef PROFILE_RENATER_PEERING_L2
#define HAVE_BRIDGE
#define HAVE_LOCONN

#define PORT_TABLE_SIZE                        512

#define BUNDLE_TABLE_SIZE                      128

#define VLAN_TABLE_SIZE                        512

#define IPV4_LPM_TABLE_SIZE            8192

#define IPV6_LPM_TABLE_SIZE            2048

#define IPV4_HOST_TABLE_SIZE                   256
#define IPV6_HOST_TABLE_SIZE                   256

#define NEXTHOP_TABLE_SIZE                     512


#define MAC_TABLE_SIZE                 71680

#define _TABLE_SIZE_P4_
#endif
