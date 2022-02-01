##undef _TABLE_SIZE_P4_
#ifdef PROFILE_BNG
#define HAVE_ALPM
#define HAVE_PPPOE
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

#define IPV4_LPM_TABLE_SIZE            147456

#define IPV6_LPM_TABLE_SIZE            36864

#define IPV4_HOST_TABLE_SIZE                   256
#define IPV6_HOST_TABLE_SIZE                   256

#define NEXTHOP_TABLE_SIZE                     512


#define PPPOE_TABLE_SIZE               24576

#define _TABLE_SIZE_P4_
#endif
#ifdef PROFILE_BRAS
#define HAVE_ALPM
#define HAVE_L2TP
#define HAVE_TUN
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

#define IPV4_LPM_TABLE_SIZE            98304

#define IPV6_LPM_TABLE_SIZE            24576

#define IPV4_HOST_TABLE_SIZE                   256
#define IPV6_HOST_TABLE_SIZE                   256

#define NEXTHOP_TABLE_SIZE                     512


#define IPV4_TUNNEL_TABLE_SIZE         49152

#define IPV6_TUNNEL_TABLE_SIZE         24576
#define _TABLE_SIZE_P4_
#endif
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
#ifdef PROFILE_CLEANER
#define HAVE_ALPM
#define HAVE_FLOWSPEC
##define HAVE_COPP
##define HAVE_INACL
##define HAVE_INQOS
##define HAVE_OUTACL
##define HAVE_OUTQOS
##define HAVE_PBR

#define PORT_TABLE_SIZE                        512

#define BUNDLE_TABLE_SIZE                      128

#define VLAN_TABLE_SIZE                        512

#define IPV4_LPM_TABLE_SIZE            256

#define IPV6_LPM_TABLE_SIZE            64

#define IPV4_HOST_TABLE_SIZE                   256
#define IPV6_HOST_TABLE_SIZE                   256

#define NEXTHOP_TABLE_SIZE                     512


#define IPV4_FLOWSPEC_TABLE_SIZE       1024

#define IPV6_FLOWSPEC_TABLE_SIZE       1024
#define _TABLE_SIZE_P4_
#endif
#ifdef PROFILE_CPE
#define HAVE_ALPM
##define HAVE_MCAST
##define HAVE_PBR
##define HAVE_NAT
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

#define IPV4_LPM_TABLE_SIZE            196608

#define IPV6_LPM_TABLE_SIZE            49152

#define IPV4_HOST_TABLE_SIZE                   256
#define IPV6_HOST_TABLE_SIZE                   256

#define NEXTHOP_TABLE_SIZE                     512
#define _TABLE_SIZE_P4_
#endif
#ifdef PROFILE_GEANT_TESTBED
#define HAVE_MPLS
#define HAVE_BIER
#define HAVE_POLKA
#define HAVE_PBR

#define PORT_TABLE_SIZE                        512

#define BUNDLE_TABLE_SIZE                      128

#define VLAN_TABLE_SIZE                        512

#define IPV4_LPM_TABLE_SIZE            8192

#define IPV6_LPM_TABLE_SIZE            2048

#define IPV4_HOST_TABLE_SIZE                   256
#define IPV6_HOST_TABLE_SIZE                   256

#define NEXTHOP_TABLE_SIZE                     512


#define MPLS_TABLE_SIZE                32768

#define POLKA_TABLE_SIZE               2048

#define IPV4_MCAST_TABLE_SIZE          256

#define IPV6_MCAST_TABLE_SIZE          256

#define IPV4_PBRACL_TABLE_SIZE         1024

#define IPV6_PBRACL_TABLE_SIZE         512
#define _TABLE_SIZE_P4_
#endif
#ifdef PROFILE_GRE
#define HAVE_ALPM
#define HAVE_GRE
#define HAVE_TUN
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

#define IPV4_LPM_TABLE_SIZE            81920

#define IPV6_LPM_TABLE_SIZE            20480

#define IPV4_HOST_TABLE_SIZE                   256
#define IPV6_HOST_TABLE_SIZE                   256

#define NEXTHOP_TABLE_SIZE                     512


#define IPV4_TUNNEL_TABLE_SIZE         40960

#define IPV6_TUNNEL_TABLE_SIZE         20480
#define _TABLE_SIZE_P4_
#endif
#ifdef PROFILE_IPIP
#define HAVE_ALPM
#define HAVE_IPIP
#define HAVE_TUN
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

#define IPV4_LPM_TABLE_SIZE            81920

#define IPV6_LPM_TABLE_SIZE            20480

#define IPV4_HOST_TABLE_SIZE                   256
#define IPV6_HOST_TABLE_SIZE                   256

#define NEXTHOP_TABLE_SIZE                     512


#define IPV4_TUNNEL_TABLE_SIZE         40960

#define IPV6_TUNNEL_TABLE_SIZE         20480
#define _TABLE_SIZE_P4_
#endif
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

#define IPV4_LPM_TABLE_SIZE            24576

#define IPV6_LPM_TABLE_SIZE            6144

#define IPV4_HOST_TABLE_SIZE                   256
#define IPV6_HOST_TABLE_SIZE                   256

#define NEXTHOP_TABLE_SIZE                     512


#define NSH_TABLE_SIZE                 98304
#define _TABLE_SIZE_P4_
#endif
#ifdef PROFILE_NOP_MCHOME
#define HAVE_MPLS
#define HAVE_DUPLAB
#define HAVE_BIER
#define HAVE_POLKA

#define PORT_TABLE_SIZE                        512

#define BUNDLE_TABLE_SIZE                      128

#define VLAN_TABLE_SIZE                        512

#define IPV4_LPM_TABLE_SIZE            12288

#define IPV6_LPM_TABLE_SIZE            3072

#define IPV4_HOST_TABLE_SIZE                   256
#define IPV6_HOST_TABLE_SIZE                   256

#define NEXTHOP_TABLE_SIZE                     512


#define MPLS_TABLE_SIZE                49152

#define POLKA_TABLE_SIZE               3072

#define IPV4_MCAST_TABLE_SIZE          384

#define IPV6_MCAST_TABLE_SIZE          384
#define _TABLE_SIZE_P4_
#endif
#ifdef PROFILE_PE
#define HAVE_ALPM
#define HAVE_MPLS
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

#define IPV4_LPM_TABLE_SIZE            40960

#define IPV6_LPM_TABLE_SIZE            10240

#define IPV4_HOST_TABLE_SIZE                   256
#define IPV6_HOST_TABLE_SIZE                   256

#define NEXTHOP_TABLE_SIZE                     512


#define MPLS_TABLE_SIZE                163840
#define _TABLE_SIZE_P4_
#endif
#ifdef PROFILE_P
#define HAVE_ALPM
#define HAVE_MPLS
##define HAVE_DUPLAB
##HAVE_COPP
##HAVE_INACL
##HAVE_INQOS
##HAVE_OUTACL
##HAVE_OUTQOS
##define HAVE_PBR

#define PORT_TABLE_SIZE                        512

#define BUNDLE_TABLE_SIZE                      128

#define VLAN_TABLE_SIZE                        512

#define IPV4_LPM_TABLE_SIZE            40960

#define IPV6_LPM_TABLE_SIZE            10240

#define IPV4_HOST_TABLE_SIZE                   256
#define IPV6_HOST_TABLE_SIZE                   256

#define NEXTHOP_TABLE_SIZE                     512


#define MPLS_TABLE_SIZE                163840
#define _TABLE_SIZE_P4_
#endif
#ifdef PROFILE_RAWIP
#define HAVE_ALPM
##define HAVE_COPP
##define HAVE_INACL
##define HAVE_INQOS
##define HAVE_OUTACL
##define HAVE_OUTQOS
##define HAVE_PBR

#define PORT_TABLE_SIZE                        512

#define BUNDLE_TABLE_SIZE                      128

#define VLAN_TABLE_SIZE                        512

#define IPV4_LPM_TABLE_SIZE            196608

#define IPV6_LPM_TABLE_SIZE            49152

#define IPV4_HOST_TABLE_SIZE                   256
#define IPV6_HOST_TABLE_SIZE                   256

#define NEXTHOP_TABLE_SIZE                     512
#define _TABLE_SIZE_P4_
#endif
#ifdef PROFILE_RENATER_PEERING_L2
#define HAVE_BRIDGE
#define HAVE_INACL
#define HAVE_OUTACL

#define PORT_TABLE_SIZE                        512

#define BUNDLE_TABLE_SIZE                      128

#define VLAN_TABLE_SIZE                        512

#define IPV4_LPM_TABLE_SIZE            8192

#define IPV6_LPM_TABLE_SIZE            2048

#define IPV4_HOST_TABLE_SIZE                   256
#define IPV6_HOST_TABLE_SIZE                   256

#define NEXTHOP_TABLE_SIZE                     512


#define MAC_TABLE_SIZE                 32768

#define IPV4_INACL_TABLE_SIZE          2048

#define IPV6_INACL_TABLE_SIZE          1024

#define IPV4_OUTACL_TABLE_SIZE         2048

#define IPV6_OUTACL_TABLE_SIZE         1024
#define _TABLE_SIZE_P4_
#endif
#ifdef PROFILE_RENATER_PEERING_L3
#define HAVE_MPLS
#define HAVE_FLOWSPEC
#define HAVE_COPP
#define HAVE_INACL
#define HAVE_OUTACL

#define PORT_TABLE_SIZE                        512

#define BUNDLE_TABLE_SIZE                      128

#define VLAN_TABLE_SIZE                        512

#define IPV4_LPM_TABLE_SIZE            4096

#define IPV6_LPM_TABLE_SIZE            1024

#define IPV4_HOST_TABLE_SIZE                   256
#define IPV6_HOST_TABLE_SIZE                   256

#define NEXTHOP_TABLE_SIZE                     512


#define MPLS_TABLE_SIZE                16384

#define IPV4_INACL_TABLE_SIZE          1024
#define IPV6_INACL_TABLE_SIZE          512

#define IPV4_OUTACL_TABLE_SIZE         1024
#define IPV6_OUTACL_TABLE_SIZE         512

#define IPV4_FLOWSPEC_TABLE_SIZE       1024
#define IPV6_FLOWSPEC_TABLE_SIZE       512

#define IPV4_COPP_TABLE_SIZE                   512
#define IPV6_COPP_TABLE_SIZE                   256
#define _TABLE_SIZE_P4_
#endif
#ifdef PROFILE_SRV6
#define HAVE_SRV6
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

#define IPV4_LPM_TABLE_SIZE            2048

#define IPV6_LPM_TABLE_SIZE            8192

#define IPV4_HOST_TABLE_SIZE                   256
#define IPV6_HOST_TABLE_SIZE                   256

#define NEXTHOP_TABLE_SIZE                     512
#define _TABLE_SIZE_P4_
#endif
#ifdef PROFILE_TOR
#define HAVE_VXLAN
#define HAVE_TUN
#define HAVE_BRIDGE
##define HAVE_COPP
##define HAVE_INACL
##define HAVE_INQOS
##define HAVE_OUTACL
##define HAVE_OUTQOS
##define HAVE_PBR

#define PORT_TABLE_SIZE                        512

#define BUNDLE_TABLE_SIZE                      128

#define VLAN_TABLE_SIZE                        512

#define IPV4_LPM_TABLE_SIZE            14336

#define IPV6_LPM_TABLE_SIZE            3584

#define IPV4_HOST_TABLE_SIZE                   256
#define IPV6_HOST_TABLE_SIZE                   256

#define NEXTHOP_TABLE_SIZE                     512


#define IPV4_TUNNEL_TABLE_SIZE         7168

#define IPV6_TUNNEL_TABLE_SIZE         3584

#define MAC_TABLE_SIZE                 28672
#define _TABLE_SIZE_P4_
#endif
#ifdef PROFILE_WLC
#define HAVE_PCKOUDP
#define HAVE_TUN
#define HAVE_BRIDGE
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

#define NEXTHOP_TABLE_SIZE                     512


#define IPV4_TUNNEL_TABLE_SIZE         8192

#define IPV6_TUNNEL_TABLE_SIZE         4096

#define MAC_TABLE_SIZE                 32768
#define _TABLE_SIZE_P4_
#endif
