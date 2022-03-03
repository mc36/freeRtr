#include <openssl/conf.h>
#if OPENSSL_VERSION_NUMBER >= 0x30000000L
#include <openssl/provider.h>
#endif
#include <openssl/evp.h>
#include <openssl/rand.h>
#include <openssl/err.h>


#define maxPorts 128

#ifdef basicLoop
#define capabilities "nothing"
#else
#define capabilities "copp acl nat vlan bundle bridge pppoe hairpin gre l2tp route mpls vpls evpn eompls gretap pppoetap l2tptap vxlan ipip macsec ipsec pckoudp openvpn wireguard srv6 pbr qos flwspc mroute duplab bier amt nsh polka racl inspect mpolka sgt vrfysrc gtp"
#endif

#define platformBase "p4emu/openssl/"
