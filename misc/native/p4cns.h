#include <openssl/conf.h>
//#include <openssl/provider.h>
#include <openssl/evp.h>
#include <openssl/rand.h>
#include <openssl/err.h>


#define maxPorts 128

#ifdef basicLoop
#define capabilities "nothing"
#else
#define capabilities "copp acl racl inspect nat vlan bundle bridge pppoe hairpin gre l2tp route mpls vpls evpn eompls gretap pppoetap l2tptap vxlan ipip macsec ipsec pckoudp openvpn wireguard srv6 pbr qos flwspc mroute duplab bier amt nsh polka sgt"
#endif

#define platformBase "p4emu/openssl/"
