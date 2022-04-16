description ppp with packet over dtls

addrouter r1
int ser1 ser - $1a$ $1b$
!
crypto rsakey rsa generate 2048
crypto dsakey dsa generate 1024
crypto ecdsakey ecdsa generate 256
crypto certificate dsa generate dsa dsa
crypto certificate rsa generate rsa rsa
crypto certificate ecdsa generate ecdsa ecdsa
vrf def v1
 rd 1:1
 exit
int ser1
 vrf for v1
 ipv4 addr 1.1.1.1 255.255.255.0
 ipv6 addr 1234::1 ffff::
 exit
int lo0
 vrf for v1
 ipv4 addr 4.4.4.4 255.255.255.255
 exit
ipv4 pool p4 2.2.2.1 0.0.0.1 254
int di1
 enc ppp
 vrf for v1
 ipv4 addr 2.2.2.0 255.255.255.255
 ppp ip4cp local 2.2.2.0
 ipv4 pool p4
 ppp ip4cp open
 exit
server pckodtls pou
 clone di1
 security rsakey rsa
 security dsakey dsa
 security ecdsakey ecdsa
 security rsacert rsa
 security dsacert dsa
 security ecdsacert ecdsa
 security protocol dtls
 vrf v1
 exit
server dns dns
 zone test.corp defttl 43200
 rr www.test.corp ip4a 1.1.1.1
 vrf v1
 exit
!

addrouter r2
int ser1 ser - $1b$ $1a$
!
vrf def v1
 rd 1:1
 exit
proxy-profile p1
 vrf v1
 security dtls
 exit
int ser1
 vrf for v1
 ipv4 addr 1.1.1.2 255.255.255.0
 ipv6 addr 1234::2 ffff::
 exit
prefix-list p1
 permit 0.0.0.0/0
 exit
int di1
 enc ppp
 vrf for v1
 ipv4 addr 3.3.3.3 255.255.255.128
 ppp ip4cp open
 ppp ip4cp local 0.0.0.0
 ipv4 gateway-prefix p1
 exit
vpdn pou
 int di1
 proxy p1
 tar www.test.corp
 vcid 2554
 prot pckodtls
 exit
proxy-profile p2
 vrf v1
 exit
client proxy p2
client name-server 1.1.1.1
!


r2 tping 100 60 2.2.2.0 vrf v1
r2 tping 100 5 4.4.4.4 vrf v1
