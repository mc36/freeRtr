description nat64 translation

addrouter r1
int eth1 eth 0000.0000.1111 $1a$ $1b$
!
vrf def v1
 rd 1:1
 exit
int eth1
 vrf for v1
 ipv4 addr 1.1.1.1 255.255.255.252
 exit
int lo1
 vrf for v1
 ipv4 addr 2.2.2.2 255.255.255.255
 exit
ipv4 route v1 0.0.0.0 0.0.0.0 1.1.1.2
!

addrouter r2
int eth1 eth 0000.0000.2222 $1b$ $1a$
int eth2 eth 0000.0000.2222 $2a$ $2b$
!
vrf def v1
 rd 1:1
 exit
int eth1
 vrf for v1
 ipv4 addr 1.1.1.2 255.255.255.252
 exit
int eth2
 vrf for v1
 ipv6 addr 1234::1 ffff:ffff::
 exit
ipv4 route v1 0.0.0.0 0.0.0.0 1.1.1.1
ipv6 route v1 :: :: 1234::2
access-list nat
 deny all fe80:: ffff:: all any all
 deny all any all fe80:: ffff:: all
 deny all any all ff00:: ff00:: all
 deny all 6464:: ffff:ffff:ffff:ffff:: all 6464:: ffff:ffff:ffff:ffff:: all
 perm all any all 6464:: ffff:ffff:ffff:ffff:: all
 exit
int tun1
 tun key 96
 tun vrf v1
 tun sou eth2
 tun des 6464::a01:4042
 tun mod 6to4
 vrf forwarding v1
 ipv4 addr 10.1.64.65 255.255.255.252
 ipv6 addr 6464::a01:4042 ffff:ffff:ffff:ffff:ffff:ffff::
 exit
ipv6 nat v1 srclist nat int tun1
!

addrouter r3
int eth1 eth 0000.0000.3333 $2b$ $2a$
!
vrf def v1
 rd 1:1
 exit
int eth1
 vrf for v1
 ipv6 addr 1234::2 ffff:ffff::
 exit
int lo1
 vrf for v1
 ipv6 addr 8888::8 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
 exit
ipv6 route v1 :: :: 1234::1
!



r1 tping 100 5 1.1.1.2 vrf v1
r3 tping 100 5 1234::1 vrf v1
r3 tping 100 5 6464::0202:0202 vrf v1 sou lo1
