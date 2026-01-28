description ipv4-ipv6 multicast translation

addrouter r1
int eth1 eth 0000.0000.1111 $1a$ $1b$
!
vrf def v1
 rd 1:1
 exit
int eth1
 vrf for v1
 ipv4 addr 1.1.1.1 255.255.255.252
 ipv4 multi static 232.2.2.2 1.1.1.1
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
 ipv6 addr 1234::101:106 ffff:ffff:ffff:ffff:ffff:ffff:ffff:fffc
 exit
int tun1
 tunnel vrf v1
 tunnel source eth1
 tunnel destination 1.1.1.1
 tunnel domain-name 1234::101:105 1.1.1.5 1234::101:101
 tunnel mode 4to6
 vrf for v1
 ipv4 addr 1.1.1.0 255.255.255.0
 ipv6 addr 1234::101:100 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ff00
 ipv4 multi static 232.2.2.2 1.1.1.1
 exit
ipv4 multi v1 join 232.2.2.2 1.1.1.1
!

addrouter r3
int eth1 eth 0000.0000.3333 $2b$ $2a$
!
vrf def v1
 rd 1:1
 exit
int eth1
 vrf for v1
 ipv6 addr 1234::101:105 ffff:ffff:ffff:ffff:ffff:ffff:ffff:fffc
 exit
ipv6 route v1 :: :: 1234::101:106
ipv6 mroute v1 :: :: 1234::101:106
ipv6 multi v1 join ff06::1 1234:1::1
!


r2 tping 100 5 1.1.1.1 vrf v1
r2 tping 100 5 1234::101:105 vrf v1
r1 tping 200 5 232.2.2.2 vrf v1 sou eth1 multi
