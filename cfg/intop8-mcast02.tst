description interop8: pim

exit

addrouter r1
int eth1 eth 0000.0000.1111 $1a$ $1b$
!
vrf def v1
 rd 1:1
 exit
int eth1
 vrf for v1
 ipv4 addr 1.1.1.1 255.255.255.0
 ipv6 addr 1234:1::1 ffff:ffff:ffff:ffff::
 ipv4 pim ena
 ipv6 pim ena
 exit
ipv4 route v1 1.1.2.0 255.255.255.0 1.1.1.2
ipv6 route v1 4321::2 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff fe80::2
!

addother r2
int eth1 eth 0000.0000.2222 $1b$ $1a$
int eth2 eth 0000.0000.2222 $2b$ $2a$
!
ip forwarding
ipv6 forwarding
interface ens3
 ip address 1.1.1.2/24
 ipv6 address 1234:1::2/64
 ip pim
 ipv6 pim
 no shutdown
 exit
interface ens4
 ip address 1.1.2.2/24
 ipv6 address 1234:2::2/64
 ip pim
 ipv6 pim
 no shutdown
 exit
!

addrouter r3
int eth1 eth 0000.0000.3333 $2a$ $2b$
!
vrf def v1
 rd 1:1
 exit
int eth1
 vrf for v1
 ipv4 addr 1.1.2.1 255.255.255.0
 ipv6 addr 1234:2::1 ffff:ffff:ffff:ffff::
 ipv4 pim ena
 ipv6 pim ena
 exit
ipv4 route v1 1.1.1.0 255.255.255.0 1.1.2.2
ipv6 route v1 1234:1::1 ffff:ffff:ffff:ffff:: 1234:2::2
ipv4 mroute v1 1.1.1.0 255.255.255.0 1.1.2.2
ipv6 mroute v1 1234:1::1 ffff:ffff:ffff:ffff:: 1234:2::2
ipv4 multi v1 join 232.2.2.2 1.1.1.1
ipv6 multi v1 join ff06::1 1234:1::1
!


r1 tping 100 60 1.1.1.1 vrf v1 sou eth1
r1 tping 100 60 1234:1::1 vrf v1 sou eth1

r3 tping 100 60 1.1.2.1 vrf v1 sou eth1
r3 tping 100 60 1234:2::1 vrf v1 sou eth1

r1 tping 100 120 232.2.2.2 vrf v1 sou eth1
!r1 tping 100 120 ff06::1 vrf v1 sou eth1
