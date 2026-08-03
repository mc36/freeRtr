description rift default address suppression

addrouter r1
int eth1 eth 0000.0000.1111 $1a$ $1b$
!
vrf def v1
 rd 1:1
 exit
int lo1
 vrf for v1
 ipv4 addr 2.2.2.1 255.255.255.255
 ipv6 addr 4321::1 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
 exit
router rift4 1
 vrf v1
 router 41
 suppress
 justadv lo1
 exit
router rift6 1
 vrf v1
 router 61
 suppress
 justadv lo1
 exit
int lo2
 vrf for v1
 ipv4 addr 2.2.2.2 255.255.255.255
 ipv6 addr 4321::2 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
 router rift4 1 ena
 router rift4 1 passiv
 router rift6 1 ena
 router rift6 1 passiv
 exit
int lo3
 vrf for v1
 ipv4 addr 2.2.2.3 255.255.255.255
 ipv6 addr 4321::3 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
 router rift4 1 ena
 router rift4 1 passiv
 router rift6 1 ena
 router rift6 1 passiv
 exit
int eth1
 vrf for v1
 ipv4 addr 1.1.1.1 255.255.255.252
 ipv6 addr 1234:1::1 ffff:ffff::
 router rift4 1 ena
 router rift6 1 ena
 exit
!

addrouter r2
int eth1 eth 0000.0000.2222 $1b$ $1a$
!
vrf def v1
 rd 1:1
 exit
router rift4 1
 vrf v1
 router 42
 exit
router rift6 1
 vrf v1
 router 62
 exit
int eth1
 vrf for v1
 ipv4 addr 1.1.1.2 255.255.255.252
 ipv6 addr 1234:1::2 ffff:ffff::
 router rift4 1 ena
 router rift6 1 ena
 exit
!




r2 tping 100 40 2.2.2.1 vrf v1
r2 tping 0 40 2.2.2.2 vrf v1
r2 tping 0 40 2.2.2.3 vrf v1
r2 tping 100 40 4321::1 vrf v1
r2 tping 0 40 4321::2 vrf v1
r2 tping 0 40 4321::3 vrf v1

r2 output show ipv4 rift 1 nei
r2 output show ipv6 rift 1 nei
r2 output show ipv4 rift 1 dat
r2 output show ipv6 rift 1 dat
r2 output show ipv4 rift 1 tre n
r2 output show ipv6 rift 1 tre n
r2 output show ipv4 route v1
r2 output show ipv6 route v1
