description ospf with mpolka

addrouter r1
int eth1 eth 0000.0000.1111 $1a$ $1b$
!
vrf def v1
 rd 1:1
 exit
router ospf4 1
 vrf v1
 router 4.4.4.1
 segrout 10
 area 0 ena
 area 0 segrout
 red conn
 exit
router ospf6 1
 vrf v1
 router 6.6.6.1
 segrout 10
 area 0 ena
 area 0 segrout
 red conn
 exit
int lo1
 vrf for v1
 ipv4 addr 2.2.2.1 255.255.255.255
 ipv6 addr 4321::1 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
 router ospf4 1 ena
 router ospf4 1 segrout index 1
 router ospf4 1 segrout node
 router ospf6 1 ena
 router ospf6 1 segrout index 1
 router ospf6 1 segrout node
 exit
int eth1
 vrf for v1
 ipv4 addr 1.1.1.1 255.255.255.252
 ipv6 addr 1234:1::1 ffff:ffff::
 mpls enable
 mpolka enable 1 65536 10
 router ospf4 1 ena
 router ospf6 1 ena
 exit
interface tun1
 tunnel vrf v1
 tunnel source loopback1
 tunnel destination 2.2.2.3
 tunnel domain-name 2.2.2.2 2.2.2.3 , 2.2.2.3 2.2.2.3
 tunnel mode mpolka
 vrf forwarding v1
 ipv4 address 3.3.3.1 255.255.255.252
 exit
interface tun2
 tunnel vrf v1
 tunnel source loopback1
 tunnel destination 4321::3
 tunnel domain-name 4321::2 4321::3 , 4321::3 4321::3
 tunnel mode mpolka
 vrf forwarding v1
 ipv6 address 3333::1 ffff::
 exit
!

addrouter r2
int eth1 eth 0000.0000.2222 $1b$ $1a$
int eth2 eth 0000.0000.2222 $2a$ $2b$
!
vrf def v1
 rd 1:1
 exit
router ospf4 1
 vrf v1
 router 4.4.4.2
 segrout 10
 area 0 ena
 area 0 segrout
 red conn
 exit
router ospf6 1
 vrf v1
 router 6.6.6.2
 segrout 10
 area 0 ena
 area 0 segrout
 red conn
 exit
int lo1
 vrf for v1
 ipv4 addr 2.2.2.2 255.255.255.255
 ipv6 addr 4321::2 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
 router ospf4 1 ena
 router ospf4 1 segrout index 2
 router ospf4 1 segrout node
 router ospf6 1 ena
 router ospf6 1 segrout index 2
 router ospf6 1 segrout node
 exit
int eth1
 vrf for v1
 ipv4 addr 1.1.1.2 255.255.255.252
 ipv6 addr 1234:1::2 ffff:ffff::
 mpls enable
 mpolka enable 2 65536 10
 router ospf4 1 ena
 router ospf6 1 ena
 exit
int eth2
 vrf for v1
 ipv4 addr 1.1.1.5 255.255.255.252
 ipv6 addr 1234:2::1 ffff:ffff::
 mpls enable
 mpolka enable 2 65536 10
 router ospf4 1 ena
 router ospf6 1 ena
 exit
!

addrouter r3
int eth1 eth 0000.0000.3333 $2b$ $2a$
!
vrf def v1
 rd 1:1
 exit
router ospf4 1
 vrf v1
 router 4.4.4.3
 segrout 10
 area 0 ena
 area 0 segrout
 red conn
 exit
router ospf6 1
 vrf v1
 router 6.6.6.3
 segrout 10
 area 0 ena
 area 0 segrout
 red conn
 exit
int lo1
 vrf for v1
 ipv4 addr 2.2.2.3 255.255.255.255
 ipv6 addr 4321::3 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
 router ospf4 1 ena
 router ospf4 1 segrout index 3
 router ospf4 1 segrout node
 router ospf6 1 ena
 router ospf6 1 segrout index 3
 router ospf6 1 segrout node
 exit
int eth1
 vrf for v1
 ipv4 addr 1.1.1.6 255.255.255.252
 ipv6 addr 1234:2::2 ffff:ffff::
 mpls enable
 mpolka enable 3 65536 10
 router ospf4 1 ena
 router ospf6 1 ena
 exit
interface tun1
 tunnel vrf v1
 tunnel source loopback1
 tunnel destination 2.2.2.1
 tunnel domain-name 2.2.2.2 2.2.2.1 , 2.2.2.1 2.2.2.1
 tunnel mode mpolka
 vrf forwarding v1
 ipv4 address 3.3.3.2 255.255.255.252
 exit
interface tun2
 tunnel vrf v1
 tunnel source loopback1
 tunnel destination 4321::1
 tunnel domain-name 4321::2 4321::1 , 4321::1 4321::1
 tunnel mode mpolka
 vrf forwarding v1
 ipv6 address 3333::2 ffff::
 exit
!



r1 tping 100 40 2.2.2.2 vrf v1 int lo1
r1 tping 100 40 2.2.2.3 vrf v1 int lo1
r1 tping 100 40 4321::2 vrf v1 int lo1
r1 tping 100 40 4321::3 vrf v1 int lo1

r2 tping 100 40 2.2.2.1 vrf v1 int lo1
r2 tping 100 40 2.2.2.3 vrf v1 int lo1
r2 tping 100 40 4321::1 vrf v1 int lo1
r2 tping 100 40 4321::3 vrf v1 int lo1

r3 tping 100 40 2.2.2.1 vrf v1 int lo1
r3 tping 100 40 2.2.2.2 vrf v1 int lo1
r3 tping 100 40 4321::1 vrf v1 int lo1
r3 tping 100 40 4321::2 vrf v1 int lo1

r1 tping 100 20 3.3.3.2 vrf v1 int tun1
r3 tping 100 20 3.3.3.1 vrf v1 int tun1

r1 tping 100 20 3333::2 vrf v1 int tun2
r3 tping 100 20 3333::1 vrf v1 int tun2

r2 output show ipv4 ospf 1 nei
r2 output show ipv6 ospf 1 nei
r2 output show ipv4 ospf 1 dat 0
r2 output show ipv6 ospf 1 dat 0
r2 output show ipv4 ospf 1 tre 0
r2 output show ipv6 ospf 1 tre 0
r2 output show ipv4 route v1
r2 output show ipv6 route v1
r2 output show ipv4 segrou v1
r2 output show ipv6 segrou v1
