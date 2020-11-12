description integrated isis multi-topology with sr

addrouter r1
int eth1 eth 0000.0000.1111 $1a$ $1b$
!
vrf def v1
 rd 1:1
 exit
access-list test4
 deny 1 any all any all
 permit all any all any all
 exit
access-list test6
 deny all 4321:: ffff:: all 4321:: ffff:: all
 permit all any all any all
 exit
router isis4 1
 vrf v1
 net 48.4444.0000.1111.00
 multi-topology
 is-type level2
 traffeng 4.4.4.1
 segrout 10
 both segrout
 red conn
 afi-other enable
 afi-other red conn
 exit
int lo1
 vrf for v1
 ipv4 addr 2.2.2.1 255.255.255.255
 ipv6 addr 4321::1 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
 router isis4 1 ena
 router isis4 1 other-ena
 router isis4 1 segrout index 1
 router isis4 1 segrout other-index 2
 router isis4 1 segrout node
 exit
int eth1
 vrf for v1
 ipv4 addr 1.1.1.1 255.255.255.252
 ipv4 access-group-in test4
 ipv6 addr 1234:1::1 ffff:ffff::
 ipv6 access-group-in test6
 mpls enable
 router isis4 1 ena
 router isis4 1 other-ena
 exit
!

addrouter r2
int eth1 eth 0000.0000.2222 $1b$ $1a$
int eth2 eth 0000.0000.2222 $2a$ $2b$
!
vrf def v1
 rd 1:1
 exit
access-list test4
 deny 1 any all any all
 permit all any all any all
 exit
access-list test6
 deny all 4321:: ffff:: all 4321:: ffff:: all
 permit all any all any all
 exit
router isis4 1
 vrf v1
 net 48.6666.0000.2222.00
 multi-topology
 is-type level2
 traffeng 6.6.6.2
 segrout 10
 both segrout
 red conn
 afi-other enable
 afi-other red conn
 exit
int lo1
 vrf for v1
 ipv4 addr 2.2.2.2 255.255.255.255
 ipv6 addr 4321::2 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
 router isis4 1 ena
 router isis4 1 other-ena
 router isis4 1 segrout index 3
 router isis4 1 segrout other-index 4
 router isis4 1 segrout node
 exit
int eth1
 vrf for v1
 ipv4 addr 1.1.1.2 255.255.255.252
 ipv4 access-group-in test4
 ipv6 addr 1234:1::2 ffff:ffff::
 ipv6 access-group-in test6
 mpls enable
 router isis4 1 ena
 router isis4 1 other-ena
 exit
int eth2
 vrf for v1
 ipv4 addr 1.1.1.5 255.255.255.252
 ipv4 access-group-in test4
 ipv6 addr 1234:2::1 ffff:ffff::
 ipv6 access-group-in test6
 mpls enable
 router isis4 1 ena
 router isis4 1 other-ena
 exit
!

addrouter r3
int eth1 eth 0000.0000.3333 $2b$ $2a$
!
vrf def v1
 rd 1:1
 exit
access-list test4
 deny 1 any all any all
 permit all any all any all
 exit
access-list test6
 deny all 4321:: ffff:: all 4321:: ffff:: all
 permit all any all any all
 exit
router isis4 1
 vrf v1
 net 48.4444.0000.3333.00
 multi-topology
 is-type level2
 traffeng 4.4.4.3
 segrout 10
 both segrout
 red conn
 afi-other enable
 afi-other red conn
 exit
int lo1
 vrf for v1
 ipv4 addr 2.2.2.3 255.255.255.255
 ipv6 addr 4321::3 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
 router isis4 1 ena
 router isis4 1 other-ena
 router isis4 1 segrout index 5
 router isis4 1 segrout other-index 6
 router isis4 1 segrout node
 exit
int eth1
 vrf for v1
 ipv4 addr 1.1.1.6 255.255.255.252
 ipv4 access-group-in test4
 ipv6 addr 1234:2::2 ffff:ffff::
 ipv6 access-group-in test6
 mpls enable
 router isis4 1 ena
 router isis4 1 other-ena
 exit
!


r1 tping 100 20 2.2.2.2 /vrf v1 /int lo1
r1 tping 100 20 4321::2 /vrf v1 /int lo1
r1 tping 100 20 2.2.2.3 /vrf v1 /int lo1
r1 tping 100 20 4321::3 /vrf v1 /int lo1
r2 tping 100 20 2.2.2.1 /vrf v1 /int lo1
r2 tping 100 20 4321::1 /vrf v1 /int lo1
r2 tping 100 20 2.2.2.3 /vrf v1 /int lo1
r2 tping 100 20 4321::3 /vrf v1 /int lo1
r3 tping 100 20 2.2.2.1 /vrf v1 /int lo1
r3 tping 100 20 4321::1 /vrf v1 /int lo1
r3 tping 100 20 2.2.2.2 /vrf v1 /int lo1
r3 tping 100 20 4321::2 /vrf v1 /int lo1

r2 output show ipv4 isis 1 nei
r2 output show ipv6 isis 1 nei
r2 output show ipv4 isis 1 dat 2
r2 output show ipv6 isis 1 dat 2
r2 output show ipv4 isis 1 tre 2
r2 output show ipv6 isis 1 tre 2
r2 output show ipv4 route v1
r2 output show ipv6 route v1
r2 output show ipv4 segrou v1
r2 output show ipv6 segrou v1
