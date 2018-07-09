description bgp with labels over sr

addrouter r1
int eth1 eth 0000.0000.1111 $1a$ $1b$
!
vrf def v1
 rd 1:1
 exit
router lsrp4 1
 vrf v1
 router 4.4.4.1
 segrout 10 1
 exit
router lsrp6 1
 vrf v1
 router 6.6.6.1
 segrout 10 1
 exit
int lo1
 vrf for v1
 ipv4 addr 2.2.2.1 255.255.255.255
 ipv6 addr 4321::1 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
 router lsrp4 1 ena
 router lsrp6 1 ena
 exit
int lo2
 vrf for v1
 ipv4 addr 2.2.2.11 255.255.255.255
 ipv6 addr 4321::11 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
 exit
int eth1
 vrf for v1
 ipv4 addr 1.1.1.1 255.255.255.0
 ipv6 addr 1234::1 ffff::
 mpls enable
 router lsrp4 1 ena
 router lsrp6 1 ena
 exit
router bgp4 1
 vrf v1
 address lab
 local-as 1
 router-id 4.4.4.1
 neigh 2.2.2.3 remote-as 1
 neigh 2.2.2.3 update lo1
 red conn
 exit
router bgp6 1
 vrf v1
 address lab
 local-as 1
 router-id 6.6.6.1
 neigh 4321::3 remote-as 1
 neigh 4321::3 update lo1
 red conn
 exit
!

addrouter r2
int eth1 eth 0000.0000.2222 $1b$ $1a$
int eth2 eth 0000.0000.2222 $2a$ $2b$
!
vrf def v1
 rd 1:1
 exit
router lsrp4 1
 vrf v1
 router 4.4.4.2
 segrout 10 2
 red conn
 exit
router lsrp6 1
 vrf v1
 router 6.6.6.2
 segrout 10 2
 red conn
 exit
int lo1
 vrf for v1
 ipv4 addr 2.2.2.2 255.255.255.255
 ipv6 addr 4321::2 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
 exit
int eth1
 vrf for v1
 ipv4 addr 1.1.1.2 255.255.255.0
 ipv6 addr 1234::2 ffff::
 mpls enable
 router lsrp4 1 ena
 router lsrp6 1 ena
 exit
int eth2
 vrf for v1
 ipv4 addr 1.1.2.2 255.255.255.0
 ipv6 addr 1235::2 ffff::
 mpls enable
 router lsrp4 1 ena
 router lsrp6 1 ena
 exit
!

addrouter r3
int eth1 eth 0000.0000.3333 $2b$ $2a$
!
vrf def v1
 rd 1:1
 exit
router lsrp4 1
 vrf v1
 router 4.4.4.3
 segrout 10 3
 exit
router lsrp6 1
 vrf v1
 router 6.6.6.3
 segrout 10 3
 exit
int lo1
 vrf for v1
 ipv4 addr 2.2.2.3 255.255.255.255
 ipv6 addr 4321::3 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
 router lsrp4 1 ena
 router lsrp6 1 ena
 exit
int lo2
 vrf for v1
 ipv4 addr 2.2.2.13 255.255.255.255
 ipv6 addr 4321::13 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
 exit
int eth1
 vrf for v1
 ipv4 addr 1.1.2.3 255.255.255.0
 ipv6 addr 1235::3 ffff::
 mpls enable
 router lsrp4 1 ena
 router lsrp6 1 ena
 exit
router bgp4 1
 vrf v1
 address lab
 local-as 1
 router-id 4.4.4.3
 neigh 2.2.2.1 remote-as 1
 neigh 2.2.2.1 update lo1
 red conn
 exit
router bgp6 1
 vrf v1
 address lab
 local-as 1
 router-id 6.6.6.3
 neigh 4321::1 remote-as 1
 neigh 4321::1 update lo1
 red conn
 exit
!


r1 tping 100 20 2.2.2.2 /vrf v1 /int lo1
r1 tping 100 20 2.2.2.3 /vrf v1 /int lo1
r1 tping 100 20 4321::2 /vrf v1 /int lo1
r1 tping 100 20 4321::3 /vrf v1 /int lo1

r2 tping 100 20 2.2.2.1 /vrf v1 /int lo1
r2 tping 100 20 2.2.2.3 /vrf v1 /int lo1
r2 tping 100 20 4321::1 /vrf v1 /int lo1
r2 tping 100 20 4321::3 /vrf v1 /int lo1

r3 tping 100 20 2.2.2.1 /vrf v1 /int lo1
r3 tping 100 20 2.2.2.2 /vrf v1 /int lo1
r3 tping 100 20 4321::1 /vrf v1 /int lo1
r3 tping 100 20 4321::2 /vrf v1 /int lo1

r1 tping 100 60 2.2.2.13 /vrf v1 /int lo2
r1 tping 100 60 4321::13 /vrf v1 /int lo2
r3 tping 100 60 2.2.2.11 /vrf v1 /int lo2
r3 tping 100 60 4321::11 /vrf v1 /int lo2
