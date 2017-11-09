description multicast routing with pim over bier

addrouter r1
int eth1 eth 0000.0000.1111 $1a$ $1b$
!
vrf def v1
 rd 1:1
 label-mode per-prefix
 exit
router lsrp4 1
 vrf v1
 router 4.4.4.1
 bier 256 10 1
 exit
router lsrp6 1
 vrf v1
 router 6.6.6.1
 bier 256 10 1
 exit
int lo1
 vrf for v1
 ipv4 addr 2.2.2.1 255.255.255.255
 ipv6 addr 4321::1 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
 router lsrp4 1 ena
 router lsrp6 1 ena
 ipv4 pim ena
 ipv6 pim ena
 ipv4 pim join lo1
 ipv6 pim join lo1
 ipv4 pim bier 1
 ipv6 pim bier 1
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
 mpls ldp4
 mpls ldp6
 router lsrp4 1 ena
 router lsrp6 1 ena
 ipv4 pim ena
 ipv6 pim ena
 ipv4 pim join lo1
 ipv6 pim join lo1
 ipv4 pim bier 1
 ipv6 pim bier 1
 exit
router bgp4 1
 vrf v1
 address uni multi
 local-as 1
 router-id 4.4.4.1
 neigh 2.2.2.4 remote-as 1
 neigh 2.2.2.4 update lo1
 red conn
 exit
router bgp6 1
 vrf v1
 address uni multi
 local-as 1
 router-id 6.6.6.1
 neigh 4321::4 remote-as 1
 neigh 4321::4 update lo1
 red conn
 exit
!

addrouter r2
int eth1 eth 0000.0000.2222 $1b$ $1a$
int eth2 eth 0000.0000.2222 $2a$ $2b$
!
vrf def v1
 rd 1:1
 label-mode per-prefix
 exit
router lsrp4 1
 vrf v1
 router 4.4.4.2
 bier 256 10 2
 red conn
 exit
router lsrp6 1
 vrf v1
 router 6.6.6.2
 bier 256 10 2
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
 mpls ldp4
 mpls ldp6
 router lsrp4 1 ena
 router lsrp6 1 ena
 exit
int eth2
 vrf for v1
 ipv4 addr 1.1.2.2 255.255.255.0
 ipv6 addr 1235::2 ffff::
 mpls enable
 mpls ldp4
 mpls ldp6
 router lsrp4 1 ena
 router lsrp6 1 ena
 exit
!

addrouter r3
int eth1 eth 0000.0000.3333 $2b$ $2a$
int eth2 eth 0000.0000.3333 $3a$ $3b$
!
vrf def v1
 rd 1:1
 label-mode per-prefix
 exit
router lsrp4 1
 vrf v1
 router 4.4.4.3
 bier 256 10 3
 red conn
 exit
router lsrp6 1
 vrf v1
 router 6.6.6.3
 bier 256 10 3
 red conn
 exit
int lo1
 vrf for v1
 ipv4 addr 2.2.2.3 255.255.255.255
 ipv6 addr 4321::3 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
 exit
int eth1
 vrf for v1
 ipv4 addr 1.1.2.3 255.255.255.0
 ipv6 addr 1235::3 ffff::
 mpls enable
 mpls ldp4
 mpls ldp6
 router lsrp4 1 ena
 router lsrp6 1 ena
 exit
int eth2
 vrf for v1
 ipv4 addr 1.1.3.3 255.255.255.0
 ipv6 addr 1236::3 ffff::
 mpls enable
 mpls ldp4
 mpls ldp6
 router lsrp4 1 ena
 router lsrp6 1 ena
 exit
!

addrouter r4
int eth1 eth 0000.0000.4444 $3b$ $3a$
!
vrf def v1
 rd 1:1
 label-mode per-prefix
 exit
router lsrp4 1
 vrf v1
 router 4.4.4.4
 bier 256 10 4
 exit
router lsrp6 1
 vrf v1
 router 6.6.6.4
 bier 256 10 4
 exit
int lo1
 vrf for v1
 ipv4 addr 2.2.2.4 255.255.255.255
 ipv6 addr 4321::4 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
 router lsrp4 1 ena
 router lsrp6 1 ena
 ipv4 pim ena
 ipv6 pim ena
 ipv4 pim join lo1
 ipv6 pim join lo1
 ipv4 pim bier 4
 ipv6 pim bier 4
 exit
int lo2
 vrf for v1
 ipv4 addr 2.2.2.14 255.255.255.255
 ipv6 addr 4321::14 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
 exit
int eth1
 vrf for v1
 ipv4 addr 1.1.3.4 255.255.255.0
 ipv6 addr 1236::4 ffff::
 mpls enable
 mpls ldp4
 mpls ldp6
 router lsrp4 1 ena
 router lsrp6 1 ena
 ipv4 pim ena
 ipv6 pim ena
 ipv4 pim join lo1
 ipv6 pim join lo1
 ipv4 pim bier 4
 ipv6 pim bier 4
 exit
router bgp4 1
 vrf v1
 address uni multi
 local-as 1
 router-id 4.4.4.4
 neigh 2.2.2.1 remote-as 1
 neigh 2.2.2.1 update lo1
 red conn
 exit
router bgp6 1
 vrf v1
 address uni multi
 local-as 1
 router-id 6.6.6.4
 neigh 4321::1 remote-as 1
 neigh 4321::1 update lo1
 red conn
 exit
ipv4 multi v1 join 232.2.2.2 2.2.2.1
ipv6 multi v1 join ff06::1 4321::1
!


r1 tping 100 20 2.2.2.2 /vrf v1 /int lo1
r1 tping 100 20 4321::2 /vrf v1 /int lo1
r1 tping 100 20 2.2.2.3 /vrf v1 /int lo1
r1 tping 100 20 4321::3 /vrf v1 /int lo1
r1 tping 100 20 2.2.2.4 /vrf v1 /int lo1
r1 tping 100 20 4321::4 /vrf v1 /int lo1

r2 tping 100 20 2.2.2.1 /vrf v1 /int lo1
r2 tping 100 20 4321::1 /vrf v1 /int lo1
r2 tping 100 20 2.2.2.3 /vrf v1 /int lo1
r2 tping 100 20 4321::3 /vrf v1 /int lo1
r2 tping 100 20 2.2.2.4 /vrf v1 /int lo1
r2 tping 100 20 4321::4 /vrf v1 /int lo1

r3 tping 100 20 2.2.2.1 /vrf v1 /int lo1
r3 tping 100 20 4321::1 /vrf v1 /int lo1
r3 tping 100 20 2.2.2.2 /vrf v1 /int lo1
r3 tping 100 20 4321::2 /vrf v1 /int lo1
r3 tping 100 20 2.2.2.4 /vrf v1 /int lo1
r3 tping 100 20 4321::4 /vrf v1 /int lo1

r4 tping 100 20 2.2.2.1 /vrf v1 /int lo1
r4 tping 100 20 4321::1 /vrf v1 /int lo1
r4 tping 100 20 2.2.2.2 /vrf v1 /int lo1
r4 tping 100 20 4321::2 /vrf v1 /int lo1
r4 tping 100 20 2.2.2.3 /vrf v1 /int lo1
r4 tping 100 20 4321::3 /vrf v1 /int lo1

r1 tping 100 20 2.2.2.14 /vrf v1 /int lo2
r1 tping 100 20 4321::14 /vrf v1 /int lo2
r4 tping 100 20 2.2.2.11 /vrf v1 /int lo2
r4 tping 100 20 4321::11 /vrf v1 /int lo2

r1 tping 100 10 232.2.2.2 /vrf v1 /int lo1
r1 tping 100 10 ff06::1 /vrf v1 /int lo1
