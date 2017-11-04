description redistribution with everything

addrouter r1
int eth1 eth 0000.0000.1111 $1a$ $1b$
int eth2 eth 0000.0000.1111 $2a$ $2b$
int eth3 eth 0000.0000.1111 $3a$ $3b$
int eth4 eth 0000.0000.1111 $4a$ $4b$
int eth5 eth 0000.0000.1111 $5a$ $5b$
int eth6 eth 0000.0000.1111 $6a$ $6b$
int eth7 eth 0000.0000.1111 $7a$ $7b$
int eth8 eth 0000.0000.1111 $8a$ $8b$
int eth9 eth 0000.0000.1111 $9a$ $9b$
int eth10 eth 0000.0000.1111 $10a$ $10b$
!
vrf def v1
 rd 1:1
 rt-both 1:2
 exit
vrf def v2
 rd 1:2
 label-mode per-prefix
 exit
router isis4 1
 vrf v1
 net 48.4444.0000.1111.00
 is-type level2
 red conn
 red isis4 1
 red ospf4 1
 red rip4 1
 red babel4 1
 red olsr4 1
 red pvrp4 1
 red lsrp4 1
 red eigrp4 1
 red bgp4 1
 red bgp4 2
 exit
router isis6 1
 vrf v1
 net 48.6666.0000.1111.00
 is-type level2
 red conn
 red isis6 1
 red ospf6 1
 red rip6 1
 red babel6 1
 red olsr6 1
 red pvrp6 1
 red lsrp6 1
 red eigrp6 1
 red bgp6 1
 red bgp6 2
 exit
router ospf4 1
 vrf v1
 router 4.4.4.1
 area 0 ena
 red conn
 red isis4 1
 red ospf4 1
 red rip4 1
 red babel4 1
 red olsr4 1
 red pvrp4 1
 red lsrp4 1
 red eigrp4 1
 red bgp4 1
 red bgp4 2
 exit
router ospf6 1
 vrf v1
 router 6.6.6.1
 area 0 ena
 red conn
 red isis6 1
 red ospf6 1
 red rip6 1
 red babel6 1
 red olsr6 1
 red pvrp6 1
 red lsrp6 1
 red eigrp6 1
 red bgp6 1
 red bgp6 2
 exit
router pvrp4 1
 vrf v1
 router 4.4.4.1
 red conn
 red isis4 1
 red ospf4 1
 red rip4 1
 red babel4 1
 red olsr4 1
 red pvrp4 1
 red lsrp4 1
 red eigrp4 1
 red bgp4 1
 red bgp4 2
 exit
router pvrp6 1
 vrf v1
 router 6.6.6.1
 red conn
 red isis6 1
 red ospf6 1
 red rip6 1
 red babel6 1
 red olsr6 1
 red pvrp6 1
 red lsrp6 1
 red eigrp6 1
 red bgp6 1
 red bgp6 2
 exit
router lsrp4 1
 vrf v1
 router 4.4.4.1
 red conn
 red isis4 1
 red ospf4 1
 red rip4 1
 red babel4 1
 red olsr4 1
 red pvrp4 1
 red lsrp4 1
 red eigrp4 1
 red bgp4 1
 red bgp4 2
 exit
router lsrp6 1
 vrf v1
 router 6.6.6.1
 red conn
 red isis6 1
 red ospf6 1
 red rip6 1
 red babel6 1
 red olsr6 1
 red pvrp6 1
 red lsrp6 1
 red eigrp6 1
 red bgp6 1
 red bgp6 2
 exit
router eigrp4 1
 vrf v1
 router 4.4.4.1
 as 1
 red conn
 red isis4 1
 red ospf4 1
 red rip4 1
 red babel4 1
 red olsr4 1
 red pvrp4 1
 red lsrp4 1
 red eigrp4 1
 red bgp4 1
 red bgp4 2
 exit
router eigrp6 1
 vrf v1
 router 6.6.6.1
 as 1
 red conn
 red isis6 1
 red ospf6 1
 red rip6 1
 red babel6 1
 red olsr6 1
 red pvrp6 1
 red lsrp6 1
 red eigrp6 1
 red bgp6 1
 red bgp6 2
 exit
router rip4 1
 vrf v1
 red conn
 red isis4 1
 red ospf4 1
 red rip4 1
 red babel4 1
 red olsr4 1
 red pvrp4 1
 red lsrp4 1
 red eigrp4 1
 red bgp4 1
 red bgp4 2
 exit
router rip6 1
 vrf v1
 red conn
 red isis6 1
 red ospf6 1
 red rip6 1
 red babel6 1
 red olsr6 1
 red pvrp6 1
 red lsrp6 1
 red eigrp6 1
 red bgp6 1
 red bgp6 2
 exit
router babel4 1
 vrf v1
 router 1111-2222-3333-0001
 red conn
 red isis4 1
 red ospf4 1
 red rip4 1
 red babel4 1
 red olsr4 1
 red pvrp4 1
 red lsrp4 1
 red eigrp4 1
 red bgp4 1
 red bgp4 2
 exit
router babel6 1
 vrf v1
 router 1111-2222-3333-0001
 red conn
 red isis6 1
 red ospf6 1
 red rip6 1
 red babel6 1
 red olsr6 1
 red pvrp6 1
 red lsrp6 1
 red eigrp6 1
 red bgp6 1
 red bgp6 2
 exit
router olsr4 1
 vrf v1
 red conn
 red isis4 1
 red ospf4 1
 red rip4 1
 red babel4 1
 red olsr4 1
 red pvrp4 1
 red lsrp4 1
 red eigrp4 1
 red bgp4 1
 red bgp4 2
 exit
router olsr6 1
 vrf v1
 red conn
 red isis6 1
 red ospf6 1
 red rip6 1
 red babel6 1
 red olsr6 1
 red pvrp6 1
 red lsrp6 1
 red eigrp6 1
 red bgp6 1
 red bgp6 2
 exit
router bgp4 1
 vrf v1
 address uni
 local-as 1
 router 4.4.4.1
 neigh 1.1.1.14 remote 2
 red conn
 red isis4 1
 red ospf4 1
 red rip4 1
 red babel4 1
 red olsr4 1
 red pvrp4 1
 red lsrp4 1
 red eigrp4 1
 red bgp4 1
 red bgp4 2
 exit
router bgp6 1
 vrf v1
 address uni
 local-as 1
 router 6.6.6.1
 neigh 1234:4::2 remote 2
 red conn
 red isis6 1
 red ospf6 1
 red rip6 1
 red babel6 1
 red olsr6 1
 red pvrp6 1
 red lsrp6 1
 red eigrp6 1
 red bgp6 1
 red bgp6 2
 exit
router bgp4 2
 vrf v2
 address vpnuni
 local-as 1
 router 4.4.4.1
 neigh 1.1.1.18 remote 3
 neigh 1.1.1.18 send-comm both
 afi-vrf v1 ena
 afi-vrf v1 red conn
 afi-vrf v1 red isis4 1
 afi-vrf v1 red ospf4 1
 afi-vrf v1 red rip4 1
 afi-vrf v1 red babel4 1
 afi-vrf v1 red olsr4 1
 afi-vrf v1 red pvrp4 1
 afi-vrf v1 red lsrp4 1
 afi-vrf v1 red eigrp4 1
 afi-vrf v1 red bgp4 1
 afi-vrf v1 red bgp4 2
 exit
router bgp6 2
 vrf v2
 address vpnuni
 local-as 1
 router 6.6.6.2
 neigh 1234:5::2 remote 3
 neigh 1234:5::2 send-comm both
 afi-vrf v1 ena
 afi-vrf v1 red conn
 afi-vrf v1 red isis6 1
 afi-vrf v1 red ospf6 1
 afi-vrf v1 red rip6 1
 afi-vrf v1 red babel6 1
 afi-vrf v1 red olsr6 1
 afi-vrf v1 red pvrp6 1
 afi-vrf v1 red lsrp6 1
 afi-vrf v1 red eigrp6 1
 afi-vrf v1 red bgp6 1
 afi-vrf v1 red bgp6 2
 exit
int lo1
 vrf for v1
 ipv4 addr 2.2.2.1 255.255.255.255
 ipv6 addr 4321::1 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
 exit
int eth1.11
 vrf for v1
 ipv4 addr 1.1.1.1 255.255.255.252
 router isis4 1 ena
 exit
int eth1.12
 vrf for v1
 ipv6 addr 1234:1::1 ffff:ffff::
 router isis6 1 ena
 exit
int eth2.11
 vrf for v1
 ipv4 addr 1.1.1.5 255.255.255.252
 router ospf4 1 ena
 exit
int eth2.12
 vrf for v1
 ipv6 addr 1234:2::1 ffff:ffff::
 router ospf6 1 ena
 exit
int eth3.11
 vrf for v1
 ipv4 addr 1.1.1.9 255.255.255.252
 router rip4 1 ena
 exit
int eth3.12
 vrf for v1
 ipv6 addr 1234:3::1 ffff:ffff::
 router rip6 1 ena
 exit
int eth4.11
 vrf for v1
 ipv4 addr 1.1.1.13 255.255.255.252
 exit
int eth4.12
 vrf for v1
 ipv6 addr 1234:4::1 ffff:ffff::
 exit
int eth5.11
 vrf for v2
 ipv4 addr 1.1.1.17 255.255.255.252
 mpls enable
 exit
int eth5.12
 vrf for v2
 ipv6 addr 1234:5::1 ffff:ffff::
 mpls enable
 exit
int eth6.11
 vrf for v1
 ipv4 addr 1.1.1.21 255.255.255.252
 router pvrp4 1 ena
 exit
int eth6.12
 vrf for v1
 ipv6 addr 1234:6::1 ffff:ffff::
 router pvrp6 1 ena
 exit
int eth7.11
 vrf for v1
 ipv4 addr 1.1.1.25 255.255.255.252
 router eigrp4 1 ena
 exit
int eth7.12
 vrf for v1
 ipv6 addr 1234:7::1 ffff:ffff::
 router eigrp6 1 ena
 exit
int eth8.11
 vrf for v1
 ipv4 addr 1.1.1.29 255.255.255.252
 router babel4 1 ena
 exit
int eth8.12
 vrf for v1
 ipv6 addr 1234:8::1 ffff:ffff::
 router babel6 1 ena
 exit
int eth9.11
 vrf for v1
 ipv4 addr 1.1.1.33 255.255.255.252
 router lsrp4 1 ena
 exit
int eth9.12
 vrf for v1
 ipv6 addr 1234:9::1 ffff:ffff::
 router lsrp6 1 ena
 exit
int eth10.11
 vrf for v1
 ipv4 addr 1.1.1.37 255.255.255.252
 router olsr4 1 ena
 exit
int eth10.12
 vrf for v1
 ipv6 addr 1234:10::1 ffff:ffff::
 router olsr6 1 ena
 exit
!

addrouter r2
int eth1 eth 0000.0000.2222 $1b$ $1a$
!
vrf def v1
 rd 1:1
 exit
router isis4 1
 vrf v1
 net 48.4444.0000.2222.00
 is-type level2
 red conn
 red isis4 2
 exit
router isis6 1
 vrf v1
 net 48.6666.0000.2222.00
 is-type level2
 red conn
 red isis6 2
 exit
int lo1
 vrf for v1
 ipv4 addr 2.2.2.2 255.255.255.255
 ipv6 addr 4321::2 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
 exit
int eth1.11
 vrf for v1
 ipv4 addr 1.1.1.2 255.255.255.252
 router isis4 1 ena
 exit
int eth1.12
 vrf for v1
 ipv6 addr 1234:1::2 ffff:ffff::
 router isis6 1 ena
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
 area 0 ena
 red conn
 exit
router ospf6 1
 vrf v1
 router 6.6.6.3
 area 0 ena
 red conn
 exit
int lo1
 vrf for v1
 ipv4 addr 2.2.2.3 255.255.255.255
 ipv6 addr 4321::3 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
 exit
int eth1.11
 vrf for v1
 ipv4 addr 1.1.1.6 255.255.255.252
 router ospf4 1 ena
 exit
int eth1.12
 vrf for v1
 ipv6 addr 1234:2::2 ffff:ffff::
 router ospf6 1 ena
 exit
!

addrouter r4
int eth1 eth 0000.0000.3333 $3b$ $3a$
!
vrf def v1
 rd 1:1
 exit
router rip4 1
 vrf v1
 red conn
 exit
router rip6 1
 vrf v1
 red conn
 exit
int lo1
 vrf for v1
 ipv4 addr 2.2.2.4 255.255.255.255
 ipv6 addr 4321::4 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
 exit
int eth1.11
 vrf for v1
 ipv4 addr 1.1.1.10 255.255.255.252
 router rip4 1 ena
 exit
int eth1.12
 vrf for v1
 ipv6 addr 1234:3::2 ffff:ffff::
 router rip6 1 ena
 exit
!

addrouter r5
int eth1 eth 0000.0000.5555 $4b$ $4a$
!
vrf def v1
 rd 1:1
 exit
router bgp4 1
 vrf v1
 address uni
 local-as 2
 router 4.4.4.5
 neigh 1.1.1.13 remote 1
 red conn
 exit
router bgp6 1
 vrf v1
 address uni
 local-as 2
 router 6.6.6.5
 neigh 1234:4::1 remote 1
 red conn
 exit
int lo1
 vrf for v1
 ipv4 addr 2.2.2.5 255.255.255.255
 ipv6 addr 4321::5 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
 exit
int eth1.11
 vrf for v1
 ipv4 addr 1.1.1.14 255.255.255.252
 exit
int eth1.12
 vrf for v1
 ipv6 addr 1234:4::2 ffff:ffff::
 exit
!


addrouter r6
int eth1 eth 0000.0000.6666 $5b$ $5a$
!
vrf def v1
 rd 1:1
 rt-both 1:2
 exit
vrf def v2
 rd 1:2
 label-mode per-prefix
 exit
router bgp4 2
 vrf v2
 address vpnuni
 local-as 3
 router 4.4.4.6
 neigh 1.1.1.17 remote 1
 neigh 1.1.1.17 send-comm both
 afi-vrf v1 ena
 afi-vrf v1 red conn
 exit
router bgp6 2
 vrf v2
 address vpnuni
 local-as 3
 router 6.6.6.6
 neigh 1234:5::1 remote 1
 neigh 1234:5::1 send-comm both
 afi-vrf v1 ena
 afi-vrf v1 red conn
 exit
int lo1
 vrf for v1
 ipv4 addr 2.2.2.6 255.255.255.255
 ipv6 addr 4321::6 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
 exit
int eth1.11
 vrf for v2
 ipv4 addr 1.1.1.18 255.255.255.252
 mpls enable
 exit
int eth1.12
 vrf for v2
 ipv6 addr 1234:5::2 ffff:ffff::
 mpls enable
 exit
!

addrouter r7
int eth1 eth 0000.0000.7777 $6b$ $6a$
!
vrf def v1
 rd 1:1
 exit
router pvrp4 1
 vrf v1
 router 4.4.4.7
 red conn
 exit
router pvrp6 1
 vrf v1
 router 6.6.6.7
 red conn
 exit
int lo1
 vrf for v1
 ipv4 addr 2.2.2.7 255.255.255.255
 ipv6 addr 4321::7 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
 exit
int eth1.11
 vrf for v1
 ipv4 addr 1.1.1.22 255.255.255.252
 router pvrp4 1 ena
 exit
int eth1.12
 vrf for v1
 ipv6 addr 1234:6::2 ffff:ffff::
 router pvrp6 1 ena
 exit
!

addrouter r8
int eth1 eth 0000.0000.8888 $7b$ $7a$
!
vrf def v1
 rd 1:1
 exit
router eigrp4 1
 vrf v1
 router 4.4.4.8
 as 1
 red conn
 exit
router eigrp6 1
 vrf v1
 router 6.6.6.8
 as 1
 red conn
 exit
int lo1
 vrf for v1
 ipv4 addr 2.2.2.8 255.255.255.255
 ipv6 addr 4321::8 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
 exit
int eth1.11
 vrf for v1
 ipv4 addr 1.1.1.26 255.255.255.252
 router eigrp4 1 ena
 exit
int eth1.12
 vrf for v1
 ipv6 addr 1234:7::2 ffff:ffff::
 router eigrp6 1 ena
 exit
!

addrouter r9
int eth1 eth 0000.0000.9999 $8b$ $8a$
!
vrf def v1
 rd 1:1
 exit
router babel4 1
 vrf v1
 router 1111-2222-3333-0009
 red conn
 exit
router babel6 1
 vrf v1
 router 1111-2222-3333-0009
 red conn
 exit
int lo1
 vrf for v1
 ipv4 addr 2.2.2.9 255.255.255.255
 ipv6 addr 4321::9 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
 exit
int eth1.11
 vrf for v1
 ipv4 addr 1.1.1.30 255.255.255.252
 router babel4 1 ena
 exit
int eth1.12
 vrf for v1
 ipv6 addr 1234:8::2 ffff:ffff::
 router babel6 1 ena
 exit
!

addrouter r10
int eth1 eth 0000.0000.aaaa $9b$ $9a$
!
vrf def v1
 rd 1:1
 exit
router lsrp4 1
 vrf v1
 router 4.4.4.10
 red conn
 exit
router lsrp6 1
 vrf v1
 router 6.6.6.10
 red conn
 exit
int lo1
 vrf for v1
 ipv4 addr 2.2.2.10 255.255.255.255
 ipv6 addr 4321::10 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
 exit
int eth1.11
 vrf for v1
 ipv4 addr 1.1.1.34 255.255.255.252
 router lsrp4 1 ena
 exit
int eth1.12
 vrf for v1
 ipv6 addr 1234:9::2 ffff:ffff::
 router lsrp6 1 ena
 exit
!

addrouter r11
int eth1 eth 0000.0000.bbbb $10b$ $10a$
!
vrf def v1
 rd 1:1
 exit
router olsr4 1
 vrf v1
 red conn
 exit
router olsr6 1
 vrf v1
 red conn
 exit
int lo1
 vrf for v1
 ipv4 addr 2.2.2.11 255.255.255.255
 ipv6 addr 4321::11 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
 exit
int eth1.11
 vrf for v1
 ipv4 addr 1.1.1.38 255.255.255.252
 router olsr4 1 ena
 exit
int eth1.12
 vrf for v1
 ipv6 addr 1234:10::2 ffff:ffff::
 router olsr6 1 ena
 exit
!


r1 tping 100 60 2.2.2.2 /vrf v1
r1 tping 100 60 2.2.2.3 /vrf v1
r1 tping 100 60 2.2.2.4 /vrf v1
r1 tping 100 60 2.2.2.5 /vrf v1
r1 tping 100 60 2.2.2.6 /vrf v1
r1 tping 100 60 2.2.2.7 /vrf v1
r1 tping 100 60 2.2.2.8 /vrf v1
r1 tping 100 60 2.2.2.9 /vrf v1
r1 tping 100 60 2.2.2.10 /vrf v1
r1 tping 100 60 2.2.2.11 /vrf v1
r1 tping 100 60 4321::2 /vrf v1
r1 tping 100 60 4321::3 /vrf v1
r1 tping 100 60 4321::4 /vrf v1
r1 tping 100 60 4321::5 /vrf v1
r1 tping 100 60 4321::6 /vrf v1
r1 tping 100 60 4321::7 /vrf v1
r1 tping 100 60 4321::8 /vrf v1
r1 tping 100 60 4321::9 /vrf v1
r1 tping 100 60 4321::10 /vrf v1
r1 tping 100 60 4321::11 /vrf v1

r2 tping 100 60 2.2.2.1 /vrf v1
r2 tping 100 60 2.2.2.3 /vrf v1
r2 tping 100 60 2.2.2.4 /vrf v1
r2 tping 100 60 2.2.2.5 /vrf v1
r2 tping 100 60 2.2.2.6 /vrf v1
r2 tping 100 60 2.2.2.7 /vrf v1
r2 tping 100 60 2.2.2.8 /vrf v1
r2 tping 100 60 2.2.2.9 /vrf v1
r2 tping 100 60 2.2.2.10 /vrf v1
r2 tping 100 60 2.2.2.11 /vrf v1
r2 tping 100 60 4321::1 /vrf v1
r2 tping 100 60 4321::3 /vrf v1
r2 tping 100 60 4321::4 /vrf v1
r2 tping 100 60 4321::5 /vrf v1
r2 tping 100 60 4321::6 /vrf v1
r2 tping 100 60 4321::7 /vrf v1
r2 tping 100 60 4321::8 /vrf v1
r2 tping 100 60 4321::9 /vrf v1
r2 tping 100 60 4321::10 /vrf v1
r2 tping 100 60 4321::11 /vrf v1

r3 tping 100 60 2.2.2.1 /vrf v1
r3 tping 100 60 2.2.2.2 /vrf v1
r3 tping 100 60 2.2.2.4 /vrf v1
r3 tping 100 60 2.2.2.5 /vrf v1
r3 tping 100 60 2.2.2.6 /vrf v1
r3 tping 100 60 2.2.2.7 /vrf v1
r3 tping 100 60 2.2.2.8 /vrf v1
r3 tping 100 60 2.2.2.9 /vrf v1
r3 tping 100 60 2.2.2.10 /vrf v1
r3 tping 100 60 2.2.2.11 /vrf v1
r3 tping 100 60 4321::1 /vrf v1
r3 tping 100 60 4321::2 /vrf v1
r3 tping 100 60 4321::4 /vrf v1
r3 tping 100 60 4321::5 /vrf v1
r3 tping 100 60 4321::6 /vrf v1
r3 tping 100 60 4321::7 /vrf v1
r3 tping 100 60 4321::8 /vrf v1
r3 tping 100 60 4321::9 /vrf v1
r3 tping 100 60 4321::10 /vrf v1
r3 tping 100 60 4321::11 /vrf v1

r4 tping 100 60 2.2.2.1 /vrf v1
r4 tping 100 60 2.2.2.2 /vrf v1
r4 tping 100 60 2.2.2.3 /vrf v1
r4 tping 100 60 2.2.2.5 /vrf v1
r4 tping 100 60 2.2.2.6 /vrf v1
r4 tping 100 60 2.2.2.7 /vrf v1
!r4 tping 100 60 2.2.2.8 /vrf v1
!r4 tping 100 60 2.2.2.9 /vrf v1
r4 tping 100 60 2.2.2.10 /vrf v1
r4 tping 100 60 2.2.2.11 /vrf v1
r4 tping 100 60 4321::1 /vrf v1
r4 tping 100 60 4321::2 /vrf v1
r4 tping 100 60 4321::3 /vrf v1
r4 tping 100 60 4321::5 /vrf v1
r4 tping 100 60 4321::6 /vrf v1
r4 tping 100 60 4321::7 /vrf v1
!r4 tping 100 60 4321::8 /vrf v1
!r4 tping 100 60 4321::9 /vrf v1
r4 tping 100 60 4321::10 /vrf v1
r4 tping 100 60 4321::11 /vrf v1

r5 tping 100 60 2.2.2.1 /vrf v1
r5 tping 100 60 2.2.2.2 /vrf v1
r5 tping 100 60 2.2.2.3 /vrf v1
r5 tping 100 60 2.2.2.4 /vrf v1
r5 tping 100 60 2.2.2.6 /vrf v1
r5 tping 100 60 2.2.2.7 /vrf v1
r5 tping 100 60 2.2.2.8 /vrf v1
r5 tping 100 60 2.2.2.9 /vrf v1
r5 tping 100 60 2.2.2.10 /vrf v1
r5 tping 100 60 2.2.2.11 /vrf v1
r5 tping 100 60 4321::1 /vrf v1
r5 tping 100 60 4321::2 /vrf v1
r5 tping 100 60 4321::3 /vrf v1
r5 tping 100 60 4321::4 /vrf v1
r5 tping 100 60 4321::6 /vrf v1
r5 tping 100 60 4321::7 /vrf v1
r5 tping 100 60 4321::8 /vrf v1
r5 tping 100 60 4321::9 /vrf v1
r5 tping 100 60 4321::10 /vrf v1
r5 tping 100 60 4321::11 /vrf v1

r6 tping 100 60 2.2.2.1 /vrf v1
r6 tping 100 60 2.2.2.2 /vrf v1
r6 tping 100 60 2.2.2.3 /vrf v1
r6 tping 100 60 2.2.2.4 /vrf v1
r6 tping 100 60 2.2.2.5 /vrf v1
r6 tping 100 60 2.2.2.7 /vrf v1
r6 tping 100 60 2.2.2.8 /vrf v1
r6 tping 100 60 2.2.2.9 /vrf v1
r6 tping 100 60 2.2.2.10 /vrf v1
r6 tping 100 60 2.2.2.11 /vrf v1
r6 tping 100 60 4321::1 /vrf v1
r6 tping 100 60 4321::2 /vrf v1
r6 tping 100 60 4321::3 /vrf v1
r6 tping 100 60 4321::4 /vrf v1
r6 tping 100 60 4321::5 /vrf v1
r6 tping 100 60 4321::7 /vrf v1
r6 tping 100 60 4321::8 /vrf v1
r6 tping 100 60 4321::9 /vrf v1
r6 tping 100 60 4321::10 /vrf v1
r6 tping 100 60 4321::11 /vrf v1

r7 tping 100 60 2.2.2.1 /vrf v1
r7 tping 100 60 2.2.2.2 /vrf v1
r7 tping 100 60 2.2.2.3 /vrf v1
r7 tping 100 60 2.2.2.4 /vrf v1
r7 tping 100 60 2.2.2.5 /vrf v1
r7 tping 100 60 2.2.2.6 /vrf v1
r7 tping 100 60 2.2.2.8 /vrf v1
r7 tping 100 60 2.2.2.9 /vrf v1
r7 tping 100 60 2.2.2.10 /vrf v1
r7 tping 100 60 2.2.2.11 /vrf v1
r7 tping 100 60 4321::1 /vrf v1
r7 tping 100 60 4321::2 /vrf v1
r7 tping 100 60 4321::3 /vrf v1
r7 tping 100 60 4321::4 /vrf v1
r7 tping 100 60 4321::5 /vrf v1
r7 tping 100 60 4321::6 /vrf v1
r7 tping 100 60 4321::8 /vrf v1
r7 tping 100 60 4321::9 /vrf v1
r7 tping 100 60 4321::10 /vrf v1
r7 tping 100 60 4321::11 /vrf v1

r8 tping 100 60 2.2.2.1 /vrf v1
r8 tping 100 60 2.2.2.2 /vrf v1
r8 tping 100 60 2.2.2.3 /vrf v1
r8 tping 100 60 2.2.2.4 /vrf v1
r8 tping 100 60 2.2.2.5 /vrf v1
r8 tping 100 60 2.2.2.6 /vrf v1
r8 tping 100 60 2.2.2.7 /vrf v1
r8 tping 100 60 2.2.2.9 /vrf v1
r8 tping 100 60 2.2.2.10 /vrf v1
r8 tping 100 60 2.2.2.11 /vrf v1
r8 tping 100 60 4321::1 /vrf v1
r8 tping 100 60 4321::2 /vrf v1
r8 tping 100 60 4321::3 /vrf v1
r8 tping 100 60 4321::4 /vrf v1
r8 tping 100 60 4321::5 /vrf v1
r8 tping 100 60 4321::6 /vrf v1
r8 tping 100 60 4321::7 /vrf v1
r8 tping 100 60 4321::9 /vrf v1
r8 tping 100 60 4321::10 /vrf v1
r8 tping 100 60 4321::11 /vrf v1

r9 tping 100 60 2.2.2.1 /vrf v1
r9 tping 100 60 2.2.2.2 /vrf v1
r9 tping 100 60 2.2.2.3 /vrf v1
r9 tping 100 60 2.2.2.4 /vrf v1
r9 tping 100 60 2.2.2.5 /vrf v1
r9 tping 100 60 2.2.2.6 /vrf v1
r9 tping 100 60 2.2.2.7 /vrf v1
!r9 tping 100 60 2.2.2.8 /vrf v1
r9 tping 100 60 2.2.2.10 /vrf v1
r9 tping 100 60 2.2.2.11 /vrf v1
r9 tping 100 60 4321::1 /vrf v1
r9 tping 100 60 4321::2 /vrf v1
r9 tping 100 60 4321::3 /vrf v1
r9 tping 100 60 4321::4 /vrf v1
r9 tping 100 60 4321::5 /vrf v1
r9 tping 100 60 4321::6 /vrf v1
r9 tping 100 60 4321::7 /vrf v1
!r9 tping 100 60 4321::8 /vrf v1
r9 tping 100 60 4321::10 /vrf v1
r9 tping 100 60 4321::11 /vrf v1

r10 tping 100 60 2.2.2.1 /vrf v1
r10 tping 100 60 2.2.2.2 /vrf v1
r10 tping 100 60 2.2.2.3 /vrf v1
r10 tping 100 60 2.2.2.4 /vrf v1
r10 tping 100 60 2.2.2.5 /vrf v1
r10 tping 100 60 2.2.2.6 /vrf v1
r10 tping 100 60 2.2.2.7 /vrf v1
r10 tping 100 60 2.2.2.8 /vrf v1
r10 tping 100 60 2.2.2.9 /vrf v1
r10 tping 100 60 2.2.2.11 /vrf v1
r10 tping 100 60 4321::1 /vrf v1
r10 tping 100 60 4321::2 /vrf v1
r10 tping 100 60 4321::3 /vrf v1
r10 tping 100 60 4321::4 /vrf v1
r10 tping 100 60 4321::5 /vrf v1
r10 tping 100 60 4321::6 /vrf v1
r10 tping 100 60 4321::7 /vrf v1
r10 tping 100 60 4321::8 /vrf v1
r10 tping 100 60 4321::9 /vrf v1
r10 tping 100 60 4321::11 /vrf v1

r11 tping 100 60 2.2.2.1 /vrf v1
r11 tping 100 60 2.2.2.2 /vrf v1
r11 tping 100 60 2.2.2.3 /vrf v1
r11 tping 100 60 2.2.2.4 /vrf v1
r11 tping 100 60 2.2.2.5 /vrf v1
r11 tping 100 60 2.2.2.6 /vrf v1
r11 tping 100 60 2.2.2.7 /vrf v1
!r11 tping 100 60 2.2.2.8 /vrf v1
!r11 tping 100 60 2.2.2.9 /vrf v1
r11 tping 100 60 2.2.2.10 /vrf v1
r11 tping 100 60 4321::1 /vrf v1
r11 tping 100 60 4321::2 /vrf v1
r11 tping 100 60 4321::3 /vrf v1
r11 tping 100 60 4321::4 /vrf v1
r11 tping 100 60 4321::5 /vrf v1
r11 tping 100 60 4321::6 /vrf v1
r11 tping 100 60 4321::7 /vrf v1
!r11 tping 100 60 4321::8 /vrf v1
!r11 tping 100 60 4321::9 /vrf v1
r11 tping 100 60 4321::10 /vrf v1
