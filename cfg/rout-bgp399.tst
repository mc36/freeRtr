description unified mpls with sr

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
 segrout 10 1
 red conn
 exit
router lsrp6 1
 vrf v1
 router 6.6.6.1
 segrout 10 1
 red conn
 exit
int lo0
 vrf for v1
 ipv4 addr 2.2.2.1 255.255.255.255
 ipv6 addr 4321::1 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
 exit
int eth1
 vrf for v1
 ipv4 addr 1.1.1.1 255.255.255.252
 ipv6 addr 1234:1::1 ffff:ffff::
 mpls enable
 router lsrp4 1 ena
 router lsrp6 1 ena
 exit
int pweth1
 vrf for v1
 ipv4 addr 3.3.3.1 255.255.255.0
 pseudo v1 lo0 pweompls 2.2.2.6 1234
 exit
int pweth2
 vrf for v1
 ipv4 addr 3.3.4.1 255.255.255.0
 pseudo v1 lo0 pweompls 4321::6 1234
 exit
router bgp4 1
 vrf v1
 address lab
 local-as 1
 router-id 4.4.4.1
 neigh 2.2.2.3 remote-as 1
 neigh 2.2.2.3 update lo0
 red conn
 exit
router bgp6 1
 vrf v1
 address lab
 local-as 1
 router-id 6.6.6.1
 neigh 4321::3 remote-as 1
 neigh 4321::3 update lo0
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
 segrout 10 2
 red conn
 exit
router lsrp6 1
 vrf v1
 router 6.6.6.2
 segrout 10 2
 red conn
 exit
int lo0
 vrf for v1
 ipv4 addr 2.2.2.2 255.255.255.255
 ipv6 addr 4321::2 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
 exit
int eth1
 vrf for v1
 ipv4 addr 1.1.1.2 255.255.255.252
 ipv6 addr 1234:1::2 ffff:ffff::
 mpls enable
 router lsrp4 1 ena
 router lsrp6 1 ena
 exit
int eth2
 vrf for v1
 ipv4 addr 1.1.1.5 255.255.255.252
 ipv6 addr 1234:2::1 ffff:ffff::
 mpls enable
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
 segrout 10 3
 red conn
 exit
router lsrp6 1
 vrf v1
 router 6.6.6.3
 segrout 10 3
 red conn
 exit
int lo0
 vrf for v1
 ipv4 addr 2.2.2.3 255.255.255.255
 ipv6 addr 4321::3 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
 exit
int eth1
 vrf for v1
 ipv4 addr 1.1.1.6 255.255.255.252
 ipv6 addr 1234:2::2 ffff:ffff::
 mpls enable
 router lsrp4 1 ena
 router lsrp6 1 ena
 exit
int eth2
 vrf for v1
 ipv4 addr 1.1.1.9 255.255.255.252
 ipv6 addr 1234:3::1 ffff:ffff::
 mpls enable
 exit
router bgp4 1
 vrf v1
 address lab
 local-as 1
 router-id 4.4.4.3
 neigh 2.2.2.1 remote-as 1
 neigh 2.2.2.1 update lo0
 neigh 2.2.2.1 next-hop-self
 neigh 1.1.1.10 remote-as 1
 neigh 1.1.1.10 route-reflect
 neigh 1.1.1.10 next-hop-self
 red conn
 exit
router bgp6 1
 vrf v1
 address lab
 local-as 1
 router-id 6.6.6.3
 neigh 4321::1 remote-as 1
 neigh 4321::1 update lo0
 neigh 4321::1 next-hop-self
 neigh 1234:3::2 remote-as 1
 neigh 1234:3::2 route-reflect
 neigh 1234:3::2 next-hop-self
 red conn
 exit
!

addrouter r4
int eth1 eth 0000.0000.4444 $3b$ $3a$
int eth2 eth 0000.0000.4444 $4a$ $4b$
!
vrf def v1
 rd 1:1
 label-mode per-prefix
 exit
router lsrp4 1
 vrf v1
 router 4.4.4.4
 segrout 10 4
 red conn
 exit
router lsrp6 1
 vrf v1
 router 6.6.6.4
 segrout 10 4
 red conn
 exit
int lo0
 vrf for v1
 ipv4 addr 2.2.2.4 255.255.255.255
 ipv6 addr 4321::4 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
 exit
int eth1
 vrf for v1
 ipv4 addr 1.1.1.10 255.255.255.252
 ipv6 addr 1234:3::2 ffff:ffff::
 mpls enable
 exit
int eth2
 vrf for v1
 ipv4 addr 1.1.1.13 255.255.255.252
 ipv6 addr 1234:4::1 ffff:ffff::
 mpls enable
 router lsrp4 1 ena
 router lsrp6 1 ena
 exit
router bgp4 1
 vrf v1
 address lab
 local-as 1
 router-id 4.4.4.4
 neigh 2.2.2.6 remote-as 1
 neigh 2.2.2.6 update lo0
 neigh 2.2.2.6 next-hop-self
 neigh 1.1.1.9 remote-as 1
 neigh 1.1.1.9 route-reflect
 neigh 1.1.1.9 next-hop-self
 red conn
 exit
router bgp6 1
 vrf v1
 address lab
 local-as 1
 router-id 6.6.6.4
 neigh 4321::6 remote-as 1
 neigh 4321::6 update lo0
 neigh 4321::6 next-hop-self
 neigh 1234:3::1 remote-as 1
 neigh 1234:3::1 route-reflect
 neigh 1234:3::1 next-hop-self
 red conn
 exit
!

addrouter r5
int eth1 eth 0000.0000.5555 $4b$ $4a$
int eth2 eth 0000.0000.5555 $5a$ $5b$
!
vrf def v1
 rd 1:1
 label-mode per-prefix
 exit
router lsrp4 1
 vrf v1
 router 4.4.4.5
 segrout 10 5
 red conn
 exit
router lsrp6 1
 vrf v1
 router 6.6.6.5
 segrout 10 5
 red conn
 exit
int lo0
 vrf for v1
 ipv4 addr 2.2.2.5 255.255.255.255
 ipv6 addr 4321::5 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
 exit
int eth1
 vrf for v1
 ipv4 addr 1.1.1.14 255.255.255.252
 ipv6 addr 1234:4::2 ffff:ffff::
 mpls enable
 router lsrp4 1 ena
 router lsrp6 1 ena
 exit
int eth2
 vrf for v1
 ipv4 addr 1.1.1.17 255.255.255.252
 ipv6 addr 1234:5::1 ffff:ffff::
 mpls enable
 router lsrp4 1 ena
 router lsrp6 1 ena
 exit
!

addrouter r6
int eth1 eth 0000.0000.6666 $5b$ $5a$
!
vrf def v1
 rd 1:1
 label-mode per-prefix
 exit
router lsrp4 1
 vrf v1
 router 4.4.4.6
 segrout 10 6
 red conn
 exit
router lsrp6 1
 vrf v1
 router 6.6.6.6
 segrout 10 6
 red conn
 exit
int lo0
 vrf for v1
 ipv4 addr 2.2.2.6 255.255.255.255
 ipv6 addr 4321::6 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
 exit
int eth1
 vrf for v1
 ipv4 addr 1.1.1.18 255.255.255.252
 ipv6 addr 1234:5::2 ffff:ffff::
 mpls enable
 router lsrp4 1 ena
 router lsrp6 1 ena
 exit
int pweth1
 vrf for v1
 ipv4 addr 3.3.3.2 255.255.255.0
 pseudo v1 lo0 pweompls 2.2.2.1 1234
 exit
int pweth2
 vrf for v1
 ipv4 addr 3.3.4.2 255.255.255.0
 pseudo v1 lo0 pweompls 4321::1 1234
 exit
router bgp4 1
 vrf v1
 address lab
 local-as 1
 router-id 4.4.4.6
 neigh 2.2.2.4 remote-as 1
 neigh 2.2.2.4 update lo0
 red conn
 exit
router bgp6 1
 vrf v1
 address lab
 local-as 1
 router-id 6.6.6.6
 neigh 4321::4 remote-as 1
 neigh 4321::4 update lo0
 red conn
 exit
!


r1 tping 100 60 2.2.2.2 vrf v1 int lo0
r1 tping 100 60 4321::2 vrf v1 int lo0
r1 tping 100 60 2.2.2.3 vrf v1 int lo0
r1 tping 100 60 4321::3 vrf v1 int lo0

r2 tping 100 60 2.2.2.1 vrf v1 int lo0
r2 tping 100 60 4321::1 vrf v1 int lo0
r2 tping 100 60 2.2.2.3 vrf v1 int lo0
r2 tping 100 60 4321::3 vrf v1 int lo0

r3 tping 100 60 2.2.2.1 vrf v1 int lo0
r3 tping 100 60 4321::1 vrf v1 int lo0
r3 tping 100 60 2.2.2.2 vrf v1 int lo0
r3 tping 100 60 4321::2 vrf v1 int lo0

r4 tping 100 60 2.2.2.5 vrf v1 int lo0
r4 tping 100 60 4321::5 vrf v1 int lo0
r4 tping 100 60 2.2.2.6 vrf v1 int lo0
r4 tping 100 60 4321::6 vrf v1 int lo0

r5 tping 100 60 2.2.2.4 vrf v1 int lo0
r5 tping 100 60 4321::4 vrf v1 int lo0
r5 tping 100 60 2.2.2.6 vrf v1 int lo0
r5 tping 100 60 4321::6 vrf v1 int lo0

r6 tping 100 60 2.2.2.4 vrf v1 int lo0
r6 tping 100 60 4321::4 vrf v1 int lo0
r6 tping 100 60 2.2.2.5 vrf v1 int lo0
r6 tping 100 60 4321::5 vrf v1 int lo0

r4 tping 100 60 2.2.2.1 vrf v1 int lo0
r4 tping 100 60 4321::1 vrf v1 int lo0
r4 tping 100 60 2.2.2.3 vrf v1 int lo0
r4 tping 100 60 4321::3 vrf v1 int lo0

r6 tping 100 60 2.2.2.1 vrf v1 int lo0
r6 tping 100 60 4321::1 vrf v1 int lo0
r6 tping 100 60 2.2.2.3 vrf v1 int lo0
r6 tping 100 60 4321::3 vrf v1 int lo0

r1 tping 100 60 2.2.2.4 vrf v1 int lo0
r1 tping 100 60 4321::4 vrf v1 int lo0
r1 tping 100 60 2.2.2.6 vrf v1 int lo0
r1 tping 100 60 4321::6 vrf v1 int lo0

r3 tping 100 60 2.2.2.4 vrf v1 int lo0
r3 tping 100 60 4321::4 vrf v1 int lo0
r3 tping 100 60 2.2.2.6 vrf v1 int lo0
r3 tping 100 60 4321::6 vrf v1 int lo0

r1 tping 100 60 3.3.3.2 vrf v1
r6 tping 100 60 3.3.3.1 vrf v1
r1 tping 100 60 3.3.4.2 vrf v1
r6 tping 100 60 3.3.4.1 vrf v1

