description ebgp vpn client with soft-reconfig

addrouter r1
int eth1 eth 0000.0000.1111 $1a$ $1b$
!
vrf def v1
 rd 1:1
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
 exit
route-map rm1
 sequence 10 act deny
  match aspath .*3.*
 sequence 20 act permit
 exit
router bgp4 1
 vrf v1
 address uni
 local-as 1
 router-id 4.4.4.1
 neigh 1.1.1.2 remote-as 2
 neigh 1.1.1.2 soft-reconfig
 neigh 1.1.1.2 route-map-in rm1
 red conn
 exit
router bgp6 1
 vrf v1
 address uni
 local-as 1
 router-id 6.6.6.1
 neigh 1234:1::2 remote-as 2
 neigh 1234:1::2 soft-reconfig
 neigh 1234:1::2 route-map-in rm1
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
int lo0
 vrf for v1
 ipv4 addr 2.2.2.2 255.255.255.255
 ipv6 addr 4321::2 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
 exit
int eth1
 vrf for v1
 ipv4 addr 1.1.1.2 255.255.255.252
 ipv6 addr 1234:1::2 ffff:ffff::
 exit
int eth2
 vrf for v1
 ipv4 addr 1.1.1.5 255.255.255.252
 ipv6 addr 1234:2::1 ffff:ffff::
 exit
router bgp4 1
 vrf v1
 address uni
 local-as 2
 router-id 4.4.4.2
 neigh 1.1.1.1 remote-as 1
 neigh 1.1.1.1 soft-reconfig
 neigh 1.1.1.1 internal-vpn
 neigh 1.1.1.6 remote-as 3
 neigh 1.1.1.6 attribset
 neigh 1.1.1.6 soft-reconfig
 red conn
 exit
router bgp6 1
 vrf v1
 address uni
 local-as 2
 router-id 6.6.6.2
 neigh 1234:1::1 remote-as 1
 neigh 1234:1::1 soft-reconfig
 neigh 1234:1::1 internal-vpn
 neigh 1234:2::2 remote-as 3
 neigh 1234:2::2 attribset
 neigh 1234:2::2 soft-reconfig
 red conn
 exit
!

addrouter r3
int eth1 eth 0000.0000.3333 $2b$ $2a$
int eth2 eth 0000.0000.3333 $3a$ $3b$
!
vrf def v1
 rd 1:1
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
 exit
int eth2
 vrf for v1
 ipv4 addr 1.1.1.9 255.255.255.252
 ipv6 addr 1234:3::1 ffff:ffff::
 exit
router bgp4 1
 vrf v1
 address uni
 local-as 3
 router-id 4.4.4.3
 neigh 1.1.1.5 remote-as 2
 neigh 1.1.1.5 attribset
 neigh 1.1.1.5 soft-reconfig
 neigh 1.1.1.10 remote-as 4
 neigh 1.1.1.10 attribset
 neigh 1.1.1.10 soft-reconfig
 red conn
 exit
router bgp6 1
 vrf v1
 address uni
 local-as 3
 router-id 6.6.6.3
 neigh 1234:2::1 remote-as 2
 neigh 1234:2::1 attribset
 neigh 1234:2::1 soft-reconfig
 neigh 1234:3::2 remote-as 4
 neigh 1234:3::2 attribset
 neigh 1234:3::2 soft-reconfig
 red conn
 exit
!

addrouter r4
int eth1 eth 0000.0000.4444 $3b$ $3a$
int eth2 eth 0000.0000.4444 $4a$ $4b$
!
vrf def v1
 rd 1:1
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
 exit
int eth2
 vrf for v1
 ipv4 addr 1.1.1.13 255.255.255.252
 ipv6 addr 1234:4::1 ffff:ffff::
 exit
router bgp4 1
 vrf v1
 address uni
 local-as 4
 router-id 4.4.4.4
 neigh 1.1.1.9 remote-as 3
 neigh 1.1.1.9 attribset
 neigh 1.1.1.9 soft-reconfig
 neigh 1.1.1.14 remote-as 5
 neigh 1.1.1.14 soft-reconfig
 neigh 1.1.1.14 internal-vpn
 red conn
 exit
router bgp6 1
 vrf v1
 address uni
 local-as 4
 router-id 6.6.6.4
 neigh 1234:3::1 remote-as 3
 neigh 1234:3::1 attribset
 neigh 1234:3::1 soft-reconfig
 neigh 1234:4::2 remote-as 5
 neigh 1234:4::2 soft-reconfig
 neigh 1234:4::2 internal-vpn
 red conn
 exit
!

addrouter r5
int eth1 eth 0000.0000.5555 $4b$ $4a$
!
vrf def v1
 rd 1:1
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
 exit
route-map rm1
 sequence 10 act deny
  match aspath .*3.*
 sequence 20 act permit
 exit
router bgp4 1
 vrf v1
 address uni
 local-as 5
 router-id 4.4.4.5
 neigh 1.1.1.13 remote-as 4
 neigh 1.1.1.13 soft-reconfig
 neigh 1.1.1.13 route-map-in rm1
 red conn
 exit
router bgp6 1
 vrf v1
 address uni
 local-as 5
 router-id 6.6.6.5
 neigh 1234:4::1 remote-as 4
 neigh 1234:4::1 soft-reconfig
 neigh 1234:4::1 route-map-in rm1
 red conn
 exit
!

r2 tping 100 60 2.2.2.1 /vrf v1 /int lo0
r2 tping 100 60 4321::1 /vrf v1 /int lo0
r2 tping 100 60 2.2.2.3 /vrf v1 /int lo0
r2 tping 100 60 4321::3 /vrf v1 /int lo0
r2 tping 100 60 2.2.2.4 /vrf v1 /int lo0
r2 tping 100 60 4321::4 /vrf v1 /int lo0
r2 tping 0 60 2.2.2.5 /vrf v1 /int lo0
r2 tping 0 60 4321::5 /vrf v1 /int lo0

r3 tping 100 60 2.2.2.2 /vrf v1 /int lo0
r3 tping 100 60 4321::2 /vrf v1 /int lo0
r3 tping 100 60 2.2.2.4 /vrf v1 /int lo0
r3 tping 100 60 4321::4 /vrf v1 /int lo0
r3 tping 0 60 2.2.2.1 /vrf v1 /int lo0
r3 tping 0 60 4321::1 /vrf v1 /int lo0
r3 tping 0 60 2.2.2.5 /vrf v1 /int lo0
r3 tping 0 60 4321::5 /vrf v1 /int lo0

r4 tping 100 60 2.2.2.2 /vrf v1 /int lo0
r4 tping 100 60 4321::2 /vrf v1 /int lo0
r4 tping 100 60 2.2.2.3 /vrf v1 /int lo0
r4 tping 100 60 4321::3 /vrf v1 /int lo0
r4 tping 100 60 2.2.2.5 /vrf v1 /int lo0
r4 tping 100 60 4321::5 /vrf v1 /int lo0
r4 tping 0 60 2.2.2.1 /vrf v1 /int lo0
r4 tping 0 60 4321::1 /vrf v1 /int lo0

r1 tping 100 60 2.2.2.2 /vrf v1 /int lo0
r1 tping 100 60 4321::2 /vrf v1 /int lo0
r1 tping 100 60 2.2.2.5 /vrf v1 /int lo0
r1 tping 100 60 4321::5 /vrf v1 /int lo0
r1 tping 0 60 2.2.2.3 /vrf v1 /int lo0
r1 tping 0 60 4321::3 /vrf v1 /int lo0
r1 tping 0 60 2.2.2.4 /vrf v1 /int lo0
r1 tping 0 60 4321::4 /vrf v1 /int lo0

r5 tping 100 60 2.2.2.1 /vrf v1 /int lo0
r5 tping 100 60 4321::1 /vrf v1 /int lo0
r5 tping 100 60 2.2.2.4 /vrf v1 /int lo0
r5 tping 100 60 4321::4 /vrf v1 /int lo0
r5 tping 0 60 2.2.2.2 /vrf v1 /int lo0
r5 tping 0 60 4321::2 /vrf v1 /int lo0
r5 tping 0 60 2.2.2.3 /vrf v1 /int lo0
r5 tping 0 60 4321::3 /vrf v1 /int lo0
