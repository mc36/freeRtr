description bgp unknown attribute ingress

addrouter r1
int eth1 eth 0000.0000.1111 $1a$ $1b$
int eth2 eth 0000.0000.2222 $2a$ $2b$
int eth3 eth 0000.0000.3333 $3a$ $3b$
!
vrf def v1
 rd 1:1
 exit
bridge 1
 mac-learn
 exit
int lo0
 vrf for v1
 ipv4 addr 2.2.2.1 255.255.255.255
 ipv6 addr 4321::1 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
 exit
int eth1
 bridge-gr 1
 exit
int eth2
 bridge-gr 1
 exit
int eth3
 bridge-gr 1
 exit
int bvi1
 vrf for v1
 ipv4 addr 1.1.1.1 255.255.255.0
 ipv6 addr 1234::1 ffff:ffff::
 exit
router bgp4 1
 vrf v1
 address uni
 local-as 1
 router-id 4.4.4.1
 neigh 1.1.1.2 remote-as 1
 neigh 1.1.1.2 route-reflect
 neigh 1.1.1.2 unknowns-in all
 neigh 1.1.1.3 remote-as 1
 neigh 1.1.1.3 route-reflect
 neigh 1.1.1.3 unknowns-in all
 neigh 1.1.1.4 remote-as 1
 neigh 1.1.1.4 route-reflect
 neigh 1.1.1.4 unknowns-in all
 red conn
 exit
router bgp6 1
 vrf v1
 address uni
 local-as 1
 router-id 6.6.6.1
 neigh 1234::2 remote-as 1
 neigh 1234::2 route-reflect
 neigh 1234::2 unknowns-in all
 neigh 1234::3 remote-as 1
 neigh 1234::3 route-reflect
 neigh 1234::3 unknowns-in all
 neigh 1234::4 remote-as 1
 neigh 1234::4 route-reflect
 neigh 1234::4 unknowns-in all
 red conn
 exit
!

addrouter r2
int eth1 eth 0000.0000.2222 $1b$ $1a$
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
 ipv4 addr 1.1.1.2 255.255.255.0
 ipv6 addr 1234::2 ffff:ffff::
 exit
route-map all
 action permit
 exit
!

addrouter r3
int eth1 eth 0000.0000.3333 $2b$ $2a$
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
 ipv4 addr 1.1.1.3 255.255.255.0
 ipv6 addr 1234::3 ffff:ffff::
 exit
route-map rm1
 sequence 10 act deny
  match unknown 2
 sequence 20 act permit
 exit
router bgp4 1
 vrf v1
 address uni
 local-as 1
 router-id 4.4.4.3
 neigh 1.1.1.1 remote-as 1
 neigh 1.1.1.1 route-map-in rm1
 neigh 1.1.1.1 unknowns-in all
 neigh 1.1.1.1 unknowns-out all
 red conn
 exit
router bgp6 1
 vrf v1
 address uni
 local-as 1
 router-id 6.6.6.3
 neigh 1234::1 remote-as 1
 neigh 1234::1 route-map-in rm1
 neigh 1234::1 unknowns-in all
 neigh 1234::1 unknowns-out all
 red conn
 exit
!

addrouter r4
int eth1 eth 0000.0000.4444 $3b$ $3a$
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
 ipv4 addr 1.1.1.4 255.255.255.0
 ipv6 addr 1234::4 ffff:ffff::
 exit
route-map all
 action permit
 exit
!


r2 tping 100 60 1.1.1.1 vrf v1
r2 send pack bgpattr v1 eth1 1.1.1.1 1 2.2.2.2/32 all 255 4 3 2 1 2 3 4 , 0 1 2 3 2 1
r2 read wait

r4 tping 100 60 1234::1 vrf v1
r4 send pack bgpattr v1 eth1 1234::1 1 4321::4/128 all 255 4 3 2 1 2 3 4 , 0 1 2 3 2 1
r4 read wait

r1 tping 100 60 2.2.2.3 vrf v1
r1 tping 100 60 4321::3 vrf v1
r1 tping 100 60 2.2.2.2 vrf v1
r1 tping 0 60 4321::2 vrf v1
r1 tping 0 60 2.2.2.4 vrf v1
r1 tping 100 60 4321::4 vrf v1

r3 tping 100 60 2.2.2.1 vrf v1
r3 tping 100 60 4321::1 vrf v1
r3 tping 100 60 2.2.2.2 vrf v1
r3 tping 0 60 4321::2 vrf v1
r3 tping 0 60 2.2.2.4 vrf v1
r3 tping 100 60 4321::4 vrf v1
