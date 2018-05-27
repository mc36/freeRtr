description bgp prefix hammering

addrouter r1
int eth1 eth 0000.0000.1111 $1a$ $1b$
int eth2 eth 0000.0000.1111 $2a$ $2b$
int eth3 eth 0000.0000.1111 $3a$ $3b$
int eth4 eth 0000.0000.1111 $4a$ $4b$
int eth5 eth 0000.0000.1111 $5a$ $5b$
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
 ipv6 addr 2222::1 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
 exit
int lo1
 vrf for v1
 ipv4 addr 4.4.4.1 255.255.255.255
 ipv6 addr 4444::1 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
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
int eth4
 bridge-gr 1
 exit
int eth5
 bridge-gr 1
 exit
int bvi1
 vrf for v1
 ipv4 addr 1.1.1.1 255.255.255.0
 ipv6 addr 1234:1::1 ffff:ffff::
 exit
router bgp4 1
 vrf v1
 address uni
 local-as 1
 router-id 4.4.4.1
 neigh 1.1.1.2 remote-as 1
 neigh 1.1.1.2 route-reflect
 neigh 1.1.1.3 remote-as 1
 neigh 1.1.1.3 route-reflect
 neigh 1.1.1.4 remote-as 1
 neigh 1.1.1.4 route-reflect
 neigh 1.1.1.5 remote-as 1
 neigh 1.1.1.5 route-reflect
 red conn
 exit
router bgp6 1
 vrf v1
 address uni
 local-as 1
 router-id 6.6.6.1
 neigh 1234:1::2 remote-as 1
 neigh 1234:1::2 route-reflect
 neigh 1234:1::3 remote-as 1
 neigh 1234:1::3 route-reflect
 neigh 1234:1::4 remote-as 1
 neigh 1234:1::4 route-reflect
 neigh 1234:1::6 remote-as 1
 neigh 1234:1::6 route-reflect
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
 ipv6 addr 2222::2 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
 exit
int lo1
 vrf for v1
 ipv4 addr 4.4.4.2 255.255.255.255
 ipv6 addr 4444::2 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
 exit
int eth1
 vrf for v1
 ipv4 addr 1.1.1.2 255.255.255.0
 ipv6 addr 1234:1::2 ffff:ffff::
 exit
router bgp4 1
 vrf v1
 address uni
 local-as 1
 router-id 4.4.4.2
 neigh 1.1.1.1 remote-as 1
 red conn
 exit
router bgp6 1
 vrf v1
 address uni
 local-as 1
 router-id 6.6.6.2
 neigh 1234:1::1 remote-as 1
 red conn
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
 ipv6 addr 2222::3 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
 exit
int lo1
 vrf for v1
 ipv4 addr 4.4.4.3 255.255.255.255
 ipv6 addr 4444::3 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
 exit
int eth1
 vrf for v1
 ipv4 addr 1.1.1.3 255.255.255.0
 ipv6 addr 1234:1::3 ffff:ffff::
 exit
router bgp4 1
 vrf v1
 address uni
 local-as 1
 router-id 4.4.4.3
 neigh 1.1.1.1 remote-as 1
 red conn
 exit
router bgp6 1
 vrf v1
 address uni
 local-as 1
 router-id 6.6.6.3
 neigh 1234:1::1 remote-as 1
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
 ipv6 addr 2222::4 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
 exit
int lo1
 vrf for v1
 ipv4 addr 4.4.4.4 255.255.255.255
 ipv6 addr 4444::4 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
 exit
int eth1
 vrf for v1
 ipv4 addr 1.1.1.4 255.255.255.0
 ipv6 addr 1234:1::4 ffff:ffff::
 exit
router bgp4 1
 vrf v1
 address uni
 local-as 1
 router-id 4.4.4.4
 neigh 1.1.1.1 remote-as 1
 red conn
 exit
router bgp6 1
 vrf v1
 address uni
 local-as 1
 router-id 6.6.6.4
 neigh 1234:1::1 remote-as 1
 red conn
 exit
!

addrouter r5
int eth1 eth 0000.0000.5555 $4b$ $4a$
!
vrf def v1
 rd 1:1
 exit
route-map all
 action permit
 exit
int eth1
 vrf for v1
 ipv4 addr 1.1.1.5 255.255.255.0
 ipv6 addr 1234:1::5 ffff:ffff::
 exit
!

addrouter r6
int eth1 eth 0000.0000.6666 $5b$ $5a$
!
vrf def v1
 rd 1:1
 exit
route-map all
 action permit
 exit
int eth1
 vrf for v1
 ipv4 addr 1.1.1.6 255.255.255.0
 ipv6 addr 1234:1::6 ffff:ffff::
 exit
!



r5 tping 100 60 1.1.1.1 /vrf v1
r5 send pack bgpgen v1 eth1 1.1.1.1 1 3.3.3.0/24 all 100000
r5 read sending

r6 tping 100 60 1234:1::1 /vrf v1
r6 send pack bgpgen v1 eth1 1234:1::1 1 3333::/120 all 100000
r6 read sending


r1 tping 100 60 2.2.2.2 /vrf v1
r1 tping 100 60 2222::2 /vrf v1
r1 tping 100 60 2.2.2.3 /vrf v1
r1 tping 100 60 2222::3 /vrf v1
r1 tping 100 60 2.2.2.4 /vrf v1
r1 tping 100 60 2222::4 /vrf v1

r1 tping 100 60 4.4.4.2 /vrf v1
r1 tping 100 60 4444::2 /vrf v1
r1 tping 100 60 4.4.4.3 /vrf v1
r1 tping 100 60 4444::3 /vrf v1
r1 tping 100 60 4.4.4.4 /vrf v1
r1 tping 100 60 4444::4 /vrf v1

r2 tping 100 60 2.2.2.1 /vrf v1
r2 tping 100 60 2222::1 /vrf v1
r2 tping 100 60 2.2.2.3 /vrf v1
r2 tping 100 60 2222::3 /vrf v1
r2 tping 100 60 2.2.2.4 /vrf v1
r2 tping 100 60 2222::4 /vrf v1

r2 tping 100 60 4.4.4.1 /vrf v1
r2 tping 100 60 4444::1 /vrf v1
r2 tping 100 60 4.4.4.3 /vrf v1
r2 tping 100 60 4444::3 /vrf v1
r2 tping 100 60 4.4.4.4 /vrf v1
r2 tping 100 60 4444::4 /vrf v1

r3 tping 100 60 2.2.2.1 /vrf v1
r3 tping 100 60 2222::1 /vrf v1
r3 tping 100 60 2.2.2.2 /vrf v1
r3 tping 100 60 2222::2 /vrf v1
r3 tping 100 60 2.2.2.4 /vrf v1
r3 tping 100 60 2222::4 /vrf v1

r3 tping 100 60 4.4.4.1 /vrf v1
r3 tping 100 60 4444::1 /vrf v1
r3 tping 100 60 4.4.4.2 /vrf v1
r3 tping 100 60 4444::2 /vrf v1
r3 tping 100 60 4.4.4.4 /vrf v1
r3 tping 100 60 4444::4 /vrf v1

r4 tping 100 60 2.2.2.1 /vrf v1
r4 tping 100 60 2222::1 /vrf v1
r4 tping 100 60 2.2.2.2 /vrf v1
r4 tping 100 60 2222::2 /vrf v1
r4 tping 100 60 2.2.2.3 /vrf v1
r4 tping 100 60 2222::3 /vrf v1

r4 tping 100 60 4.4.4.1 /vrf v1
r4 tping 100 60 4444::1 /vrf v1
r4 tping 100 60 4.4.4.2 /vrf v1
r4 tping 100 60 4444::2 /vrf v1
r4 tping 100 60 4.4.4.3 /vrf v1
r4 tping 100 60 4444::3 /vrf v1

r5 send end
r5 read finish

r6 send end
r6 read finish

r1 tping 100 60 2.2.2.2 /vrf v1
r1 tping 100 60 2222::2 /vrf v1
r1 tping 100 60 2.2.2.3 /vrf v1
r1 tping 100 60 2222::3 /vrf v1
r1 tping 100 60 2.2.2.4 /vrf v1
r1 tping 100 60 2222::4 /vrf v1

r1 tping 100 60 4.4.4.2 /vrf v1
r1 tping 100 60 4444::2 /vrf v1
r1 tping 100 60 4.4.4.3 /vrf v1
r1 tping 100 60 4444::3 /vrf v1
r1 tping 100 60 4.4.4.4 /vrf v1
r1 tping 100 60 4444::4 /vrf v1

r2 tping 100 60 2.2.2.1 /vrf v1
r2 tping 100 60 2222::1 /vrf v1
r2 tping 100 60 2.2.2.3 /vrf v1
r2 tping 100 60 2222::3 /vrf v1
r2 tping 100 60 2.2.2.4 /vrf v1
r2 tping 100 60 2222::4 /vrf v1

r2 tping 100 60 4.4.4.1 /vrf v1
r2 tping 100 60 4444::1 /vrf v1
r2 tping 100 60 4.4.4.3 /vrf v1
r2 tping 100 60 4444::3 /vrf v1
r2 tping 100 60 4.4.4.4 /vrf v1
r2 tping 100 60 4444::4 /vrf v1

r3 tping 100 60 2.2.2.1 /vrf v1
r3 tping 100 60 2222::1 /vrf v1
r3 tping 100 60 2.2.2.2 /vrf v1
r3 tping 100 60 2222::2 /vrf v1
r3 tping 100 60 2.2.2.4 /vrf v1
r3 tping 100 60 2222::4 /vrf v1

r3 tping 100 60 4.4.4.1 /vrf v1
r3 tping 100 60 4444::1 /vrf v1
r3 tping 100 60 4.4.4.2 /vrf v1
r3 tping 100 60 4444::2 /vrf v1
r3 tping 100 60 4.4.4.4 /vrf v1
r3 tping 100 60 4444::4 /vrf v1

r4 tping 100 60 2.2.2.1 /vrf v1
r4 tping 100 60 2222::1 /vrf v1
r4 tping 100 60 2.2.2.2 /vrf v1
r4 tping 100 60 2222::2 /vrf v1
r4 tping 100 60 2.2.2.3 /vrf v1
r4 tping 100 60 2222::3 /vrf v1

r4 tping 100 60 4.4.4.1 /vrf v1
r4 tping 100 60 4444::1 /vrf v1
r4 tping 100 60 4.4.4.2 /vrf v1
r4 tping 100 60 4444::2 /vrf v1
r4 tping 100 60 4.4.4.3 /vrf v1
r4 tping 100 60 4444::3 /vrf v1
