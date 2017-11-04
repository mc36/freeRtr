description bgp prefix packing

addrouter r1
int eth1 eth 0000.0000.1111 $1a$ $1b$
!
vrf def v1
 rd 1:1
 exit
int lo1
 vrf for v1
 ipv4 addr 2.2.2.1 255.255.255.255
 ipv6 addr 2222::1 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
 exit
int lo2
 vrf for v1
 ipv4 addr 2.2.2.2 255.255.255.255
 ipv6 addr 2222::2 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
 exit
int lo3
 vrf for v1
 ipv4 addr 2.2.2.3 255.255.255.255
 ipv6 addr 2222::3 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
 exit
int lo4
 vrf for v1
 ipv4 addr 2.2.2.4 255.255.255.255
 ipv6 addr 2222::4 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
 exit
int lo5
 vrf for v1
 ipv4 addr 2.2.2.5 255.255.255.255
 ipv6 addr 2222::5 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
 exit
int lo6
 vrf for v1
 ipv4 addr 2.2.2.6 255.255.255.255
 ipv6 addr 2222::6 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
 exit
int lo7
 vrf for v1
 ipv4 addr 2.2.2.7 255.255.255.255
 ipv6 addr 2222::7 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
 exit
int lo8
 vrf for v1
 ipv4 addr 2.2.2.8 255.255.255.255
 ipv6 addr 2222::8 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
 exit
int eth1
 vrf for v1
 ipv4 addr 1.1.1.1 255.0.0.0
 ipv6 addr 1234:1::1 ffff:ffff::
 exit
router bgp4 1
 vrf v1
 address uni
 local-as 1
 router-id 4.4.4.1
 neigh 1.1.1.2 remote-as 2
 red conn
 exit
router bgp6 1
 vrf v1
 address uni
 local-as 1
 router-id 6.6.6.1
 neigh 1234:1::2 remote-as 2
 red conn
 exit
!

addrouter r2
int eth1 eth 0000.0000.2222 $1b$ $1a$
!
vrf def v1
 rd 1:1
 exit
int lo1
 vrf for v1
 ipv4 addr 3.3.3.1 255.255.255.255
 ipv6 addr 3333::1 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
 exit
int lo2
 vrf for v1
 ipv4 addr 3.3.3.2 255.255.255.255
 ipv6 addr 3333::2 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
 exit
int lo3
 vrf for v1
 ipv4 addr 3.3.3.3 255.255.255.255
 ipv6 addr 3333::3 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
 exit
int lo4
 vrf for v1
 ipv4 addr 3.3.3.4 255.255.255.255
 ipv6 addr 3333::4 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
 exit
int lo5
 vrf for v1
 ipv4 addr 3.3.3.5 255.255.255.255
 ipv6 addr 3333::5 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
 exit
int lo6
 vrf for v1
 ipv4 addr 3.3.3.6 255.255.255.255
 ipv6 addr 3333::6 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
 exit
int lo7
 vrf for v1
 ipv4 addr 3.3.3.7 255.255.255.255
 ipv6 addr 3333::7 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
 exit
int lo8
 vrf for v1
 ipv4 addr 3.3.3.8 255.255.255.255
 ipv6 addr 3333::8 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
 exit
int eth1
 vrf for v1
 ipv4 addr 1.1.1.2 255.0.0.0
 ipv6 addr 1234:1::2 ffff:ffff::
 exit
router bgp4 1
 vrf v1
 address uni
 local-as 2
 router-id 4.4.4.2
 neigh 1.1.1.1 remote-as 1
 red conn
 exit
router bgp6 1
 vrf v1
 address uni
 local-as 2
 router-id 6.6.6.2
 neigh 1234:1::1 remote-as 1
 red conn
 exit
!

r1 tping 100 60 3.3.3.1 /vrf v1
r1 tping 100 60 3333::1 /vrf v1
r1 tping 100 60 3.3.3.2 /vrf v1
r1 tping 100 60 3333::2 /vrf v1
r1 tping 100 60 3.3.3.3 /vrf v1
r1 tping 100 60 3333::3 /vrf v1
r1 tping 100 60 3.3.3.4 /vrf v1
r1 tping 100 60 3333::4 /vrf v1
r1 tping 100 60 3.3.3.5 /vrf v1
r1 tping 100 60 3333::5 /vrf v1
r1 tping 100 60 3.3.3.6 /vrf v1
r1 tping 100 60 3333::6 /vrf v1
r1 tping 100 60 3.3.3.7 /vrf v1
r1 tping 100 60 3333::7 /vrf v1
r1 tping 100 60 3.3.3.8 /vrf v1
r1 tping 100 60 3333::8 /vrf v1

r2 tping 100 60 2.2.2.1 /vrf v1
r2 tping 100 60 2222::1 /vrf v1
r2 tping 100 60 2.2.2.2 /vrf v1
r2 tping 100 60 2222::2 /vrf v1
r2 tping 100 60 2.2.2.3 /vrf v1
r2 tping 100 60 2222::3 /vrf v1
r2 tping 100 60 2.2.2.4 /vrf v1
r2 tping 100 60 2222::4 /vrf v1
r2 tping 100 60 2.2.2.5 /vrf v1
r2 tping 100 60 2222::5 /vrf v1
r2 tping 100 60 2.2.2.6 /vrf v1
r2 tping 100 60 2222::6 /vrf v1
r2 tping 100 60 2.2.2.7 /vrf v1
r2 tping 100 60 2222::7 /vrf v1
r2 tping 100 60 2.2.2.8 /vrf v1
r2 tping 100 60 2222::8 /vrf v1

r1 send conf t
r1 send router bgp4 1
r1 send no red conn
r1 send exit
r1 send router bgp6 1
r1 send no red conn
r1 send end

r1 tping 100 60 3.3.3.1 /vrf v1
r1 tping 100 60 3333::1 /vrf v1
r1 tping 100 60 3.3.3.2 /vrf v1
r1 tping 100 60 3333::2 /vrf v1
r1 tping 100 60 3.3.3.3 /vrf v1
r1 tping 100 60 3333::3 /vrf v1
r1 tping 100 60 3.3.3.4 /vrf v1
r1 tping 100 60 3333::4 /vrf v1
r1 tping 100 60 3.3.3.5 /vrf v1
r1 tping 100 60 3333::5 /vrf v1
r1 tping 100 60 3.3.3.6 /vrf v1
r1 tping 100 60 3333::6 /vrf v1
r1 tping 100 60 3.3.3.7 /vrf v1
r1 tping 100 60 3333::7 /vrf v1
r1 tping 100 60 3.3.3.8 /vrf v1
r1 tping 100 60 3333::8 /vrf v1

r2 tping 0 60 2.2.2.1 /vrf v1
r2 tping 0 60 2222::1 /vrf v1
r2 tping 0 60 2.2.2.2 /vrf v1
r2 tping 0 60 2222::2 /vrf v1
r2 tping 0 60 2.2.2.3 /vrf v1
r2 tping 0 60 2222::3 /vrf v1
r2 tping 0 60 2.2.2.4 /vrf v1
r2 tping 0 60 2222::4 /vrf v1
r2 tping 0 60 2.2.2.5 /vrf v1
r2 tping 0 60 2222::5 /vrf v1
r2 tping 0 60 2.2.2.6 /vrf v1
r2 tping 0 60 2222::6 /vrf v1
r2 tping 0 60 2.2.2.7 /vrf v1
r2 tping 0 60 2222::7 /vrf v1
r2 tping 0 60 2.2.2.8 /vrf v1
r2 tping 0 60 2222::8 /vrf v1

r1 send conf t
r1 send router bgp4 1
r1 send red conn
r1 send exit
r1 send router bgp6 1
r1 send red conn
r1 send end

r1 tping 100 60 3.3.3.1 /vrf v1
r1 tping 100 60 3333::1 /vrf v1
r1 tping 100 60 3.3.3.2 /vrf v1
r1 tping 100 60 3333::2 /vrf v1
r1 tping 100 60 3.3.3.3 /vrf v1
r1 tping 100 60 3333::3 /vrf v1
r1 tping 100 60 3.3.3.4 /vrf v1
r1 tping 100 60 3333::4 /vrf v1
r1 tping 100 60 3.3.3.5 /vrf v1
r1 tping 100 60 3333::5 /vrf v1
r1 tping 100 60 3.3.3.6 /vrf v1
r1 tping 100 60 3333::6 /vrf v1
r1 tping 100 60 3.3.3.7 /vrf v1
r1 tping 100 60 3333::7 /vrf v1
r1 tping 100 60 3.3.3.8 /vrf v1
r1 tping 100 60 3333::8 /vrf v1

r2 tping 100 60 2.2.2.1 /vrf v1
r2 tping 100 60 2222::1 /vrf v1
r2 tping 100 60 2.2.2.2 /vrf v1
r2 tping 100 60 2222::2 /vrf v1
r2 tping 100 60 2.2.2.3 /vrf v1
r2 tping 100 60 2222::3 /vrf v1
r2 tping 100 60 2.2.2.4 /vrf v1
r2 tping 100 60 2222::4 /vrf v1
r2 tping 100 60 2.2.2.5 /vrf v1
r2 tping 100 60 2222::5 /vrf v1
r2 tping 100 60 2.2.2.6 /vrf v1
r2 tping 100 60 2222::6 /vrf v1
r2 tping 100 60 2.2.2.7 /vrf v1
r2 tping 100 60 2222::7 /vrf v1
r2 tping 100 60 2.2.2.8 /vrf v1
r2 tping 100 60 2222::8 /vrf v1
