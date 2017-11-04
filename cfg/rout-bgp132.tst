description bgp maximum prefix

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
int lo1
 vrf for v1
 ipv4 addr 2.2.2.11 255.255.255.255
 ipv6 addr 4321::11 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
 exit
int lo2
 vrf for v1
 ipv4 addr 2.2.2.21 255.255.255.255
 ipv6 addr 4321::21 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
 exit
int eth1
 vrf for v1
 ipv4 addr 1.1.1.1 255.255.255.252
 ipv6 addr 1234:1::1 ffff:ffff::
 exit
router bgp4 1
 vrf v1
 address uni
 local-as 1
 router-id 4.4.4.1
 neigh 1.1.1.2 remote-as 2
 neigh 1.1.1.2 maximum 3 50
 red conn
 exit
router bgp6 1
 vrf v1
 address uni
 local-as 1
 router-id 6.6.6.1
 neigh 1234:1::2 remote-as 2
 neigh 1234:1::2 maximum 3 50
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
int lo1
 vrf for v1
 ipv4 addr 2.2.2.12 255.255.255.255
 ipv6 addr 4321::12 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
 exit
int lo2
 vrf for v1
 ipv4 addr 2.2.2.22 255.255.255.255
 ipv6 addr 4321::22 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
 exit
int lo3
 vrf for v1
 ipv4 addr 2.2.2.32 255.255.255.255
 ipv6 addr 4321::32 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
 exit
int lo4
 vrf for v1
 ipv4 addr 2.2.2.42 255.255.255.255
 ipv6 addr 4321::42 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
 exit
int lo5
 vrf for v1
 ipv4 addr 2.2.2.52 255.255.255.255
 ipv6 addr 4321::52 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
 exit
int lo6
 vrf for v1
 ipv4 addr 2.2.2.62 255.255.255.255
 ipv6 addr 4321::62 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
 exit
int lo7
 vrf for v1
 ipv4 addr 2.2.2.72 255.255.255.255
 ipv6 addr 4321::72 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
 exit
int lo8
 vrf for v1
 ipv4 addr 2.2.2.82 255.255.255.255
 ipv6 addr 4321::82 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
 exit
int eth1
 vrf for v1
 ipv4 addr 1.1.1.2 255.255.255.252
 ipv6 addr 1234:1::2 ffff:ffff::
 exit
router bgp4 1
 vrf v1
 address uni
 local-as 2
 router-id 4.4.4.2
 neigh 1.1.1.1 remote-as 1
 aggregate 2.2.2.0/24 summary
 red conn
 exit
router bgp6 1
 vrf v1
 address uni
 local-as 2
 router-id 6.6.6.2
 neigh 1234:1::1 remote-as 1
 aggregate 4321::/32 summary
 red conn
 exit
!


r1 tping 100 60 2.2.2.2 /vrf v1
r1 tping 100 60 4321::2 /vrf v1
r1 tping 100 60 2.2.2.12 /vrf v1
r1 tping 100 60 4321::12 /vrf v1
r1 tping 100 60 2.2.2.22 /vrf v1
r1 tping 100 60 4321::22 /vrf v1
r1 tping 100 60 2.2.2.32 /vrf v1
r1 tping 100 60 4321::32 /vrf v1
r1 tping 100 60 2.2.2.42 /vrf v1
r1 tping 100 60 4321::42 /vrf v1
r1 tping 100 60 2.2.2.52 /vrf v1
r1 tping 100 60 4321::52 /vrf v1
r1 tping 100 60 2.2.2.62 /vrf v1
r1 tping 100 60 4321::62 /vrf v1
r1 tping 100 60 2.2.2.72 /vrf v1
r1 tping 100 60 4321::72 /vrf v1
r1 tping 100 60 2.2.2.82 /vrf v1
r1 tping 100 60 4321::82 /vrf v1

r2 tping 100 60 2.2.2.1 /vrf v1
r2 tping 100 60 4321::1 /vrf v1
r2 tping 100 60 2.2.2.11 /vrf v1
r2 tping 100 60 4321::11 /vrf v1
r2 tping 100 60 2.2.2.21 /vrf v1
r2 tping 100 60 4321::21 /vrf v1

r2 send conf t
r2 send router bgp4 1
r2 send no aggregate 2.2.2.0/24 summary
r2 send exit
r2 send router bgp6 1
r2 send no aggregate 4321::/32 summary
r2 send end

r1 tping 0 60 2.2.2.2 /vrf v1
r1 tping 0 60 4321::2 /vrf v1
r1 tping 0 60 2.2.2.12 /vrf v1
r1 tping 0 60 4321::12 /vrf v1
r1 tping 0 60 2.2.2.22 /vrf v1
r1 tping 0 60 4321::22 /vrf v1

r2 tping 0 60 2.2.2.1 /vrf v1
r2 tping 0 60 4321::1 /vrf v1
r2 tping 0 60 2.2.2.11 /vrf v1
r2 tping 0 60 4321::11 /vrf v1
r2 tping 0 60 2.2.2.21 /vrf v1
r2 tping 0 60 4321::21 /vrf v1

r2 send conf t
r2 send router bgp4 1
r2 send aggregate 2.2.2.0/24 summary
r2 send exit
r2 send router bgp6 1
r2 send aggregate 4321::/32 summary
r2 send end

r1 tping 100 60 2.2.2.2 /vrf v1
r1 tping 100 60 4321::2 /vrf v1
r1 tping 100 60 2.2.2.12 /vrf v1
r1 tping 100 60 4321::12 /vrf v1
r1 tping 100 60 2.2.2.22 /vrf v1
r1 tping 100 60 4321::22 /vrf v1
r1 tping 100 60 2.2.2.32 /vrf v1
r1 tping 100 60 4321::32 /vrf v1
r1 tping 100 60 2.2.2.42 /vrf v1
r1 tping 100 60 4321::42 /vrf v1
r1 tping 100 60 2.2.2.52 /vrf v1
r1 tping 100 60 4321::52 /vrf v1
r1 tping 100 60 2.2.2.62 /vrf v1
r1 tping 100 60 4321::62 /vrf v1
r1 tping 100 60 2.2.2.72 /vrf v1
r1 tping 100 60 4321::72 /vrf v1
r1 tping 100 60 2.2.2.82 /vrf v1
r1 tping 100 60 4321::82 /vrf v1

r2 tping 100 60 2.2.2.1 /vrf v1
r2 tping 100 60 4321::1 /vrf v1
r2 tping 100 60 2.2.2.11 /vrf v1
r2 tping 100 60 4321::11 /vrf v1
r2 tping 100 60 2.2.2.21 /vrf v1
r2 tping 100 60 4321::21 /vrf v1
