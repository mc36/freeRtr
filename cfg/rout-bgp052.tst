description bgp roa rpki

addrouter r1
int eth1 eth 0000.0000.2222 $1a$ $1b$
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
 ipv4 addr 2.2.2.111 255.255.255.255
 ipv6 addr 4321::111 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
 exit
int lo2
 vrf for v1
 ipv4 addr 2.2.2.222 255.255.255.255
 ipv6 addr 4321::222 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
 exit
server telnet tel
 vrf v1
 port 666
 exit
int eth1
 vrf for v1
 ipv4 addr 1.1.1.1 255.255.255.252
 ipv6 addr 1234:1::1 ffff:ffff::
 exit
router rpki4 1
 vrf v1
 exit
router rpki6 1
 vrf v1
 exit
router bgp4 1
 vrf v1
 no safe-ebgp
 address uni
 local-as 3
 router-id 4.4.4.1
 rpki rpki4 1
 neigh 1.1.1.2 remote-as 1
 neigh 1.1.1.2 rpki-in acc
 red conn
 exit
router bgp6 1
 vrf v1
 no safe-ebgp
 address uni
 local-as 3
 rpki rpki6 1
 router-id 6.6.6.1
 neigh 1234:1::2 remote-as 1
 neigh 1234:1::2 rpki-in acc
 red conn
 exit
!

addrouter r2
int eth1 eth 0000.0000.1111 $1b$ $1a$
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
prefix-list p4
 sequence 10 deny 2.2.2.222/32
 sequence 20 permit 0.0.0.0/0 le 32
 exit
prefix-list p6
 sequence 10 deny 4321::222/128
 sequence 20 permit ::/0 le 128
 exit
route-map rm1
 set locpref 1234
 exit
route-map rm2
 match validroa 1
 set locpref 4321
 exit
server rpki r
 vrf v1
 prefix 2.2.2.111/24 32 3
 prefix 4321::111/56 128 3
 exit
router rpki4 1
 vrf v1
 neigh 2.2.2.2 port 323
 wakeup bgp4 1
 exit
router rpki6 1
 vrf v1
 neigh 4321::2 port 323
 wakeup bgp6 1
 exit
router bgp4 1
 vrf v1
 no safe-ebgp
 address uni
 local-as 1
 router-id 4.4.4.2
 rpki rpki4 1
 neigh 1.1.1.1 remote-as 3
 neigh 1.1.1.1 rpki-in rew
 neigh 1.1.1.1 send-comm both
 neigh 1.1.1.1 prefix-list-in p4
 neigh 1.1.1.1 route-map-in rm2
 neigh 1.1.1.6 remote-as 2
 neigh 1.1.1.6 rpki-in rew
 neigh 1.1.1.6 send-comm both
 neigh 1.1.1.6 prefix-list-in p4
 neigh 1.1.1.6 route-map-in rm1
 red conn
 exit
router bgp6 1
 vrf v1
 no safe-ebgp
 address uni
 local-as 1
 router-id 6.6.6.2
 rpki rpki6 1
 neigh 1234:1::1 remote-as 3
 neigh 1234:1::1 rpki-in rew
 neigh 1234:1::1 send-comm both
 neigh 1234:1::1 prefix-list-in p6
 neigh 1234:1::1 route-map-in rm2
 neigh 1234:2::2 remote-as 2
 neigh 1234:2::2 rpki-in rew
 neigh 1234:2::2 send-comm both
 neigh 1234:2::2 prefix-list-in p6
 neigh 1234:2::2 route-map-in rm1
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
 ipv6 addr 4321::3 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
 exit
int lo1
 vrf for v1
 ipv4 addr 2.2.2.111 255.255.255.255
 ipv6 addr 4321::111 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
 exit
int eth1
 vrf for v1
 ipv4 addr 1.1.1.6 255.255.255.252
 ipv6 addr 1234:2::2 ffff:ffff::
 exit
router rpki4 1
 vrf v1
 exit
router rpki6 1
 vrf v1
 exit
router bgp4 1
 vrf v1
 no safe-ebgp
 address uni
 local-as 2
 router-id 4.4.4.3
 rpki rpki4 1
 neigh 1.1.1.5 remote-as 1
 neigh 1.1.1.5 rpki-in acc
 red conn
 exit
router bgp6 1
 vrf v1
 no safe-ebgp
 address uni
 local-as 2
 router-id 6.6.6.3
 rpki rpki6 1
 neigh 1234:2::1 remote-as 1
 neigh 1234:2::1 rpki-in acc
 red conn
 exit
!


r1 tping 100 60 2.2.2.2 vrf v1
r1 tping 100 60 4321::2 vrf v1
r1 tping 100 60 2.2.2.3 vrf v1
r1 tping 100 60 4321::3 vrf v1

r3 tping 100 60 2.2.2.1 vrf v1
r3 tping 100 60 4321::1 vrf v1
r3 tping 100 60 2.2.2.2 vrf v1
r3 tping 100 60 4321::2 vrf v1

r2 tping 100 60 2.2.2.1 vrf v1
r2 tping 100 60 4321::1 vrf v1
r2 tping 100 60 2.2.2.3 vrf v1
r2 tping 100 60 4321::3 vrf v1

r2 tping 100 60 2.2.2.111 vrf v1
r2 tping 100 60 4321::111 vrf v1
r2 tping 0 60 2.2.2.222 vrf v1
r2 tping 0 60 4321::222 vrf v1

r2 send telnet 2.2.2.111 666 vrf v1
r2 tping 100 60 2.2.2.222 vrf v1
r2 send exit
r2 read closed
r2 tping 0 60 2.2.2.222 vrf v1

r2 send telnet 4321::111 666 vrf v1
r2 tping 100 60 2.2.2.222 vrf v1
r2 send exit
r2 read closed
r2 tping 0 60 2.2.2.222 vrf v1
