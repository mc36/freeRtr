description ipv4 target port selection

addrouter r1
int eth1 eth 0000.0000.1111 $1a$ $1b$
int eth2 eth 0000.0000.1111 $2a$ $2b$
!
vrf def v1
 rd 1:1
 exit
vrf def v2
 rd 1:1
 exit
int lo1
 vrf for v2
 ipv4 addr 3.3.3.3 255.255.255.255
 exit
int eth1
 vrf for v1
 ipv4 addr 1.1.1.1 255.255.255.252
 ipv6 addr 1234:1::1 ffff:ffff::
 exit
int eth2
 vrf for v1
 ipv4 addr 1.1.1.5 255.255.255.252
 ipv6 addr 1234:2::1 ffff:ffff::
 exit
int tun1
 tun sou eth1
 tun dest 1.1.1.2
 tun vrf v1
 tun dom 1.1.1.2 10000-19999 1.1.1.6 20000-29999
 tun mod aplusp
 vrf for v1
 ipv4 addr 2.2.2.1 255.255.255.252
 ipv6 addr 4321::1234 ffff:ffff::
 exit
server telnet tel
 vrf v1
 port 666
 exit
!

addrouter r2
int eth1 eth 0000.0000.2222 $1b$ $1a$
!
vrf def v1
 rd 1:1
 exit
int eth1
 vrf for v1
 ipv4 addr 1.1.1.2 255.255.255.252
 ipv6 addr 1234:1::2 ffff:ffff::
 exit
int lo1
 vrf for v1
 ipv4 addr 2.2.2.2 255.255.255.255
 ipv6 addr 4321::1 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
 exit
ipv4 route v1 0.0.0.0 0.0.0.0 1.1.1.1
ipv6 route v1 :: :: 1234:1::1
client tcp-portrange 12000 13000
!

addrouter r3
int eth1 eth 0000.0000.3333 $2b$ $2a$
!
vrf def v1
 rd 1:1
 exit
int eth1
 vrf for v1
 ipv4 addr 1.1.1.6 255.255.255.252
 ipv6 addr 1234:2::2 ffff:ffff::
 exit
int lo1
 vrf for v1
 ipv4 addr 2.2.2.2 255.255.255.255
 ipv6 addr 4321::1 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
 exit
ipv4 route v1 0.0.0.0 0.0.0.0 1.1.1.5
ipv6 route v1 :: :: 1234:2::1
client tcp-portrange 22000 23000
!

r2 tping 100 5 1.1.1.1 vrf v1
r2 tping 100 5 1234:1::1 vrf v1
r3 tping 100 5 1.1.1.5 vrf v1
r3 tping 100 5 1234:2::1 vrf v1

r2 send telnet 1.1.1.1 666 vrf v1 sou lo1
r2 tping 100 5 3.3.3.3 vrf v2
r2 send exit
r2 read closed

r3 send telnet 1.1.1.1 666 vrf v1 sou lo1
r3 tping 100 5 3.3.3.3 vrf v2
r3 send exit
r3 read closed
