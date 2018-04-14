description interop1: eigrp prefix withdraw

addrouter r1
int eth1 eth 0000.0000.1111 $1a$ $1b$
!
vrf def v1
 rd 1:1
 exit
router eigrp4 1
 vrf v1
 router 4.4.4.1
 as 1
 red conn
 exit
router eigrp6 1
 vrf v1
 router 6.6.6.1
 as 1
 red conn
 exit
int eth1
 vrf for v1
 ipv4 addr 1.1.1.1 255.255.255.0
 ipv6 addr fe80::1 ffff::
 router eigrp4 1 ena
 router eigrp6 1 ena
 exit
int lo0
 vrf for v1
 ipv4 addr 2.2.2.1 255.255.255.255
 ipv6 addr 4321::1 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
 exit
!

addother r2
int eth1 eth 0000.0000.2222 $1b$ $1a$
!
ip routing
ipv6 unicast-routing
interface loopback0
 ip addr 2.2.2.2 255.255.255.255
 ipv6 addr 4321::2/128
 exit
router eigrp 1
 network 1.0.0.0
 redistribute connected
 exit
ipv6 router eigrp 1
 redistribute connected
 exit
interface gigabit1
 ip address 1.1.1.2 255.255.255.0
 ipv6 enable
 ipv6 eigrp 1
 no shutdown
 exit
!


r1 tping 100 10 1.1.1.2 /vrf v1
r1 tping 100 60 2.2.2.2 /vrf v1 /int lo0
!r1 tping 100 60 4321::2 /vrf v1 /int lo0

r1 send conf t
r1 send router eigrp4 1
r1 send no red conn
r1 send exit
r1 send router eigrp6 1
r1 send no red conn
r1 send end

r1 tping 0 60 2.2.2.2 /vrf v1 /int lo0
!r1 tping 0 60 4321::2 /vrf v1 /int lo0

r1 send conf t
r1 send router eigrp4 1
r1 send red conn
r1 send exit
r1 send router eigrp6 1
r1 send red conn
r1 send end

r1 tping 100 60 2.2.2.2 /vrf v1 /int lo0
!r1 tping 100 60 4321::2 /vrf v1 /int lo0
