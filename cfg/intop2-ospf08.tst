description interop2: ospf prefix withdraw

addrouter r1
int eth1 eth 0000.0000.1111 $rem1$
!
vrf def v1
 rd 1:1
 exit
router ospf4 1
 vrf v1
 router 4.4.4.1
 area 0 ena
 red conn
 exit
router ospf6 1
 vrf v1
 router 6.6.6.1
 area 0 ena
 red conn
 exit
int eth1
 vrf for v1
 ipv4 addr 1.1.1.1 255.255.255.0
 ipv6 addr fe80::1 ffff::
 router ospf4 1 ena
 router ospf6 1 ena
 exit
int lo0
 vrf for v1
 ipv4 addr 2.2.2.1 255.255.255.255
 ipv6 addr 4321::1 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
 exit
!

addremote r2
int eth1 eth 0000.0000.2222 $rem1$
!
interface loopback0
 ipv4 addr 2.2.2.2 255.255.255.255
 ipv6 addr 4321::2/128
 exit
interface gigabit0/0/0/0
 ipv4 address 1.1.1.2 255.255.255.0
 ipv6 enable
 no shutdown
 exit
router ospf 1
 redistribute connected
 area 0 interface gigabit0/0/0/0 network point-to-point
router ospfv3 1
 redistribute connected
 area 0 interface gigabit0/0/0/0 network point-to-point
root
commit
!


r1 tping 100 10 1.1.1.2 /vrf v1
r1 tping 100 60 2.2.2.2 /vrf v1 /int lo0
r1 tping 100 60 4321::2 /vrf v1 /int lo0

r1 send conf t
r1 send router ospf4 1
r1 send no red conn
r1 send exit
r1 send router ospf6 1
r1 send no red conn
r1 send end

r1 tping 0 60 2.2.2.2 /vrf v1 /int lo0
r1 tping 0 60 4321::2 /vrf v1 /int lo0

r1 send conf t
r1 send router ospf4 1
r1 send red conn
r1 send exit
r1 send router ospf6 1
r1 send red conn
r1 send end

r1 tping 100 60 2.2.2.2 /vrf v1 /int lo0
r1 tping 100 60 4321::2 /vrf v1 /int lo0
