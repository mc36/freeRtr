description interop1: rip prefix withdraw

addrouter r1
int eth1 eth 0000.0000.1111 $1a$ $1b$
!
vrf def v1
 rd 1:1
 exit
router rip4 1
 vrf v1
 red conn
 exit
router rip6 1
 vrf v1
 red conn
 exit
int eth1
 vrf for v1
 ipv4 addr 1.1.1.1 255.255.255.0
 ipv6 addr fe80::1 ffff::
 router rip4 1 ena
 router rip4 1 update-time 5000
 router rip4 1 hold-time 15000
 router rip4 1 flush-time 15000
 router rip6 1 ena
 router rip6 1 update-time 5000
 router rip6 1 hold-time 15000
 router rip6 1 flush-time 15000
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
router rip
 version 2
 redistribute connected
 no auto-summary
 timers basic 5 15 15 15
 network 1.0.0.0
 exit
ipv6 router rip 1
 redistribute connected
 timers 5 15 15 15
 exit
interface gigabit1
 ip address 1.1.1.2 255.255.255.0
 ipv6 enable
 ipv6 rip 1 enable
 no shutdown
 exit
!


r1 tping 100 10 1.1.1.2 /vrf v1
r1 tping 100 120 2.2.2.2 /vrf v1 /int lo0
r1 tping 100 120 4321::2 /vrf v1 /int lo0

r1 send conf t
r1 send router rip4 1
r1 send no red conn
r1 send exit
r1 send router rip6 1
r1 send no red conn
r1 send end

r1 tping 0 60 2.2.2.2 /vrf v1 /int lo0
r1 tping 0 60 4321::2 /vrf v1 /int lo0

r1 send conf t
r1 send router rip4 1
r1 send red conn
r1 send exit
r1 send router rip6 1
r1 send red conn
r1 send end

r1 tping 100 60 2.2.2.2 /vrf v1 /int lo0
r1 tping 100 60 4321::2 /vrf v1 /int lo0
