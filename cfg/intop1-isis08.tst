description interop1: isis authentication

addrouter r1
int eth1 eth 0000.0000.1111 $1a$ $1b$
int eth2 eth 0000.0000.3333 $2a$ $2b$
!
vrf def v1
 rd 1:1
 exit
router isis4 1
 vrf v1
 net 48.4444.0000.1111.00
 red conn
 exit
router isis6 1
 vrf v1
 net 48.6666.0000.1111.00
 red conn
 exit
int eth1
 vrf for v1
 ipv4 addr 1.1.1.1 255.255.255.0
 router isis4 1 ena
 router isis4 1 password tester
 exit
int eth2
 vrf for v1
 ipv6 addr fe80::1 ffff::
 router isis6 1 ena
 router isis6 1 password tester
 exit
int lo0
 vrf for v1
 ipv4 addr 2.2.2.1 255.255.255.255
 ipv6 addr 4321::1 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
 exit
!

addother r2
int eth1 eth 0000.0000.2211 $1b$ $1a$
int eth2 eth 0000.0000.2222 $2b$ $2a$
!
ip routing
ipv6 unicast-routing
interface loopback0
 ip addr 2.2.2.2 255.255.255.255
 ipv6 addr 4321::2/128
 exit
router isis
 net 48.0000.0000.1234.00
 metric-style wide
 redistribute connected
 address-family ipv6
  redistribute connected
 exit
key chain kc
 key 0
  key-string tester
 exit
interface gigabit2
 ip address 1.1.1.2 255.255.255.0
 isis network point-to-point
 isis authentication key-chain kc
 isis authentication mode text
 ip router isis
 no shutdown
 exit
interface gigabit1
 ipv6 enable
 isis network point-to-point
 isis authentication key-chain kc
 isis authentication mode text
 ipv6 router isis
 no shutdown
 exit
!


r1 tping 100 10 1.1.1.2 /vrf v1
r1 tping 100 60 2.2.2.2 /vrf v1 /int lo0
r1 tping 100 60 4321::2 /vrf v1 /int lo0
