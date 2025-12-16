description interop1: isis sha512 authentication

addrouter r1
int eth1 eth 0000.0000.1111 $per1$
int eth2 eth 0000.0000.3333 $per2$
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
 router isis4 1 authen-id 1
 router isis4 1 authen-type sha512
 exit
int eth2
 vrf for v1
 ipv6 addr fe80::1 ffff::
 router isis6 1 ena
 router isis6 1 password tester
 router isis6 1 authen-id 1
 router isis6 1 authen-type sha512
 exit
int lo0
 vrf for v1
 ipv4 addr 2.2.2.1 255.255.255.255
 ipv6 addr 4321::1 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
 exit
!

addpersist r2
int eth1 eth 0000.0000.2222 $per1$
int eth2 eth 0000.0000.2211 $per2$
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
 key 1
  key-string tester
  cryptographic-algorithm hmac-sha-512
 exit
interface gigabit1
 ip address 1.1.1.2 255.255.255.0
 isis network point-to-point
 isis authentication key-chain kc
 ip router isis
 no shutdown
 exit
interface gigabit2
 ipv6 enable
 isis network point-to-point
 isis authentication key-chain kc
 ipv6 router isis
 no shutdown
 exit
!


r1 tping 100 10 1.1.1.2 vrf v1
r1 tping 100 60 2.2.2.2 vrf v1 sou lo0
r1 tping 100 60 4321::2 vrf v1 sou lo0
