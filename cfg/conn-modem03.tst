description modem with alaw through proxy

addrouter r1
int eth1 eth 0000.0000.1111 $1a$ $1b$
!
vrf def v1
 rd 1:1
 exit
int lo0
 vrf for v1
 ipv4 addr 2.2.2.2 255.255.255.255
 exit
int eth1
 vrf for v1
 ipv4 addr 1.1.1.1 255.255.255.0
 ipv6 addr 1234::1 ffff::
 exit
server modem sm
 codec alaw
 vrf v1
 exit
!

addrouter r2
int eth1 eth 0000.0000.2222 $1b$ $1a$
int eth2 eth 0000.0000.2222 $2a$ $2b$
!
vrf def v1
 rd 1:1
 exit
int eth1
 vrf for v1
 ipv4 addr 1.1.1.2 255.255.255.0
 ipv6 addr 1234::2 ffff::
 exit
int eth2
 vrf for v1
 ipv4 addr 1.1.2.1 255.255.255.0
 ipv6 addr 2345::1 ffff::
 exit
dial-peer 1
 codec alaw
 match-calling .*
 match-called .*
 vrf v1
 myname 99
 target 1.1.1.1
 direction out
 exit
server sip sp
 vrf v1
 exit
!

addrouter r3
int eth1 eth 0000.0000.3333 $2b$ $2a$
!
vrf def v1
 rd 1:1
 exit
int eth1
 vrf for v1
 ipv4 addr 1.1.2.2 255.255.255.0
 ipv6 addr 2345::2 ffff::
 exit
dial-peer 1
 codec alaw
 match-calling .*
 match-called .*
 vrf v1
 myname 99
 register 1000
 target 1.1.2.1
 direction out
 exit
!


r2 tping 100 5 1.1.1.1 /vrf v1
r2 tping 100 5 1.1.2.2 /vrf v1
r3 send pack modem 11 22
r3 tping 100 5 2.2.2.2 /vrf v1
