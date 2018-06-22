description loadbalancing bundle

addrouter r1
int ser1 ser 0000.0000.1111 $1a$ $1b$
int ser2 ser 0000.0000.1111 $2a$ $2b$
!
vrf def v1
 rd 1:1
 exit
bundle 1
 no ethernet
 loadbalance layer3
 exit
int ser1
 bundle-gr 1
 exit
int ser2
 bundle-gr 1
 exit
int bun1
 vrf for v1
 ipv4 addr 1.1.1.1 255.255.255.0
 ipv6 addr 1234::1 ffff::
 exit
!

addrouter r2
int ser1 ser 0000.0000.2222 $1b$ $1a$
!
vrf def v1
 rd 1:1
 exit
int ser1
 vrf for v1
 ipv4 addr 1.1.1.2 255.255.255.0
 ipv6 addr 1234::2 ffff::
 exit
int lo0
 vrf for v1
 ipv4 addr 2.2.2.1 255.255.255.255
 ipv6 addr 4321::1 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
 exit
server telnet tel
 vrf v1
 exit
!

addrouter r3
int ser1 ser 0000.0000.2222 $2b$ $2a$
!
vrf def v1
 rd 1:1
 exit
int ser1
 vrf for v1
 ipv4 addr 1.1.1.2 255.255.255.0
 ipv6 addr 1234::2 ffff::
 exit
int lo0
 vrf for v1
 ipv4 addr 2.2.2.1 255.255.255.255
 ipv6 addr 4321::1 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
 exit
server telnet tel
 vrf v1
 exit
!


r1 tping 100 5 1.1.1.2 /vrf v1
r1 tping 100 5 1234::2 /vrf v1

r1 send telnet 1.1.1.2 /vrf v1
r1 tping 100 5 2.2.2.1 /vrf v1
r1 tping 100 5 4321::1 /vrf v1
r1 send exit
r1 read closed

r1 send telnet 1234::2 /vrf v1
r1 tping 100 5 2.2.2.1 /vrf v1
r1 tping 100 5 4321::1 /vrf v1
r1 send exit
r1 read closed
