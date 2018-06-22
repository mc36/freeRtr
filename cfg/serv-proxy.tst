description proxy server

addrouter r1
int eth1 eth 0000.0000.1111 $1a$ $1b$
!
vrf def v1
 rd 1:1
 exit
int eth1
 vrf for v1
 ipv4 addr 1.1.1.1 255.255.255.0
 ipv6 addr 1234::1 ffff::
 exit
proxy-profile p1
 protocol http
 vrf v1
 target 1.1.1.2
 port 80
 exit
!

addrouter r2
int eth1 eth 0000.0000.2222 $1b$ $1a$
int eth2 eth 0000.0000.2222 $2a$ $2b$
!
vrf def v1
 rd 1:1
 exit
proxy-profile p1
 vrf v1
 exit
int eth1
 vrf for v1
 ipv4 addr 1.1.1.2 255.255.255.0
 ipv6 addr 1234::2 ffff::
 exit
int eth2
 vrf for v1
 ipv4 addr 2.2.2.2 255.255.255.0
 ipv6 addr 4321::2 ffff::
 exit
server http http
 vrf v1
 proxy p1
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
 ipv4 addr 2.2.2.3 255.255.255.0
 ipv6 addr 4321::3 ffff::
 exit
int lo0
 vrf for v1
 ipv4 addr 3.3.3.3 255.255.255.255
 ipv6 addr 3333::3 ffff::
 exit
server telnet telnet
 vrf v1
 exit
!


r2 tping 100 5 1.1.1.1 /vrf v1
r2 tping 100 5 2.2.2.3 /vrf v1

r1 tping 0 5 3.3.3.3 /vrf v1
r1 send telnet 2.2.2.3 /prox p1
r1 tping 100 5 3.3.3.3 /vrf v1
r1 send exit
r1 read closed

r1 tping 0 5 3.3.3.3 /vrf v1
r1 send telnet 4321::3 /prox p1
r1 tping 100 5 3.3.3.3 /vrf v1
r1 send exit
r1 read closed
