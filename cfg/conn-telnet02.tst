description telnet inspection

addrouter r1
int eth1 eth 0000.0000.1111 $1a$ $1b$
!
vrf def v1
 rd 1:1
 exit
int lo0
 vrf for v1
 ipv4 addr 1.1.1.1 255.255.255.255
 exit
ipv4 pool p4 2.2.2.1 0.0.0.1 254
int di1
 enc ppp
 vrf for v1
 ipv4 addr 2.2.2.0 255.255.255.255
 ppp ip4cp local 2.2.2.0
 ipv4 pool p4
 ppp ip4cp open
 ipv4 inspect
 exit
int eth1
 vrf for v1
 ipv4 addr 3.3.3.1 255.255.255.252
 ipv4 inspect
 exit
aaa userlist usr
 username c password c
 username c privilege 14
 exit
server tel tel
 vrf v1
 login authen usr
 exec int di1
 exit
!

addrouter r2
int eth1 eth 0000.0000.2222 $1b$ $1a$
!
vrf def v1
 rd 1:1
 exit
proxy-profile p1
 vrf v1
 exit
prefix-list p1
 permit 0.0.0.0/0
 exit
int di1
 enc ppp
 vrf for v1
 ipv4 addr 4.4.4.4 255.255.255.128
 ppp ip4cp open
 ppp ip4cp local 0.0.0.0
 ipv4 gateway-prefix p1
 ipv4 inspect
 exit
int eth1
 vrf for v1
 ipv4 addr 3.3.3.2 255.255.255.252
 ipv4 inspect
 exit
chat-script login
 recv 5000 .*ser
 send c
 binsend 13
 recv 5000 .*ass
 send c
 binsend 13
 send ppp
 binsend 13
 exit
vpdn tel
 interface di1
 proxy p1
 script login
 target 3.3.3.1
 vcid 23
 protocol telnet
 exit
!


r2 tping 100 30 2.2.2.0 /vrf v1
r2 tping 100 5 1.1.1.1 /vrf v1
