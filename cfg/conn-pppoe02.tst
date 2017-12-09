description pppoe over bridge encapsulation

addrouter r1
int eth1 eth 0000.0000.1111 $1a$ $1b$
!
vrf def v1
 rd 1:1
 exit
bridge 1
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
 exit
int eth1
 bridge-gr 1
 exit
int bvi1
 p2poe server di1
 exit
!

addrouter r2
int eth1 eth 0000.0000.2222 $1b$ $1a$
!
vrf def v1
 rd 1:1
 exit
bridge 1
 exit
prefix-list p1
 permit 0.0.0.0/0
 exit
int di1
 enc ppp
 vrf for v1
 ipv4 addr 3.3.3.3 255.255.255.128
 ppp ip4cp open
 ppp ip4cp local 0.0.0.0
 ipv4 gateway-prefix p1
 exit
int eth1
 bridge-gr 1
 exit
int bvi1
 p2poe client di1
 exit
!


r2 tping 100 30 2.2.2.0 /vrf v1
r2 tping 100 5 1.1.1.1 /vrf v1
