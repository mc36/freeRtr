description ipv4 over gtp

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
int lo0
 vrf for v1
 ipv4 addr 4.4.4.4 255.255.255.255
 ipv6 addr 4444::4 ffff::
 exit
ipv4 pool p4 2.2.2.1 0.0.0.1 254
ipv6 pool p6 2222::1111 ::1 254
int di1
 enc iponly
 vrf for v1
 ipv4 addr 2.2.2.0 255.255.255.255
 ipv4 pool p4
 exit
server gtp gtp
 clone di1
 vrf v1
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
int eth1
 vrf for v1
 ipv4 addr 1.1.1.2 255.255.255.0
 ipv6 addr 1234::2 ffff::
 exit
int di1
 enc raw
 vrf for v1
 ipv4 addr 3.3.3.3 0.0.0.0
 ipv6 addr 3333::3333 ::
 exit
vpdn gtp
 int di1
 proxy p1
 tar 1.1.1.1
 called inet
 calling 4321
 dir in
 prot gtp
 exit
!


r2 tping 100 60 4.4.4.4 /vrf v1
r2 tping 0 5 4444::4 /vrf v1
