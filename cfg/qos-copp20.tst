description qos egress pps copp

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
!

addrouter r2
int eth1 eth 0000.0000.2222 $1b$ $1a$
!
policy-map p1
 seq 10 act pps
  access-rate 1024
 exit
vrf def v1
 rd 1:1
 copp4out p1
 copp6out p1
 exit
int eth1
 vrf for v1
 ipv4 addr 1.1.1.2 255.255.255.0
 ipv6 addr 1234::2 ffff::
 exit
!


r2 tping 100 5 1.1.1.1 vrf v1
r2 tping 100 5 1234::1 vrf v1

r2 tping 85-95 5 1.1.1.1 vrf v1 rep 100 tim 500 siz 100
r2 tping 85-95 5 1234::1 vrf v1 rep 100 tim 500 siz 100
r1 tping 85-95 5 1.1.1.2 vrf v1 rep 100 tim 500 siz 100
r1 tping 85-95 5 1234::2 vrf v1 rep 100 tim 500 siz 100
