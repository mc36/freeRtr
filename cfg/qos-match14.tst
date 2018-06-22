description qos egress matcher on bridged traffic

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
ipv4 route v1 0.0.0.0 0.0.0.0 1.1.1.2
ipv6 route v1 :: :: 1234::2
!

addrouter r2
int eth1 eth 0000.0000.2222 $1b$ $1a$
int eth2 eth 0000.0000.2222 $2a$ $2b$
!
vrf def v1
 rd 1:1
 exit
bridge 1
 exit
int bvi1
 vrf for v1
 ipv4 addr 1.1.1.3 255.255.255.0
 ipv6 addr 1234::3 ffff::
 exit
policy-map p1
 seq 10 act drop
  match length 300-500
 seq 20 act trans
 exit
int eth1
 bridge-gr 1
 service-policy-out p1
 exit
int eth2
 bridge-gr 1
 service-policy-out p1
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
 ipv4 addr 1.1.1.2 255.255.255.0
 ipv6 addr 1234::2 ffff::
 exit
!


r1 tping 100 5 1.1.1.2 /vrf v1 /siz 200
r3 tping 100 5 1.1.1.1 /vrf v1 /siz 200
r1 tping 100 5 1234::2 /vrf v1 /siz 200
r3 tping 100 5 1234::1 /vrf v1 /siz 200

r1 tping 0 5 1.1.1.2 /vrf v1 /siz 400
r3 tping 0 5 1.1.1.1 /vrf v1 /siz 400
r1 tping 0 5 1234::2 /vrf v1 /siz 400
r3 tping 0 5 1234::1 /vrf v1 /siz 400

r1 tping 100 5 1.1.1.2 /vrf v1 /siz 600
r3 tping 100 5 1.1.1.1 /vrf v1 /siz 600
r1 tping 100 5 1234::2 /vrf v1 /siz 600
r3 tping 100 5 1234::1 /vrf v1 /siz 600

r2 tping 100 5 1.1.1.2 /vrf v1 /siz 200
r2 tping 100 5 1.1.1.1 /vrf v1 /siz 200
r2 tping 100 5 1234::2 /vrf v1 /siz 200
r2 tping 100 5 1234::1 /vrf v1 /siz 200

r2 tping 0 5 1.1.1.2 /vrf v1 /siz 400
r2 tping 0 5 1.1.1.1 /vrf v1 /siz 400
r2 tping 0 5 1234::2 /vrf v1 /siz 400
r2 tping 0 5 1234::1 /vrf v1 /siz 400

r2 tping 100 5 1.1.1.2 /vrf v1 /siz 600
r2 tping 100 5 1.1.1.1 /vrf v1 /siz 600
r2 tping 100 5 1234::2 /vrf v1 /siz 600
r2 tping 100 5 1234::1 /vrf v1 /siz 600
