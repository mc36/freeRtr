description qos ingress dscp set

addrouter r1
int eth1 eth 0000.0000.1111 $1a$ $1b$
!
vrf def v1
 rd 1:1
 exit
policy-map p1
 seq 10 act drop
  match dscp 12
 seq 20 act trans
 exit
int eth1
 vrf for v1
 ipv4 addr 1.1.1.1 255.255.255.0
 ipv6 addr 1234::1 ffff::
 service-policy-in p1
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
policy-map p1
 seq 10 act trans
  match length 300-500
  set dscp set 12
 seq 20 act trans
  set dscp set 11
 exit
int eth1
 vrf for v1
 ipv4 addr 1.1.1.2 255.255.255.0
 ipv6 addr 1234::2 ffff::
 service-policy-in p1
 exit
int eth2
 vrf for v1
 ipv4 addr 2.2.2.1 255.255.255.0
 ipv6 addr 4321::1 ffff::
 service-policy-in p1
 exit
!

addrouter r3
int eth1 eth 0000.0000.3333 $2b$ $2a$
!
vrf def v1
 rd 1:1
 exit
policy-map p1
 seq 10 act drop
  match dscp 12
 seq 20 act trans
 exit
int eth1
 vrf for v1
 ipv4 addr 2.2.2.2 255.255.255.0
 ipv6 addr 4321::2 ffff::
 service-policy-in p1
 exit
ipv4 route v1 0.0.0.0 0.0.0.0 2.2.2.1
ipv6 route v1 :: :: 4321::1
!


r1 tping 100 5 2.2.2.2 /vrf v1 /siz 200
r3 tping 100 5 1.1.1.1 /vrf v1 /siz 200
r1 tping 100 5 4321::2 /vrf v1 /siz 200
r3 tping 100 5 1234::1 /vrf v1 /siz 200

r1 tping 0 5 2.2.2.2 /vrf v1 /siz 400
r3 tping 0 5 1.1.1.1 /vrf v1 /siz 400
r1 tping 0 5 4321::2 /vrf v1 /siz 400
r3 tping 0 5 1234::1 /vrf v1 /siz 400

r1 tping 100 5 2.2.2.2 /vrf v1 /siz 600
r3 tping 100 5 1.1.1.1 /vrf v1 /siz 600
r1 tping 100 5 4321::2 /vrf v1 /siz 600
r3 tping 100 5 1234::1 /vrf v1 /siz 600
