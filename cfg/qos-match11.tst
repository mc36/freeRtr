description qos ingress ethertype matcher

addrouter r1
int eth1 eth 0000.0000.1111 $1a$ $1b$
!
vrf def v1
 rd 1:1
 exit
policy-map p1
 seq 10 act trans
  match ethtyp 34525
 seq 20 act drop
 exit
int eth1
 vrf for v1
 ipv4 addr 3.3.3.1 255.255.255.0
 ipv6 addr 3333::1 ffff::
 service-policy-in p1
 exit
!

addrouter r2
int eth1 eth 0000.0000.2222 $1b$ $1a$
!
vrf def v1
 rd 1:1
 exit
int eth1
 vrf for v1
 ipv4 addr 3.3.3.2 255.255.255.0
 ipv6 addr 3333::2 ffff::
 exit
!


r1 tping 100 5 3.3.3.1 /vrf v1
r1 tping 100 5 3333::1 /vrf v1
r2 tping 100 5 3.3.3.2 /vrf v1
r2 tping 100 5 3333::2 /vrf v1

r2 tping 0 5 3.3.3.1 /vrf v1
r2 tping 100 5 3333::1 /vrf v1
r1 tping 0 5 3.3.3.2 /vrf v1
r1 tping 100 5 3333::2 /vrf v1
