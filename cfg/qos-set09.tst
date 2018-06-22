description qos ingress exp set

addrouter r1
int eth1 eth 0000.0000.1111 $1a$ $1b$
!
vrf def v1
 rd 1:1
 label-mode per-prefix
 exit
policy-map p1
 seq 10 act drop
  match exp 4
 seq 20 act trans
 exit
int lo0
 vrf for v1
 ipv4 addr 3.3.3.1 255.255.255.255
 ipv6 addr 3333::1 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
 exit
int eth1
 vrf for v1
 ipv4 addr 1.1.1.1 255.255.255.0
 ipv6 addr 1234::1 ffff::
 mpls enable
 mpls ldp4
 mpls ldp6
 service-policy-in p1
 exit
ipv4 route v1 3.3.3.2 255.255.255.255 1.1.1.2
ipv6 route v1 3333::2 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff 1234::2
!

addrouter r2
int eth1 eth 0000.0000.2222 $1b$ $1a$
int eth2 eth 0000.0000.2222 $2a$ $2b$
!
vrf def v1
 rd 1:1
 label-mode per-prefix
 exit
policy-map p1
 seq 10 act trans
  match length 300-500
  set exp set 4
 seq 20 act trans
  set exp set 5
 exit
int eth1
 vrf for v1
 ipv4 addr 1.1.1.2 255.255.255.0
 ipv6 addr 1234::2 ffff::
 mpls enable
 mpls ldp4
 mpls ldp6
 service-policy-in p1
 exit
int eth2
 vrf for v1
 ipv4 addr 2.2.2.1 255.255.255.0
 ipv6 addr 4321::1 ffff::
 mpls enable
 mpls ldp4
 mpls ldp6
 service-policy-in p1
 exit
ipv4 route v1 3.3.3.1 255.255.255.255 1.1.1.1
ipv6 route v1 3333::1 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff 1234::1
ipv4 route v1 3.3.3.2 255.255.255.255 2.2.2.2
ipv6 route v1 3333::2 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff 4321::2
!

addrouter r3
int eth1 eth 0000.0000.3333 $2b$ $2a$
!
vrf def v1
 rd 1:1
 label-mode per-prefix
 exit
policy-map p1
 seq 10 act drop
  match exp 4
 seq 20 act trans
 exit
int lo0
 vrf for v1
 ipv4 addr 3.3.3.2 255.255.255.255
 ipv6 addr 3333::2 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
 exit
int eth1
 vrf for v1
 ipv4 addr 2.2.2.2 255.255.255.0
 ipv6 addr 4321::2 ffff::
 mpls enable
 mpls ldp4
 mpls ldp6
 service-policy-in p1
 exit
ipv4 route v1 3.3.3.1 255.255.255.255 2.2.2.1
ipv6 route v1 3333::1 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff 4321::1
!


r1 tping 100 5 3.3.3.2 /vrf v1 /int lo0 /siz 200
r3 tping 100 5 3.3.3.1 /vrf v1 /int lo0 /siz 200
r1 tping 100 5 3333::2 /vrf v1 /int lo0 /siz 200
r3 tping 100 5 3333::1 /vrf v1 /int lo0 /siz 200

r1 tping 0 15 3.3.3.2 /vrf v1 /int lo0 /siz 400
r3 tping 0 15 3.3.3.1 /vrf v1 /int lo0 /siz 400
r1 tping 0 15 3333::2 /vrf v1 /int lo0 /siz 400
r3 tping 0 15 3333::1 /vrf v1 /int lo0 /siz 400

r1 tping 100 5 3.3.3.2 /vrf v1 /int lo0 /siz 600
r3 tping 100 5 3.3.3.1 /vrf v1 /int lo0 /siz 600
r1 tping 100 5 3333::2 /vrf v1 /int lo0 /siz 600
r3 tping 100 5 3333::1 /vrf v1 /int lo0 /siz 600
