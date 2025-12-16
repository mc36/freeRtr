description qos policer flowspec

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
access-list a4
 permit 1 any all any all
 exit
access-list a6
 permit 58 any all any all
 exit
policy-map p4
 seq 10 act pol
  access-rate 163840
  match access-group a4
 exit
policy-map p6
 seq 10 act pol
  access-rate 163840
  match access-group a6
 exit
vrf def v1
 rd 1:1
 exit
int eth1
 vrf for v1
 ipv4 addr 1.1.1.2 255.255.255.0
 ipv6 addr 1234::2 ffff::
 exit
router bgp4 1
 vrf v1
 flowspec-install
 flowspec-advert p4
 exit
router bgp6 1
 vrf v1
 flowspec-install
 flowspec-advert p6
 exit
!


r2 tping 100 5 1.1.1.1 vrf v1
r2 tping 100 5 1234::1 vrf v1

r2 tping 85-95 5 1.1.1.1 vrf v1 rep 100 tim 500 siz 100
r2 tping 85-95 5 1234::1 vrf v1 rep 100 tim 500 siz 100
r1 tping 85-95 5 1.1.1.2 vrf v1 rep 100 tim 500 siz 100
r1 tping 85-95 5 1234::2 vrf v1 rep 100 tim 500 siz 100
