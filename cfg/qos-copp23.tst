description qos divert flowspec

addrouter r1
int ser1 ser - $1a$ $1b$
int ser2 ser - $2a$ $2b$
!
vrf def v1
 rd 1:1
 exit
int ser1
 vrf for v1
 ipv4 addr 1.1.1.1 255.255.255.0
 ipv6 addr 1234::1 ffff::
 exit
int ser2
 vrf for v1
 ipv4 addr 1.1.2.1 255.255.255.0
 ipv6 addr 1235::1 ffff::
 exit
!

addrouter r2
int ser1 ser - $1b$ $1a$
int ser2 ser - $2b$ $2a$
int ser3 ser - $3a$ $3b$
int ser4 ser - $4a$ $4b$
!
vrf def v1
 rd 1:1
 exit
vrf def v3
 rd 1:3
 exit
vrf def v2
 rd 1:2
 exit
vrf def v4
 rd 1:4
 exit
access-list a4
 permit 1 any all any all
 exit
access-list a6
 permit 58 any all any all
 exit
policy-map p1
 seq 10 act trans
  match access-group a4
  set vrf v4
 exit
policy-map p2
 seq 10 act trans
  match access-group a4
  set vrf v2
 exit
policy-map p3
 seq 10 act trans
  match access-group a6
  set vrf v4
 exit
policy-map p4
 seq 10 act trans
  match access-group a6
  set vrf v2
 exit
int ser1
 vrf for v1
 ipv4 addr 1.1.1.2 255.255.255.0
 ipv6 addr 1234::2 ffff::
 exit
int ser2
 vrf for v2
 ipv4 addr 1.1.1.2 255.255.255.0
 ipv6 addr 1234::2 ffff::
 exit
int ser3
 vrf for v3
 ipv4 addr 1.1.1.2 255.255.255.0
 ipv6 addr 1234::2 ffff::
 exit
int ser4
 vrf for v4
 ipv4 addr 1.1.1.2 255.255.255.0
 ipv6 addr 1234::2 ffff::
 exit
router bgp4 1
 vrf v1
 flowspec-install
 flowspec-advert p1
 exit
router bgp6 1
 vrf v1
 flowspec-install
 flowspec-advert p3
 exit
router bgp4 3
 vrf v3
 flowspec-install
 flowspec-advert p2
 exit
router bgp6 3
 vrf v3
 flowspec-install
 flowspec-advert p4
 exit
!

addrouter r3
int ser1 ser - $3b$ $3a$
int ser2 ser - $4b$ $4a$
!
vrf def v1
 rd 1:1
 exit
int ser1
 vrf for v1
 ipv4 addr 1.1.1.3 255.255.255.0
 ipv6 addr 1234::3 ffff::
 exit
int ser2
 vrf for v1
 ipv4 addr 1.1.2.3 255.255.255.0
 ipv6 addr 1235::3 ffff::
 exit
!



r3 tping 100 15 1.1.1.1 vrf v1 siz 200
r3 tping 100 15 1234::1 vrf v1 siz 200
r1 tping 100 15 1.1.1.3 vrf v1 siz 200
r1 tping 100 15 1234::3 vrf v1 siz 200
