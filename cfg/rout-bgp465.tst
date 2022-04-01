description vpns ingress route filtering with routepolicy

addrouter r1
int eth1 eth 0000.0000.1111 $1a$ $1b$
!
vrf def v1
 rd 1:1
 label-mode per-prefix
 exit
vrf def v2
 rd 1:2
 rt-both 1:2
 exit
vrf def v3
 rd 1:3
 rt-both 1:3
 exit
vrf def v4
 rd 1:4
 rt-both 1:4
 exit
int lo0
 vrf for v1
 ipv4 addr 2.2.2.1 255.255.255.255
 ipv6 addr 4321::1 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
 exit
int lo21
 vrf for v2
 ipv4 addr 9.9.2.1 255.255.255.255
 ipv6 addr 9992::1 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
 exit
int lo22
 vrf for v2
 ipv4 addr 9.9.2.11 255.255.255.255
 ipv6 addr 9992::11 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
 exit
int lo23
 vrf for v2
 ipv4 addr 9.9.2.111 255.255.255.255
 ipv6 addr 9992::111 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
 exit
int lo31
 vrf for v3
 ipv4 addr 9.9.3.1 255.255.255.255
 ipv6 addr 9993::1 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
 exit
int lo32
 vrf for v3
 ipv4 addr 9.9.3.11 255.255.255.255
 ipv6 addr 9993::11 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
 exit
int lo33
 vrf for v3
 ipv4 addr 9.9.3.111 255.255.255.255
 ipv6 addr 9993::111 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
 exit
int lo41
 vrf for v4
 ipv4 addr 9.9.4.1 255.255.255.255
 ipv6 addr 9994::1 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
 exit
int lo42
 vrf for v4
 ipv4 addr 9.9.4.11 255.255.255.255
 ipv6 addr 9994::11 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
 exit
int lo43
 vrf for v4
 ipv4 addr 9.9.4.111 255.255.255.255
 ipv6 addr 9994::111 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
 exit
int eth1
 vrf for v1
 ipv4 addr 1.1.1.1 255.255.255.252
 ipv6 addr 1234:1::1 ffff:ffff::
 mpls enable
 mpls ldp4
 mpls ldp6
 exit
ipv4 route v1 2.2.2.2 255.255.255.255 1.1.1.2
ipv6 route v1 4321::2 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff 1234:1::2
route-policy p4
 if network 9.9.2.22/32
  drop
 enif
 if network 9.9.3.22/32
  drop
 enif
 if network 9.9.4.22/32
  drop
 enif
 if network 0.0.0.0/0 le 32
  pass
 enif
 exit
route-policy p6
 if network 9992::22/128
  drop
 enif
 if network 9993::22/128
  drop
 enif
 if network 9994::22/128
  drop
 enif
 if network ::/0 le 128
  pass
 enif
 exit
router bgp4 1
 vrf v1
 no safe-ebgp
 address vpnuni
 local-as 1
 router-id 4.4.4.1
 neigh 2.2.2.2 remote-as 2
 neigh 2.2.2.2 update lo0
 neigh 2.2.2.2 send-comm both
 neigh 2.2.2.2 vpn-route-policy-in p4
 afi-vrf v2 ena
 afi-vrf v2 red conn
 afi-vrf v3 ena
 afi-vrf v3 red conn
 afi-vrf v4 ena
 afi-vrf v4 red conn
 exit
router bgp6 1
 vrf v1
 no safe-ebgp
 address vpnuni
 local-as 1
 router-id 6.6.6.1
 neigh 4321::2 remote-as 2
 neigh 4321::2 update lo0
 neigh 4321::2 send-comm both
 neigh 4321::2 vpn-route-policy-in p6
 afi-vrf v2 ena
 afi-vrf v2 red conn
 afi-vrf v3 ena
 afi-vrf v3 red conn
 afi-vrf v4 ena
 afi-vrf v4 red conn
 exit
!

addrouter r2
int eth1 eth 0000.0000.2222 $1b$ $1a$
!
vrf def v1
 rd 1:1
 label-mode per-prefix
 exit
vrf def v2
 rd 1:2
 rt-both 1:2
 exit
vrf def v3
 rd 1:3
 rt-both 1:3
 exit
vrf def v4
 rd 1:4
 rt-both 1:4
 exit
int lo0
 vrf for v1
 ipv4 addr 2.2.2.2 255.255.255.255
 ipv6 addr 4321::2 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
 exit
int lo21
 vrf for v2
 ipv4 addr 9.9.2.2 255.255.255.255
 ipv6 addr 9992::2 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
 exit
int lo22
 vrf for v2
 ipv4 addr 9.9.2.22 255.255.255.255
 ipv6 addr 9992::22 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
 exit
int lo23
 vrf for v2
 ipv4 addr 9.9.2.222 255.255.255.255
 ipv6 addr 9992::222 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
 exit
int lo31
 vrf for v3
 ipv4 addr 9.9.3.2 255.255.255.255
 ipv6 addr 9993::2 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
 exit
int lo32
 vrf for v3
 ipv4 addr 9.9.3.22 255.255.255.255
 ipv6 addr 9993::22 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
 exit
int lo33
 vrf for v3
 ipv4 addr 9.9.3.222 255.255.255.255
 ipv6 addr 9993::222 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
 exit
int lo41
 vrf for v4
 ipv4 addr 9.9.4.2 255.255.255.255
 ipv6 addr 9994::2 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
 exit
int lo42
 vrf for v4
 ipv4 addr 9.9.4.22 255.255.255.255
 ipv6 addr 9994::22 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
 exit
int lo43
 vrf for v4
 ipv4 addr 9.9.4.222 255.255.255.255
 ipv6 addr 9994::222 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
 exit
int eth1
 vrf for v1
 ipv4 addr 1.1.1.2 255.255.255.252
 ipv6 addr 1234:1::2 ffff:ffff::
 mpls enable
 mpls ldp4
 mpls ldp6
 exit
ipv4 route v1 2.2.2.1 255.255.255.255 1.1.1.1
ipv6 route v1 4321::1 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff 1234:1::1
router bgp4 1
 vrf v1
 no safe-ebgp
 address vpnuni
 local-as 2
 router-id 4.4.4.2
 neigh 2.2.2.1 remote-as 1
 neigh 2.2.2.1 update lo0
 neigh 2.2.2.1 send-comm both
 afi-vrf v2 ena
 afi-vrf v2 red conn
 afi-vrf v3 ena
 afi-vrf v3 red conn
 afi-vrf v4 ena
 afi-vrf v4 red conn
 exit
router bgp6 1
 vrf v1
 no safe-ebgp
 address vpnuni
 local-as 2
 router-id 6.6.6.2
 neigh 4321::1 remote-as 1
 neigh 4321::1 update lo0
 neigh 4321::1 send-comm both
 afi-vrf v2 ena
 afi-vrf v2 red conn
 afi-vrf v3 ena
 afi-vrf v3 red conn
 afi-vrf v4 ena
 afi-vrf v4 red conn
 exit
!





r1 tping 100 60 2.2.2.2 /vrf v1 /int lo0
r1 tping 100 60 4321::2 /vrf v1 /int lo0

r2 tping 100 60 2.2.2.1 /vrf v1 /int lo0
r2 tping 100 60 4321::1 /vrf v1 /int lo0

r1 tping 100 60 9.9.2.2 /vrf v2
r2 tping 100 60 9.9.2.1 /vrf v2
r1 tping 100 60 9992::2 /vrf v2
r2 tping 100 60 9992::1 /vrf v2

r1 tping 100 60 9.9.3.2 /vrf v3
r2 tping 100 60 9.9.3.1 /vrf v3
r1 tping 100 60 9993::2 /vrf v3
r2 tping 100 60 9993::1 /vrf v3

r1 tping 100 60 9.9.4.2 /vrf v4
r2 tping 100 60 9.9.4.1 /vrf v4
r1 tping 100 60 9994::2 /vrf v4
r2 tping 100 60 9994::1 /vrf v4

r1 tping 0 60 9.9.2.22 /vrf v2
r2 tping 100 60 9.9.2.11 /vrf v2
r1 tping 0 60 9992::22 /vrf v2
r2 tping 100 60 9992::11 /vrf v2

r1 tping 0 60 9.9.3.22 /vrf v3
r2 tping 100 60 9.9.3.11 /vrf v3
r1 tping 0 60 9993::22 /vrf v3
r2 tping 100 60 9993::11 /vrf v3

r1 tping 0 60 9.9.4.22 /vrf v4
r2 tping 100 60 9.9.4.11 /vrf v4
r1 tping 0 60 9994::22 /vrf v4
r2 tping 100 60 9994::11 /vrf v4

r1 tping 100 60 9.9.2.222 /vrf v2
r2 tping 100 60 9.9.2.111 /vrf v2
r1 tping 100 60 9992::222 /vrf v2
r2 tping 100 60 9992::111 /vrf v2

r1 tping 100 60 9.9.3.222 /vrf v3
r2 tping 100 60 9.9.3.111 /vrf v3
r1 tping 100 60 9993::222 /vrf v3
r2 tping 100 60 9993::111 /vrf v3

r1 tping 100 60 9.9.4.222 /vrf v4
r2 tping 100 60 9.9.4.111 /vrf v4
r1 tping 100 60 9994::222 /vrf v4
r2 tping 100 60 9994::111 /vrf v4
