description bgp autoroute

addrouter r1
int ser1 ser 0000.0000.1111 $1a$ $1b$
int ser2 ser 0000.0000.1111 $2a$ $2b$
!
vrf def v1
 rd 1:1
 label-mode per-prefix
 exit
access-list test4
 deny 1 any all any all
 permit all any all any all
 exit
access-list test6
 deny 58 any all any all
 permit all any all any all
 exit
int lo0
 vrf for v1
 ipv4 addr 2.2.2.1 255.255.255.255
 ipv6 addr 4321::1 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
 exit
int lo1
 vrf for v1
 ipv4 addr 2.2.2.11 255.255.255.255
 ipv6 addr 4321::11 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
 exit
int ser1
 vrf for v1
 ipv4 addr 9.9.9.1 255.255.255.0
 ipv6 addr 9999::1 ffff::
 ipv4 access-group-in test4
 ipv6 access-group-in test6
 exit
int ser2
 vrf for v1
 ipv4 addr 9.9.8.1 255.255.255.0
 ipv6 addr 9998::1 ffff::
 ipv4 autoroute bgp4 1 2.2.2.2 9.9.8.2 recur
 ipv6 autoroute bgp6 1 4321::2 9998::2 recur
 exit
router bgp4 1
 vrf v1
 no safe-ebgp
 address uni
 local-as 1
 router-id 4.4.4.1
 neigh 9.9.9.2 remote-as 2
 red conn
 exit
router bgp6 1
 vrf v1
 no safe-ebgp
 address uni
 local-as 1
 router-id 6.6.6.1
 neigh 9999::2 remote-as 2
 red conn
 exit
!

addrouter r2
int ser1 ser 0000.0000.2222 $1b$ $1a$
int ser2 ser 0000.0000.2222 $2b$ $2a$
!
vrf def v1
 rd 1:1
 label-mode per-prefix
 exit
access-list test4
 deny 1 any all any all
 permit all any all any all
 exit
access-list test6
 deny 58 any all any all
 permit all any all any all
 exit
int lo0
 vrf for v1
 ipv4 addr 2.2.2.2 255.255.255.255
 ipv6 addr 4321::2 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
 exit
int lo1
 vrf for v1
 ipv4 addr 2.2.2.12 255.255.255.255
 ipv6 addr 4321::12 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
 exit
int ser1
 vrf for v1
 ipv4 addr 9.9.9.2 255.255.255.0
 ipv6 addr 9999::2 ffff::
 ipv4 access-group-in test4
 ipv6 access-group-in test6
 exit
int ser2
 vrf for v1
 ipv4 addr 9.9.8.2 255.255.255.0
 ipv6 addr 9998::2 ffff::
 ipv4 autoroute bgp4 1 2.2.2.1 9.9.8.1 recur
 ipv6 autoroute bgp6 1 4321::1 9998::1 recur
 exit
router bgp4 1
 vrf v1
 no safe-ebgp
 address uni
 local-as 2
 router-id 4.4.4.1
 neigh 9.9.9.1 remote-as 1
 red conn
 exit
router bgp6 1
 vrf v1
 no safe-ebgp
 address uni
 local-as 2
 router-id 6.6.6.1
 neigh 9999::1 remote-as 1
 red conn
 exit
!


r1 tping 100 60 2.2.2.12 /vrf v1
r1 tping 100 60 4321::12 /vrf v1

r2 tping 100 60 2.2.2.11 /vrf v1
r2 tping 100 60 4321::11 /vrf v1

r1 tping 100 60 2.2.2.2 /vrf v1
r1 tping 100 60 4321::2 /vrf v1

r2 tping 100 60 2.2.2.1 /vrf v1
r2 tping 100 60 4321::1 /vrf v1

r1 tping 0 60 9.9.9.2 /vrf v1
r1 tping 0 60 9999::2 /vrf v1

r2 tping 0 60 9.9.9.1 /vrf v1
r2 tping 0 60 9999::1 /vrf v1
