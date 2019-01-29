description babel autoroute

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
router babel4 1
 vrf v1
 router 1111-2222-3333-0001
 red conn
 exit
router babel6 1
 vrf v1
 router 1111-2222-3333-0001
 red conn
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
 router babel4 1 ena
 router babel6 1 ena
 ipv4 access-group-in test4
 ipv6 access-group-in test6
 exit
int ser2
 vrf for v1
 ipv4 addr 9.9.8.1 255.255.255.0
 ipv6 addr 9998::1 ffff::
 ipv4 autoroute babel4 1 2.2.2.2 9.9.8.2
 ipv6 autoroute babel6 1 4321::2 9998::2
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
router babel4 1
 vrf v1
 router 1111-2222-3333-0002
 red conn
 exit
router babel6 1
 vrf v1
 router 1111-2222-3333-0002
 red conn
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
 router babel4 1 ena
 router babel6 1 ena
 ipv4 access-group-in test4
 ipv6 access-group-in test6
 exit
int ser2
 vrf for v1
 ipv4 addr 9.9.8.2 255.255.255.0
 ipv6 addr 9998::2 ffff::
 ipv4 autoroute babel4 1 2.2.2.1 9.9.8.1
 ipv6 autoroute babel6 1 4321::1 9998::1
 exit
!


r1 tping 0 40 9.9.9.2 /vrf v1
r1 tping 0 40 9999::2 /vrf v1

r2 tping 0 40 9.9.9.1 /vrf v1
r2 tping 0 40 9999::1 /vrf v1

r1 tping 100 40 2.2.2.12 /vrf v1
r1 tping 100 40 4321::12 /vrf v1

r2 tping 100 40 2.2.2.11 /vrf v1
r2 tping 100 40 4321::11 /vrf v1

r1 tping 0 40 2.2.2.2 /vrf v1
r1 tping 0 40 4321::2 /vrf v1

r2 tping 0 40 2.2.2.1 /vrf v1
r2 tping 0 40 4321::1 /vrf v1
