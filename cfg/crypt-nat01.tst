description source list translation to interface

addrouter r1
int eth1 eth 0000.0000.1111 $1a$ $1b$
!
vrf def v1
 rd 1:1
 exit
int eth1
 vrf for v1
 ipv4 addr 1.1.1.1 255.255.255.252
 ipv6 addr 1234:1::1 ffff:ffff::
 exit
!

addrouter r2
int eth1 eth 0000.0000.2222 $1b$ $1a$
int eth2 eth 0000.0000.2222 $2a$ $2b$
!
vrf def v1
 rd 1:1
 exit
int eth1
 vrf for v1
 ipv4 addr 1.1.1.2 255.255.255.252
 ipv6 addr 1234:1::2 ffff:ffff::
 exit
int eth2
 vrf for v1
 ipv4 addr 1.1.1.5 255.255.255.252
 ipv6 addr 1234:2::1 ffff:ffff::
 exit
access-list test4
 permit all any all any all
 exit
access-list test6
 permit all 1234:2:: ffff:ffff:: all 1234:1:: ffff:ffff:: all
 exit
ipv4 nat v1 srclist test4 interface ethernet1
ipv6 nat v1 srclist test6 interface ethernet1
!

addrouter r3
int eth1 eth 0000.0000.3333 $2b$ $2a$
!
vrf def v1
 rd 1:1
 exit
int eth1
 vrf for v1
 ipv4 addr 1.1.1.6 255.255.255.252
 ipv6 addr 1234:2::2 ffff:ffff::
 exit
ipv4 route v1 0.0.0.0 0.0.0.0 1.1.1.5
ipv6 route v1 :: :: 1234:2::1
!


r3 tping 100 5 1.1.1.1 /vrf v1
r3 tping 100 5 1234:1::1 /vrf v1

r2 output show ipv4 nat v1 tran
r2 output show ipv6 nat v1 tran
output ../binTmp/crypt-nat.html
<html><body bgcolor="#000000" text="#FFFFFF" link="#00FFFF" vlink="#00FFFF" alink="#00FFFF">
here is the translation list:
<pre>
<!>show:0
</pre>
here is the ipv6 translation list:
<pre>
<!>show:1
</pre>
</body></html>
!
