description babel in chain

addrouter r1
int eth1 eth 0000.0000.1111 $1a$ $1b$
!
vrf def v1
 rd 1:1
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
int eth1
 vrf for v1
 ipv4 addr 1.1.1.1 255.255.255.252
 ipv6 addr 1234:1::1 ffff:ffff::
 router babel4 1 ena
 router babel6 1 ena
 exit
!

addrouter r2
int eth1 eth 0000.0000.2222 $1b$ $1a$
int eth2 eth 0000.0000.2222 $2a$ $2b$
!
vrf def v1
 rd 1:1
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
int eth1
 vrf for v1
 ipv4 addr 1.1.1.2 255.255.255.252
 ipv6 addr 1234:1::2 ffff:ffff::
 router babel4 1 ena
 router babel6 1 ena
 exit
int eth2
 vrf for v1
 ipv4 addr 1.1.1.5 255.255.255.252
 ipv6 addr 1234:2::1 ffff:ffff::
 router babel4 1 ena
 router babel6 1 ena
 exit
!

addrouter r3
int eth1 eth 0000.0000.3333 $2b$ $2a$
int eth2 eth 0000.0000.3333 $3a$ $3b$
!
vrf def v1
 rd 1:1
 exit
router babel4 1
 vrf v1
 router 1111-2222-3333-0003
 red conn
 exit
router babel6 1
 vrf v1
 router 1111-2222-3333-0003
 red conn
 exit
int lo0
 vrf for v1
 ipv4 addr 2.2.2.3 255.255.255.255
 ipv6 addr 4321::3 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
 exit
int eth1
 vrf for v1
 ipv4 addr 1.1.1.6 255.255.255.252
 ipv6 addr 1234:2::2 ffff:ffff::
 router babel4 1 ena
 router babel6 1 ena
 exit
int eth2
 vrf for v1
 ipv4 addr 1.1.1.9 255.255.255.252
 ipv6 addr 1234:3::1 ffff:ffff::
 router babel4 1 ena
 router babel6 1 ena
 exit
!

addrouter r4
int eth1 eth 0000.0000.4444 $3b$ $3a$
!
vrf def v1
 rd 1:1
 exit
router babel4 1
 vrf v1
 router 1111-2222-3333-0004
 red conn
 exit
router babel6 1
 vrf v1
 router 1111-2222-3333-0004
 red conn
 exit
int lo0
 vrf for v1
 ipv4 addr 2.2.2.4 255.255.255.255
 ipv6 addr 4321::4 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
 exit
int eth1
 vrf for v1
 ipv4 addr 1.1.1.10 255.255.255.252
 ipv6 addr 1234:3::2 ffff:ffff::
 router babel4 1 ena
 router babel6 1 ena
 exit
!

r1 tping 100 130 2.2.2.2 /vrf v1
r1 tping 100 130 2.2.2.3 /vrf v1
r1 tping 100 130 2.2.2.4 /vrf v1
r1 tping 100 130 4321::2 /vrf v1
r1 tping 100 130 4321::3 /vrf v1
r1 tping 100 130 4321::4 /vrf v1

r2 tping 100 130 2.2.2.1 /vrf v1
r2 tping 100 130 2.2.2.3 /vrf v1
r2 tping 100 130 2.2.2.4 /vrf v1
r2 tping 100 130 4321::1 /vrf v1
r2 tping 100 130 4321::3 /vrf v1
r2 tping 100 130 4321::4 /vrf v1

r3 tping 100 130 2.2.2.1 /vrf v1
r3 tping 100 130 2.2.2.2 /vrf v1
r3 tping 100 130 2.2.2.4 /vrf v1
r3 tping 100 130 4321::1 /vrf v1
r3 tping 100 130 4321::2 /vrf v1
r3 tping 100 130 4321::4 /vrf v1

r4 tping 100 130 2.2.2.1 /vrf v1
r4 tping 100 130 2.2.2.2 /vrf v1
r4 tping 100 130 2.2.2.3 /vrf v1
r4 tping 100 130 4321::1 /vrf v1
r4 tping 100 130 4321::2 /vrf v1
r4 tping 100 130 4321::3 /vrf v1

r2 output show ipv4 babel 1 sum
r2 output show ipv6 babel 1 sum
r2 output show ipv4 babel 1 dat
r2 output show ipv6 babel 1 dat
r2 output show ipv4 route v1
r2 output show ipv6 route v1
output ../binTmp/rout-babel.html
<html><body bgcolor="#000000" text="#FFFFFF" link="#00FFFF" vlink="#00FFFF" alink="#00FFFF">
here are the ipv4 neighbors:
<pre>
<!>show:0
</pre>
here are the ipv6 neighbors:
<pre>
<!>show:1
</pre>
here is the ipv4 database:
<pre>
<!>show:2
</pre>
here is the ipv6 database:
<pre>
<!>show:3
</pre>
here are the ipv4 routes:
<pre>
<!>show:4
</pre>
here are the ipv6 routes:
<pre>
<!>show:5
</pre>
</body></html>
!
