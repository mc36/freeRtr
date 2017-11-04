description ebgp in chain

addrouter r1
int eth1 eth 0000.0000.1111 $1a$ $1b$
!
vrf def v1
 rd 1:1
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
 exit
router bgp4 1
 vrf v1
 address uni
 local-as 1
 router-id 4.4.4.1
 neigh 1.1.1.2 remote-as 2
 red conn
 exit
router bgp6 1
 vrf v1
 address uni
 local-as 1
 router-id 6.6.6.1
 neigh 1234:1::2 remote-as 2
 red conn
 exit
!

addrouter r2
int eth1 eth 0000.0000.2222 $1b$ $1a$
int eth2 eth 0000.0000.2222 $2a$ $2b$
!
vrf def v1
 rd 1:1
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
 exit
int eth2
 vrf for v1
 ipv4 addr 1.1.1.5 255.255.255.252
 ipv6 addr 1234:2::1 ffff:ffff::
 exit
router bgp4 1
 vrf v1
 address uni
 local-as 2
 router-id 4.4.4.2
 neigh 1.1.1.1 remote-as 1
 neigh 1.1.1.6 remote-as 3
 red conn
 exit
router bgp6 1
 vrf v1
 address uni
 local-as 2
 router-id 6.6.6.2
 neigh 1234:1::1 remote-as 1
 neigh 1234:2::2 remote-as 3
 red conn
 exit
!

addrouter r3
int eth1 eth 0000.0000.3333 $2b$ $2a$
int eth2 eth 0000.0000.3333 $3a$ $3b$
!
vrf def v1
 rd 1:1
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
 exit
int eth2
 vrf for v1
 ipv4 addr 1.1.1.9 255.255.255.252
 ipv6 addr 1234:3::1 ffff:ffff::
 exit
router bgp4 1
 vrf v1
 address uni
 local-as 3
 router-id 4.4.4.3
 neigh 1.1.1.5 remote-as 2
 neigh 1.1.1.10 remote-as 4
 red conn
 exit
router bgp6 1
 vrf v1
 address uni
 local-as 3
 router-id 6.6.6.3
 neigh 1234:2::1 remote-as 2
 neigh 1234:3::2 remote-as 4
 red conn
 exit
!

addrouter r4
int eth1 eth 0000.0000.4444 $3b$ $3a$
!
vrf def v1
 rd 1:1
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
 exit
router bgp4 1
 vrf v1
 address uni
 local-as 4
 router-id 4.4.4.4
 neigh 1.1.1.9 remote-as 3
 red conn
 exit
router bgp6 1
 vrf v1
 address uni
 local-as 4
 router-id 6.6.6.4
 neigh 1234:3::1 remote-as 3
 red conn
 exit
!

r1 tping 100 60 2.2.2.2 /vrf v1
r1 tping 100 60 4321::2 /vrf v1
r1 tping 100 60 2.2.2.3 /vrf v1
r1 tping 100 60 4321::3 /vrf v1
r1 tping 100 60 2.2.2.4 /vrf v1
r1 tping 100 60 4321::4 /vrf v1

r2 tping 100 60 2.2.2.1 /vrf v1
r2 tping 100 60 4321::1 /vrf v1
r2 tping 100 60 2.2.2.3 /vrf v1
r2 tping 100 60 4321::3 /vrf v1
r2 tping 100 60 2.2.2.4 /vrf v1
r2 tping 100 60 4321::4 /vrf v1

r3 tping 100 60 2.2.2.1 /vrf v1
r3 tping 100 60 4321::1 /vrf v1
r3 tping 100 60 2.2.2.2 /vrf v1
r3 tping 100 60 4321::2 /vrf v1
r3 tping 100 60 2.2.2.4 /vrf v1
r3 tping 100 60 4321::4 /vrf v1

r4 tping 100 60 2.2.2.1 /vrf v1
r4 tping 100 60 4321::1 /vrf v1
r4 tping 100 60 2.2.2.2 /vrf v1
r4 tping 100 60 4321::2 /vrf v1
r4 tping 100 60 2.2.2.3 /vrf v1
r4 tping 100 60 4321::3 /vrf v1

r2 output show ipv4 bgp 1 sum
r2 output show ipv6 bgp 1 sum
r2 output show ipv4 bgp 1 uni dat
r2 output show ipv6 bgp 1 uni dat
r2 output show ipv4 route v1
r2 output show ipv6 route v1
output ../binTmp/rout-bgp.html
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
