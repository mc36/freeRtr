description isis narrow metric

addrouter r1
int eth1 eth 0000.0000.1111 $1a$ $1b$
!
vrf def v1
 rd 1:1
 exit
router isis4 1
 vrf v1
 net 48.4444.0000.1111.00
 no metric-wide
 is-type level2
 red conn
 exit
router isis6 1
 vrf v1
 net 48.6666.0000.1111.00
 no metric-wide
 is-type level2
 red conn
 exit
int lo1
 vrf for v1
 ipv4 addr 2.2.2.1 255.255.255.255
 ipv6 addr 4321::1 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
 exit
int eth1.11
 vrf for v1
 ipv4 addr 1.1.1.1 255.255.255.252
 router isis4 1 ena
 exit
int eth1.12
 vrf for v1
 ipv6 addr 1234:1::1 ffff:ffff::
 router isis6 1 ena
 exit
!

addrouter r2
int eth1 eth 0000.0000.2222 $1b$ $1a$
int eth2 eth 0000.0000.2222 $2a$ $2b$
!
vrf def v1
 rd 1:1
 exit
router isis4 1
 vrf v1
 net 48.4444.0000.2222.00
 no metric-wide
 is-type level2
 red conn
 exit
router isis6 1
 vrf v1
 net 48.6666.0000.2222.00
 no metric-wide
 is-type level2
 red conn
 exit
int lo1
 vrf for v1
 ipv4 addr 2.2.2.2 255.255.255.255
 ipv6 addr 4321::2 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
 exit
int eth1.11
 vrf for v1
 ipv4 addr 1.1.1.2 255.255.255.252
 router isis4 1 ena
 exit
int eth1.12
 vrf for v1
 ipv6 addr 1234:1::2 ffff:ffff::
 router isis6 1 ena
 exit
int eth2.11
 vrf for v1
 ipv4 addr 1.1.1.5 255.255.255.252
 router isis4 1 ena
 exit
int eth2.12
 vrf for v1
 ipv6 addr 1234:2::1 ffff:ffff::
 router isis6 1 ena
 exit
!

addrouter r3
int eth1 eth 0000.0000.3333 $2b$ $2a$
!
vrf def v1
 rd 1:1
 exit
router isis4 1
 vrf v1
 net 48.4444.0000.3333.00
 is-type level2
 red conn
 exit
router isis6 1
 vrf v1
 net 48.6666.0000.3333.00
 is-type level2
 red conn
 exit
int lo1
 vrf for v1
 ipv4 addr 2.2.2.3 255.255.255.255
 ipv6 addr 4321::3 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
 exit
int eth1.11
 vrf for v1
 ipv4 addr 1.1.1.6 255.255.255.252
 router isis4 1 ena
 exit
int eth1.12
 vrf for v1
 ipv6 addr 1234:2::2 ffff:ffff::
 router isis6 1 ena
 exit
!


r1 tping 100 20 2.2.2.2 /vrf v1
r2 tping 100 20 2.2.2.1 /vrf v1
r1 tping 100 20 4321::2 /vrf v1
r2 tping 100 20 4321::1 /vrf v1

r1 tping 0 20 2.2.2.3 /vrf v1 /int lo1
r1 tping 0 20 4321::3 /vrf v1 /int lo1
r2 tping 0 20 2.2.2.3 /vrf v1 /int lo1
r2 tping 100 20 4321::3 /vrf v1 /int lo1

r3 tping 100 20 2.2.2.3 /vrf v1
r3 send conf t
r3 send router isis4 1
r3 send no metric-wide
r3 send end
r3 send conf t
r3 send router isis6 1
r3 send no metric-wide
r3 send end
r3 tping 100 20 2.2.2.3 /vrf v1

r1 tping 100 20 2.2.2.3 /vrf v1 /int lo1
r1 tping 100 20 4321::3 /vrf v1 /int lo1
r2 tping 100 20 2.2.2.3 /vrf v1 /int lo1
r2 tping 100 20 4321::3 /vrf v1 /int lo1

r2 output show ipv4 isis 1 nei
r2 output show ipv6 isis 1 nei
r2 output show ipv4 isis 1 dat 2
r2 output show ipv6 isis 1 dat 2
r2 output show ipv4 isis 1 tre 2
r2 output show ipv6 isis 1 tre 2
r2 output show ipv4 route v1
r2 output show ipv6 route v1
output ../binTmp/rout-isis.html
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
here is the ipv4 tree:
<pre>
<!>show:4
</pre>
here is the ipv6 tree:
<pre>
<!>show:5
</pre>
here are the ipv4 routes:
<pre>
<!>show:6
</pre>
here are the ipv6 routes:
<pre>
<!>show:7
</pre>
</body></html>
!
