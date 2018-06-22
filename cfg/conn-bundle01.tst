description bundle of ethernet port

addrouter r1
int eth1 eth 0000.0000.1111 $1a$ $1b$
!
vrf def v1
 rd 1:1
 exit
bundle 1
 exit
int eth1
 bundle-gr 1
 exit
int bun1
 vrf for v1
 ipv4 addr 1.1.1.1 255.255.255.0
 ipv6 addr 1234::1 ffff::
 exit
!

addrouter r2
int eth1 eth 0000.0000.2222 $1b$ $1a$
!
vrf def v1
 rd 1:1
 exit
int eth1
 vrf for v1
 ipv4 addr 1.1.1.2 255.255.255.0
 ipv6 addr 1234::2 ffff::
 exit
!


r1 tping 100 5 1.1.1.2 /vrf v1
r2 tping 100 5 1.1.1.1 /vrf v1
r1 tping 100 5 1234::2 /vrf v1
r2 tping 100 5 1234::1 /vrf v1

r1 output show bundle 1
r1 output show inter bun1 full
r1 output show ipv4 arp bun1
r1 output show ipv6 neigh bun1
output ../binTmp/conn-bundle.html
<html><body bgcolor="#000000" text="#FFFFFF" link="#00FFFF" vlink="#00FFFF" alink="#00FFFF">
here is the bundle:
<pre>
<!>show:0
</pre>
here is the interface:
<pre>
<!>show:1
</pre>
here is the arp:
<pre>
<!>show:2
</pre>
here are the neighbors:
<pre>
<!>show:3
</pre>
</body></html>
!
