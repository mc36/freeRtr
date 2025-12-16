description bridge inspection

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
 ipv4 addr 1.1.1.1 255.255.255.0
 ipv6 addr 1234:1::1 ffff:ffff::
 exit
ipv4 route v1 2.2.2.2 255.255.255.255 1.1.1.2
ipv4 route v1 2.2.2.3 255.255.255.255 1.1.1.3
ipv6 route v1 4321::2 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff 1234:1::2
ipv6 route v1 4321::3 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff 1234:1::3
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
bridge 1
 inspect mac
 exit
int bvi1
 vrf for v1
 ipv4 addr 1.1.1.2 255.255.255.0
 ipv6 addr 1234:1::2 ffff:ffff::
 exit
int eth1
 bridge-gr 1
 exit
int eth2
 bridge-gr 1
 exit
ipv4 route v1 2.2.2.1 255.255.255.255 1.1.1.1
ipv4 route v1 2.2.2.3 255.255.255.255 1.1.1.3
ipv6 route v1 4321::1 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff 1234:1::1
ipv6 route v1 4321::3 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff 1234:1::3
!

addrouter r3
int eth1 eth 0000.0000.3333 $2b$ $2a$
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
 ipv4 addr 1.1.1.3 255.255.255.0
 ipv6 addr 1234:1::3 ffff:ffff::
 exit
ipv4 route v1 2.2.2.1 255.255.255.255 1.1.1.1
ipv4 route v1 2.2.2.2 255.255.255.255 1.1.1.2
ipv6 route v1 4321::1 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff 1234:1::1
ipv6 route v1 4321::2 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff 1234:1::2
!


r1 tping 100 5 2.2.2.2 vrf v1 sou lo0
r1 tping 100 5 4321::2 vrf v1 sou lo0
r1 tping 100 5 2.2.2.3 vrf v1 sou lo0
r1 tping 100 5 4321::3 vrf v1 sou lo0

r2 tping 100 5 2.2.2.1 vrf v1 sou lo0
r2 tping 100 5 4321::1 vrf v1 sou lo0
r2 tping 100 5 2.2.2.3 vrf v1 sou lo0
r2 tping 100 5 4321::3 vrf v1 sou lo0

r3 tping 100 5 2.2.2.1 vrf v1 sou lo0
r3 tping 100 5 4321::1 vrf v1 sou lo0
r3 tping 100 5 2.2.2.2 vrf v1 sou lo0
r3 tping 100 5 4321::2 vrf v1 sou lo0

r2 output show bridge 1
output ../binTmp/crypt-insp04.html
<html><body bgcolor="#000000" text="#FFFFFF" link="#00FFFF" vlink="#00FFFF" alink="#00FFFF">
here is the flows:
<pre>
<!>show:0
</pre>
</body></html>
!
