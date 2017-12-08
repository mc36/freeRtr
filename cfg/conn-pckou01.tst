description ppp with packet over udp

addrouter r1
int eth1 eth 0000.0000.1111 $1a$ $1b$
!
vrf def v1
 rd 1:1
 exit
proxy-profile p1
 vrf v1
 exit
int eth1
 vrf for v1
 ipv4 addr 1.1.1.1 255.255.255.0
 ipv6 addr 1234::1 ffff::
 exit
int di1
 enc ppp
 ppp ip4cp close
 ppp ip6cp close
 vrf for v1
 ipv4 addr 2.2.2.1 255.255.255.0
 ipv6 addr 4321::1 ffff::
 exit
vpdn pou
 interface dialer1
 proxy p1
 target 1.1.1.2
 vcid 1234
 protocol pckoudp
 exit
!

addrouter r2
int eth1 eth 0000.0000.2222 $1b$ $1a$
!
vrf def v1
 rd 1:1
 exit
proxy-profile p1
 vrf v1
 exit
int eth1
 vrf for v1
 ipv4 addr 1.1.1.2 255.255.255.0
 ipv6 addr 1234::2 ffff::
 exit
int di1
 enc ppp
 ppp ip4cp close
 ppp ip6cp close
 vrf for v1
 ipv4 addr 2.2.2.2 255.255.255.0
 ipv6 addr 4321::2 ffff::
 exit
vpdn pou
 interface dialer1
 proxy p1
 target 1.1.1.1
 vcid 1234
 protocol pckoudp
 exit
!


r1 tping 100 10 2.2.2.2 /vrf v1
r1 tping 100 10 4321::2 /vrf v1
r2 tping 100 10 2.2.2.1 /vrf v1
r2 tping 100 10 4321::1 /vrf v1

r1 output show inter dia1 full
output ../binTmp/conn-pckou.html
<html><body bgcolor="#000000" text="#FFFFFF" link="#00FFFF" vlink="#00FFFF" alink="#00FFFF">
here is the interface:
<pre>
<!>show:0
</pre>
</body></html>
!
