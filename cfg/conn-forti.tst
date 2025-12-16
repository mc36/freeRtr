description ppp over forti

addrouter r1
int eth1 eth 0000.0000.1111 $1a$ $1b$
!
vrf def v1
 rd 1:1
 exit
int eth1
 vrf for v1
 ipv4 addr 1.1.1.1 255.255.255.0
 ipv6 addr 1234::1 ffff::
 exit
aaa userlist usr
 username c password c
 exit
int di1
 enc ppp
 vrf for v1
 ipv4 addr 2.2.2.1 255.255.255.0
 ipv6 addr 2222::1 ffff:ffff:ffff:ffff::
 exit
server http h
 host * path ./
 host * forti dialer1
 host * authen usr
 vrf v1
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
 vrf for v1
 ipv4 addr 2.2.2.2 255.255.255.0
 ipv6 addr 2222::2 ffff:ffff:ffff:ffff::
 exit
vpdn forti
 int di1
 proxy p1
 tar http://1.1.1.1/
 user c
 pass c
 prot forti
 exit
!


r2 tping 100 60 2.2.2.1 vrf v1
r2 tping 100 60 2222::1 vrf v1

r2 output show inter dia1 full
output ../binTmp/conn-forti.html
<html><body bgcolor="#000000" text="#FFFFFF" link="#00FFFF" vlink="#00FFFF" alink="#00FFFF">
here is the interface:
<pre>
<!>show:0
</pre>
</body></html>
!
