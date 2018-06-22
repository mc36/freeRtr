description uti over ipv4

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
bridge 1
 mac-learn
 exit
vpdn er
 bridge-group 1
 proxy p1
 target 1.1.1.2
 vcid 123
 protocol uti
 exit
int bvi1
 vrf for v1
 ipv4 addr 2.2.2.1 255.255.255.0
 ipv6 addr 4321::1 ffff::
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
bridge 1
 mac-learn
 exit
vpdn er
 bridge-group 1
 proxy p1
 target 1.1.1.1
 vcid 123
 protocol uti
 exit
int bvi1
 vrf for v1
 ipv4 addr 2.2.2.2 255.255.255.0
 ipv6 addr 4321::2 ffff::
 exit
!


r1 tping 100 5 2.2.2.2 /vrf v1
r2 tping 100 5 2.2.2.1 /vrf v1
r1 tping 100 5 4321::2 /vrf v1
r2 tping 100 5 4321::1 /vrf v1

r2 output show bridge 1
r2 output show inter bvi1 full
r2 output show ipv4 arp bvi1
r2 output show ipv6 neigh bvi1
output ../binTmp/conn-uti.html
<html><body bgcolor="#000000" text="#FFFFFF" link="#00FFFF" vlink="#00FFFF" alink="#00FFFF">
here is the bridge:
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
