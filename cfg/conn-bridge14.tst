description bridged mac rewrite

addrouter r1
int eth1 eth 0000.0000.1111 $1a$ $1b$
!
vrf def v1
 rd 1:1
 exit
bridge 1
 exit
int eth1
 bridge-gr 1
 exit
int bvi1
 vrf for v1
 ipv4 addr 1.1.1.1 255.255.255.0
 ipv6 addr 1234::1 ffff::
 exit
!

addrouter r2
int eth1 eth 0000.0000.2222 $1b$ $1a$
int eth2 eth 0000.0000.2222 $2a$ $2b$
!
vrf def v1
 rd 1:1
 exit
bridge 1
 mac-learn
 exit
int eth1
 bridge-gr 1
 bridge-macre 0000.1234.1234
 exit
int eth2
 bridge-gr 1
 bridge-macre 0000.1234.1234
 exit
int bvi1
 vrf for v1
 ipv4 addr 1.1.1.2 255.255.255.0
 ipv6 addr 1234::2 ffff::
 exit
!

addrouter r3
int eth1 eth 0000.0000.3333 $2b$ $2a$
int eth2 eth 0000.0000.3333 $3a$ $3b$
!
vrf def v1
 rd 1:1
 exit
bridge 1
 mac-learn
 exit
int eth1
 bridge-gr 1
 exit
int eth2
 bridge-gr 1
 exit
int bvi1
 vrf for v1
 ipv4 addr 1.1.1.3 255.255.255.0
 ipv6 addr 1234::3 ffff::
 exit
!

addrouter r4
int eth1 eth 0000.0000.4444 $3b$ $3a$
!
vrf def v1
 rd 1:1
 exit
int eth1
 vrf for v1
 ipv4 addr 1.1.1.4 255.255.255.0
 ipv6 addr 1234::4 ffff::
 exit
!

r1 tping 100 5 1.1.1.2 /vrf v1
r1 tping 100 5 1.1.1.3 /vrf v1
r1 tping 100 5 1.1.1.4 /vrf v1
r1 tping 100 5 1234::2 /vrf v1
r1 tping 100 5 1234::3 /vrf v1
r1 tping 100 5 1234::4 /vrf v1

r2 tping 100 5 1.1.1.1 /vrf v1
r2 tping 100 5 1.1.1.3 /vrf v1
r2 tping 100 5 1.1.1.4 /vrf v1
r2 tping 100 5 1234::1 /vrf v1
r2 tping 100 5 1234::3 /vrf v1
r2 tping 100 5 1234::4 /vrf v1

r3 tping 100 5 1.1.1.1 /vrf v1
r3 tping 100 5 1.1.1.2 /vrf v1
r3 tping 100 5 1.1.1.4 /vrf v1
r3 tping 100 5 1234::1 /vrf v1
r3 tping 100 5 1234::2 /vrf v1
r3 tping 100 5 1234::4 /vrf v1

r4 tping 100 5 1.1.1.1 /vrf v1
r4 tping 100 5 1.1.1.2 /vrf v1
r4 tping 100 5 1.1.1.3 /vrf v1
r4 tping 100 5 1234::1 /vrf v1
r4 tping 100 5 1234::2 /vrf v1
r4 tping 100 5 1234::3 /vrf v1

r2 output show bridge 1
r2 output show inter bvi1 full
r2 output show ipv4 arp bvi1
r2 output show ipv6 neigh bvi1
output ../binTmp/conn-bridge.html
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
