description ethernet over mpls

addrouter r1
int eth1 eth 0000.0000.1111 $1a$ $1b$
!
vrf def v1
 rd 1:1
 label-mode per-prefix
 exit
int lo0
 vrf for v1
 ipv4 addr 2.2.2.1 255.255.255.255
 ipv6 addr 4321::1 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
 exit
proxy-profile p1
 vrf v1
 source lo0
 exit
int eth1
 vrf for v1
 ipv4 addr 1.1.1.1 255.255.255.0
 ipv6 addr 1234::1 ffff::
 mpls enable
 mpls ldp4
 mpls ldp6
 exit
ipv4 route v1 2.2.2.2 255.255.255.255 1.1.1.2
ipv6 route v1 4321::2 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff 1234::2
bridge 1
 mac-learn
 exit
int bvi1
 vrf for v1
 ipv4 addr 3.3.3.1 255.255.255.0
 exit
vpdn eompls
 bridge-gr 1
 proxy p1
 target 2.2.2.2
 mtu 1500
 vcid 1234
 pwtype eth
 protocol pweompls
 exit
!

addrouter r2
int eth1 eth 0000.0000.2222 $1b$ $1a$
!
vrf def v1
 rd 1:1
 label-mode per-prefix
 exit
int lo0
 vrf for v1
 ipv4 addr 2.2.2.2 255.255.255.255
 ipv6 addr 4321::2 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
 exit
proxy-profile p1
 vrf v1
 source lo0
 exit
int eth1
 vrf for v1
 ipv4 addr 1.1.1.2 255.255.255.0
 ipv6 addr 1234::2 ffff::
 mpls enable
 mpls ldp4
 mpls ldp6
 exit
ipv4 route v1 2.2.2.1 255.255.255.255 1.1.1.1
ipv6 route v1 4321::1 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff 1234::1
bridge 1
 mac-learn
 exit
int bvi1
 vrf for v1
 ipv4 addr 3.3.3.2 255.255.255.0
 exit
vpdn eompls
 bridge-gr 1
 proxy p1
 target 2.2.2.1
 mtu 1500
 vcid 1234
 pwtype eth
 protocol pweompls
 exit
!


r1 tping 100 10 2.2.2.2 /vrf v1 /int lo0
r1 tping 100 10 4321::2 /vrf v1 /int lo0
r2 tping 100 10 2.2.2.1 /vrf v1 /int lo0
r2 tping 100 10 4321::1 /vrf v1 /int lo0
r1 tping 100 40 3.3.3.2 /vrf v1
r2 tping 100 40 3.3.3.1 /vrf v1

r1 output show mpls forw
r1 output show ipv4 ldp v1 sum
r1 output show ipv6 ldp v1 sum
r1 output show bridge 1
r1 output show inter bvi1 full
r1 output show ipv4 arp bvi1
r1 output show ipv6 neigh bvi1
output ../binTmp/mpls-pwe.html
<html><body bgcolor="#000000" text="#FFFFFF" link="#00FFFF" vlink="#00FFFF" alink="#00FFFF">
here is the lib:
<pre>
<!>show:0
</pre>
here is the ipv4 neighbor:
<pre>
<!>show:1
</pre>
here is the ipv6 neighbor:
<pre>
<!>show:2
</pre>
here is the ipv4 bridge:
<pre>
<!>show:3
</pre>
here is the interface:
<pre>
<!>show:4
</pre>
here is the arp:
<pre>
<!>show:5
</pre>
here are the neighbors:
<pre>
<!>show:6
</pre>
</body></html>
!
