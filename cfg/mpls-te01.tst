description te over ethernet

addrouter r1
int eth1 eth 0000.0000.1111 $1a$ $1b$
!
vrf def v1
 rd 1:1
 exit
access-list test4
 deny 1 any all any all
 permit all any all any all
 exit
access-list test6
 deny all 4321:: ffff:: all 4321:: ffff:: all
 permit all any all any all
 exit
int eth1
 vrf for v1
 ipv4 addr 1.1.1.1 255.255.255.0
 ipv6 addr 1234::1 ffff::
 ipv4 access-group-in test4
 ipv6 access-group-in test6
 mpls enable
 mpls rsvp4
 mpls rsvp6
 exit
int tun1
 tun sou eth1
 tun dest 1.1.1.2
 tun vrf v1
 tun mod p2pte
 vrf for v1
 ipv4 addr 2.2.2.1 255.255.255.252
 exit
int tun2
 tun sou eth1
 tun dest 1234::2
 tun vrf v1
 tun mod p2pte
 vrf for v1
 ipv6 addr 4321::1 ffff::
 exit
!

addrouter r2
int eth1 eth 0000.0000.2222 $1b$ $1a$
!
vrf def v1
 rd 1:1
 exit
access-list test4
 deny 1 any all any all
 permit all any all any all
 exit
access-list test6
 deny all 4321:: ffff:: all 4321:: ffff:: all
 permit all any all any all
 exit
int eth1
 vrf for v1
 ipv4 addr 1.1.1.2 255.255.255.0
 ipv6 addr 1234::2 ffff::
 ipv4 access-group-in test4
 ipv6 access-group-in test6
 mpls enable
 mpls rsvp4
 mpls rsvp6
 exit
int tun1
 tun sou eth1
 tun dest 1.1.1.1
 tun vrf v1
 tun mod p2pte
 vrf for v1
 ipv4 addr 2.2.2.2 255.255.255.252
 exit
int tun2
 tun sou eth1
 tun dest 1234::1
 tun vrf v1
 tun mod p2pte
 vrf for v1
 ipv6 addr 4321::2 ffff::
 exit
!


r1 tping 100 10 2.2.2.2 /vrf v1
r2 tping 100 10 2.2.2.1 /vrf v1
r1 tping 100 10 4321::2 /vrf v1
r2 tping 100 10 4321::1 /vrf v1
r1 tping 0 10 1.1.1.2 /vrf v1
r2 tping 0 10 1.1.1.1 /vrf v1

r1 output show mpls forw
r1 output show ipv4 rsvp v1 sum
r1 output show ipv6 rsvp v1 sum
output ../binTmp/mpls-te.html
<html><body bgcolor="#000000" text="#FFFFFF" link="#00FFFF" vlink="#00FFFF" alink="#00FFFF">
here is the lib:
<pre>
<!>show:0
</pre>
here is the ipv4 database:
<pre>
<!>show:1
</pre>
here is the ipv6 database:
<pre>
<!>show:2
</pre>
</body></html>
!
