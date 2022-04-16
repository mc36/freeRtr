description interop9: fragmentation and reassembly

addrouter r1
int eth1 eth 0000.0000.1111 $per1$
!
vrf def v1
 rd 1:1
 exit
int eth1
 mtu 1500
 enforce-mtu both
 vrf for v1
 ipv4 addr 1.1.1.1 255.255.255.0
 ipv6 addr 1234::1 ffff::
 ipv4 reassembly 16
 ipv4 fragmentation 1400
 ipv6 reassembly 16
 ipv6 fragmentation 1400
 exit
!

addpersist r2
int eth1 eth 0000.0000.2222 $per1$
!
set interfaces ge-0/0/0.0 family inet address 1.1.1.2/24
set interfaces ge-0/0/0.0 family inet6 address 1234::2/64
commit
!


r1 tping 100 15 1.1.1.2 vrf v1 siz 222
r1 tping 100 15 1234::2 vrf v1 siz 222

r1 tping 100 15 1.1.1.2 vrf v1 siz 2222
r1 tping 100 15 1234::2 vrf v1 siz 2222
