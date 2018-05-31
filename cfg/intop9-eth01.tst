description interop9: ethernet encapsulation

addrouter r1
int eth1 eth 0000.0000.1111 $rem1$
!
vrf def v1
 rd 1:1
 exit
int eth1
 vrf for v1
 ipv4 addr 1.1.1.1 255.255.255.0
 ipv6 addr 1234::1 ffff::
 exit
!

addremote r2
int eth1 eth 0000.0000.2222 $rem1$
!
set interfaces ge-0/0/0.0 family inet address 1.1.1.2/24
set interfaces ge-0/0/0.0 family inet6 address 1234::2/64
commit
!


r1 tping 100 10 1.1.1.2 /vrf v1
r1 tping 100 10 1234::2 /vrf v1
