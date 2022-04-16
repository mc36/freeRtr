description interop2: point2point ethernet encapsulation

addrouter r1
int eth1 eth 0000.0000.1111 $per1$
!
vrf def v1
 rd 1:1
 exit
int eth1
 vrf for v1
 ipv4 addr 1.1.1.2 255.255.255.254
 ipv6 addr 1234::2 ffff:ffff:ffff:ffff:ffff:ffff:ffff:fffe
 exit
!

addpersist r2
int eth1 eth 0000.0000.2222 $per1$
!
interface gigabit0/0/0/0
 ipv4 address 1.1.1.3 255.255.255.254
 ipv6 address 1234::3/127
 no shutdown
 exit
root
commit
!


r1 tping 100 10 1.1.1.3 vrf v1
r1 tping 100 10 1234::3 vrf v1
