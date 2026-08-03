description p2p tp tunnel with symmetric labels

addrouter r1
int eth1 eth 0000.0000.1111 $1a$ $1b$
!
vrf def v1
 rd 1:1
 label-mode per-prefix
 exit
int eth1
 vrf for v1
 ipv4 addr 1.1.1.1 255.255.255.0
 ipv6 addr 1234::1 ffff:ffff::
 mpls enable
 exit
int tun1
 tun sou eth1
 tun dest 1.1.1.2
 tun vrf v1
 tun key 101
 tun mod p2ptp
 vrf for v1
 ipv4 addr 3.3.3.1 255.255.255.0
 exit
int tun2
 tun sou eth1
 tun dest 1234::2
 tun vrf v1
 tun key 102
 tun mod p2ptp
 vrf for v1
 ipv6 addr 3333::1 ffff:ffff::
 exit
!

addrouter r2
int eth1 eth 0000.0000.2222 $1b$ $1a$
!
vrf def v1
 rd 1:1
 label-mode per-prefix
 exit
int eth1
 vrf for v1
 ipv4 addr 1.1.1.2 255.255.255.0
 ipv6 addr 1234::2 ffff:ffff::
 mpls enable
 exit
int tun1
 tun sou eth1
 tun dest 1.1.1.1
 tun vrf v1
 tun key 101
 tun mod p2ptp
 vrf for v1
 ipv4 addr 3.3.3.3 255.255.255.0
 exit
int tun2
 tun sou eth1
 tun dest 1234::1
 tun vrf v1
 tun key 102
 tun mod p2ptp
 vrf for v1
 ipv6 addr 3333::3 ffff:ffff::
 exit
!


r1 tping 100 10 1.1.1.2 vrf v1 sou eth1
r2 tping 100 10 1.1.1.1 vrf v1 sou eth1
r1 tping 100 10 1234::2 vrf v1 sou eth1
r2 tping 100 10 1234::1 vrf v1 sou eth1

r1 tping 100 10 3.3.3.3 vrf v1 sou tun1
r1 tping 100 10 3333::3 vrf v1 sou tun2
r2 tping 100 10 3.3.3.1 vrf v1 sou tun1
r2 tping 100 10 3333::1 vrf v1 sou tun2
