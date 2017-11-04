description isis peer template

addrouter r1
int eth1 eth 0000.0000.1111 $1a$ $1b$
int eth2 eth 0000.0000.1111 $2a$ $2b$
!
vrf def v1
 rd 1:1
 exit
router isis4 1
 vrf v1
 net 48.4444.0000.1111.00
 red conn
 exit
router isis6 1
 vrf v1
 net 48.6666.0000.1111.00
 red conn
 exit
int lo1
 vrf for v1
 ipv4 addr 2.2.2.1 255.255.255.255
 ipv6 addr 4321::1 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
 exit
int temp1
 vrf for v1
 ipv4 addr 9.9.9.9 255.255.255.0
 router isis4 1 ena
 exit
int temp2
 vrf for v1
 ipv6 addr 9999::9 ffff::
 router isis6 1 ena
 exit
int eth1
 vrf for v1
 ipv4 addr 1.1.1.1 255.255.255.0
 temp temp1
 exit
int eth2
 vrf for v1
 ipv6 addr 1234::1 ffff::
 temp temp2
 exit
!

addrouter r2
int eth1 eth 0000.0000.2222 $1b$ $1a$
int eth2 eth 0000.0000.2222 $2b$ $2a$
!
vrf def v1
 rd 1:1
 exit
router isis4 1
 vrf v1
 net 48.4444.0000.2222.00
 red conn
 exit
router isis6 1
 vrf v1
 net 48.6666.0000.2222.00
 red conn
 exit
int lo1
 vrf for v1
 ipv4 addr 2.2.2.2 255.255.255.255
 ipv6 addr 4321::2 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
 exit
int temp1
 vrf for v1
 ipv4 addr 9.9.9.9 255.255.255.0
 exit
int temp2
 vrf for v1
 ipv6 addr 9999::9 ffff::
 exit
int eth1
 vrf for v1
 ipv4 addr 1.1.1.2 255.255.255.0
 temp temp1
 exit
int eth2
 vrf for v1
 ipv6 addr 1234::2 ffff::
 temp temp2
 exit
int temp1
 router isis4 1 ena
 exit
int temp2
 router isis6 1 ena
 exit
!


r1 tping 100 20 2.2.2.2 /vrf v1
r2 tping 100 20 2.2.2.1 /vrf v1
r1 tping 100 20 4321::2 /vrf v1
r2 tping 100 20 4321::1 /vrf v1
