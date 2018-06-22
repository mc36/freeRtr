description dejittering bundle

addrouter r1
int eth1 eth 0000.0000.1111 $1a$ $1b$
int eth2 eth 0000.0000.1111 $2a$ $2b$
!
vrf def v1
 rd 1:1
 exit
bundle 1
 repl
 sequ 8
 deji 300
 exit
int eth1
 bundle-gr 1
 exit
int eth2
 bundle-gr 1
 exit
int bun1
 vrf for v1
 ipv4 addr 1.1.1.1 255.255.255.0
 ipv6 addr 1234::1 ffff::
 exit
!

addrouter r2
int eth1 eth 0000.0000.2222 $1b$ $1a$
int eth2 eth 0000.0000.2222 $2b$ $2a$
!
vrf def v1
 rd 1:1
 exit
bundle 1
 repl
 sequ 8
 deji 300
 exit
int eth1
 bundle-gr 1
 exit
int eth2
 bundle-gr 1
 exit
int bun1
 vrf for v1
 ipv4 addr 1.1.1.2 255.255.255.0
 ipv6 addr 1234::2 ffff::
 exit
!


r1 tping 100 5 1.1.1.2 /vrf v1
r2 tping 100 5 1.1.1.1 /vrf v1
r1 tping 100 5 1234::2 /vrf v1
r2 tping 100 5 1234::1 /vrf v1

r1 tping 0 5 1.1.1.2 /vrf v1 /tim 100
r2 tping 0 5 1.1.1.1 /vrf v1 /tim 100
r1 tping 0 5 1234::2 /vrf v1 /tim 100
r2 tping 0 5 1234::1 /vrf v1 /tim 100

r1 tping 100 5 1.1.1.2 /vrf v1
r2 tping 100 5 1.1.1.1 /vrf v1
r1 tping 100 5 1234::2 /vrf v1
r2 tping 100 5 1234::1 /vrf v1
