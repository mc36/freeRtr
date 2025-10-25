description bridge tcp mss in

addrouter r1
int eth1 eth 0000.0000.1111 $1a$ $1b$
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
 exit
ipv4 pool p4 2.2.2.1 0.0.0.1 254
int di1
 enc ppp
 vrf for v1
 ipv4 addr 2.2.2.0 255.255.255.255
 ppp ip4cp local 2.2.2.0
 ipv4 pool p4
 ppp ip4cp open
 exit
server pckotcp pou
 clone di1
 vrf v1
 exit
client tcp-segments 1024 4096
!

addrouter r2
int eth1 eth 0000.0000.2222 $2a$ $2b$
!
vrf def v1
 rd 1:1
 exit
vrf def v2
 rd 1:1
 exit
vrf def v3
 rd 1:1
 exit
proxy-profile p1
 vrf v1
 exit
prefix-list p1
 permit 0.0.0.0/0
 exit
int eth1
 mtu 1500
 enforce-mtu both
 vrf for v1
 ipv4 addr 1.1.1.2 255.255.255.0
 ipv6 addr 1234::2 ffff::
 exit
int di2
 enc ppp
 vrf for v2
 ipv4 addr 4.4.4.4 255.255.255.128
 ppp ip4cp open
 ppp ip4cp local 0.0.0.0
 exit
vpdn di2
 interface di2
 proxy p1
 target 1.1.1.1
 vcid 2554
 prot pckotcp
 exit
int di3
 enc ppp
 vrf for v3
 ipv4 addr 4.4.4.4 255.255.255.128
 ppp ip4cp open
 ppp ip4cp local 0.0.0.0
 exit
vpdn di3
 interface di3
 proxy p1
 target 1234::1
 vcid 2554
 prot pckotcp
 exit
client tcp-segments 1024 4096
!

addrouter r3
int eth1 eth 0000.0000.3333 $1b$ $1a$
int eth2 eth 0000.0000.3333 $2b$ $2a$
!
bridge 1
 exit
int eth1
 bridge-gr 1
 bridge-tcp-mss ipv4in 1280
 bridge-tcp-mss ipv6in 1280
 exit
int eth2
 bridge-gr 1
 bridge-tcp-mss ipv4in 1280
 bridge-tcp-mss ipv6in 1280
 exit
!

r1 tping 100 30 1.1.1.2 vrf v1
r2 tping 100 30 1.1.1.1 vrf v1
r1 tping 100 30 1234::2 vrf v1
r2 tping 100 30 1234::1 vrf v1

r2 tping 100 30 2.2.2.0 vrf v2 siz 3000
r2 tping 100 30 2.2.2.0 vrf v3 siz 3000
