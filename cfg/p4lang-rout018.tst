description p4lang: bundle vlan bridging

addrouter r1
int eth1 eth 0000.0000.1111 $1a$ $1b$
int eth2 eth 0000.0000.1111 $2b$ $2a$
!
vrf def v1
 rd 1:1
 exit
vrf def v9
 rd 1:1
 exit
int lo9
 vrf for v9
 ipv4 addr 10.10.10.227 255.255.255.255
 exit
int eth1
 vrf for v9
 ipv4 addr 10.11.12.254 255.255.255.0
 exit
int eth2
 exit
server dhcp4 eth1
 pool 10.11.12.1 10.11.12.99
 gateway 10.11.12.254
 netmask 255.255.255.0
 dns-server 10.10.10.227
 domain-name p4l
 static 0000.0000.2222 10.11.12.111
 interface eth1
 vrf v9
 exit
int lo0
 vrf for v1
 ipv4 addr 2.2.2.101 255.255.255.255
 ipv6 addr 4321::101 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
 exit
bundle 1
 exit
bridge 1
 mac-learn
 exit
int sdn1
 bridge-gr 1
 exit
int sdn2
 bridge-gr 1
 exit
int sdn3
 bundle-gr 1
 exit
int sdn4
 bundle-gr 1
 exit
int bun1.11
 bridge-gr 1
 exit
server p4lang p4
 interconnect eth2
 export-vrf v1 1
 export-br 1
 export-port sdn1 1
 export-port sdn2 2
 export-port sdn3 3
 export-port sdn4 4
 export-port bun1 dynamic
 vrf v9
 exit
!

addother r2 feature bundle vlan bridge
int eth1 eth 0000.0000.2222 $1b$ $1a$
int eth2 eth 0000.0000.2222 $2a$ $2b$
int eth3 eth 0000.0000.2222 $3a$ $3b$
int eth4 eth 0000.0000.2222 $4a$ $4b$
int eth5 eth 0000.0000.2222 $5a$ $5b$
int eth6 eth 0000.0000.2222 $6a$ $6b$
!
!

addrouter r3
int eth1 eth 0000.0000.3333 $3b$ $3a$
!
vrf def v1
 rd 1:1
 exit
int lo0
 vrf for v1
 ipv4 addr 2.2.2.103 255.255.255.255
 ipv6 addr 4321::103 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
 exit
int eth1
 vrf for v1
 ipv4 addr 1.1.2.3 255.255.255.0
 ipv6 addr 1234:2::3 ffff:ffff::
 exit
ipv4 route v1 2.2.2.104 255.255.255.255 1.1.2.4
ipv4 route v1 2.2.2.105 255.255.255.255 1.1.2.5
ipv6 route v1 4321::104 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff 1234:2::4
ipv6 route v1 4321::105 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff 1234:2::5
!

addrouter r4
int eth1 eth 0000.0000.4444 $4b$ $4a$
!
vrf def v1
 rd 1:1
 exit
int lo0
 vrf for v1
 ipv4 addr 2.2.2.104 255.255.255.255
 ipv6 addr 4321::104 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
 exit
int eth1
 vrf for v1
 ipv4 addr 1.1.2.4 255.255.255.0
 ipv6 addr 1234:2::4 ffff:ffff::
 exit
ipv4 route v1 2.2.2.103 255.255.255.255 1.1.2.3
ipv4 route v1 2.2.2.105 255.255.255.255 1.1.2.5
ipv6 route v1 4321::103 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff 1234:2::3
ipv6 route v1 4321::105 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff 1234:2::5
!

addrouter r5
int eth1 eth 0000.0000.5555 $5b$ $5a$
int eth2 eth 0000.0000.6666 $6b$ $6a$
!
vrf def v1
 rd 1:1
 exit
bundle 1
 exit
int lo0
 vrf for v1
 ipv4 addr 2.2.2.105 255.255.255.255
 ipv6 addr 4321::105 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
 exit
int eth1
 bundle-gr 1
 exit
int eth2
 bundle-gr 1
 exit
int bun1.11
 vrf for v1
 ipv4 addr 1.1.2.5 255.255.255.0
 ipv6 addr 1234:2::5 ffff:ffff::
 exit
ipv4 route v1 2.2.2.103 255.255.255.255 1.1.2.3
ipv4 route v1 2.2.2.104 255.255.255.255 1.1.2.4
ipv6 route v1 4321::103 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff 1234:2::3
ipv6 route v1 4321::104 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff 1234:2::4
!



r3 tping 100 10 1.1.2.4 /vrf v1
r3 tping 100 10 1234:2::4 /vrf v1
r3 tping 100 10 1.1.2.5 /vrf v1
r3 tping 100 10 1234:2::5 /vrf v1

r4 tping 100 10 1.1.2.3 /vrf v1
r4 tping 100 10 1234:2::3 /vrf v1
r4 tping 100 10 1.1.2.5 /vrf v1
r4 tping 100 10 1234:2::5 /vrf v1

r5 tping 100 10 1.1.2.3 /vrf v1
r5 tping 100 10 1234:2::3 /vrf v1
r5 tping 100 10 1.1.2.4 /vrf v1
r5 tping 100 10 1234:2::4 /vrf v1


r3 tping 100 10 2.2.2.103 /vrf v1 /int lo0
r3 tping 100 10 4321::103 /vrf v1 /int lo0
r3 tping 100 10 2.2.2.104 /vrf v1 /int lo0
r3 tping 100 10 4321::104 /vrf v1 /int lo0
r3 tping 100 10 2.2.2.105 /vrf v1 /int lo0
r3 tping 100 10 4321::105 /vrf v1 /int lo0

r4 tping 100 10 2.2.2.103 /vrf v1 /int lo0
r4 tping 100 10 4321::103 /vrf v1 /int lo0
r4 tping 100 10 2.2.2.104 /vrf v1 /int lo0
r4 tping 100 10 4321::104 /vrf v1 /int lo0
r4 tping 100 10 2.2.2.105 /vrf v1 /int lo0
r4 tping 100 10 4321::105 /vrf v1 /int lo0

r5 tping 100 10 2.2.2.103 /vrf v1 /int lo0
r5 tping 100 10 4321::103 /vrf v1 /int lo0
r5 tping 100 10 2.2.2.104 /vrf v1 /int lo0
r5 tping 100 10 4321::104 /vrf v1 /int lo0
r5 tping 100 10 2.2.2.105 /vrf v1 /int lo0
r5 tping 100 10 4321::105 /vrf v1 /int lo0

r1 dping sdn . r4 2.2.2.105 /vrf v1 /int lo0
r1 dping sdn . r4 4321::105 /vrf v1 /int lo0
