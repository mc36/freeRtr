description p4lang: sgt over hairpin

addrouter r1
int eth1 eth 0000.0000.1111 $1a$ $1b$
int eth2 eth 0000.0000.1111 $2b$ $2a$
!
vrf def v1
 rd 1:1
 exit
vrf def v2
 rd 1:2
 exit
vrf def v9
 rd 1:1
 exit
hair 1
 ether
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
int lo1
 vrf for v2
 ipv4 addr 2.2.2.100 255.255.255.255
 ipv6 addr 4321::100 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
 exit
int sdn1
 sgt ena
 vrf for v1
 ipv4 addr 1.1.1.1 255.255.255.0
 ipv6 addr 1234:1::1 ffff:ffff::
 ipv6 ena
 exit
int sdn2
 sgt ena
 vrf for v1
 ipv4 addr 1.1.2.1 255.255.255.0
 ipv6 addr 1234:2::1 ffff:ffff::
 ipv6 ena
 exit
int sdn3
 sgt ass 1234
 vrf for v1
 ipv4 addr 1.1.3.1 255.255.255.0
 ipv6 addr 1234:3::1 ffff:ffff::
 ipv6 ena
 exit
int sdn4
 sgt ass 4321
 vrf for v2
 ipv4 addr 1.1.4.1 255.255.255.0
 ipv6 addr 1234:4::1 ffff:ffff::
 ipv6 ena
 exit
int hair11
 sgt ena
 vrf for v1
 ipv4 addr 1.1.5.1 255.255.255.0
 ipv6 addr 1234:5::1 ffff:ffff::
 ipv6 ena
 exit
int hair12
 sgt ena
 vrf for v2
 ipv4 addr 1.1.5.2 255.255.255.0
 ipv6 addr 1234:5::2 ffff:ffff::
 ipv6 ena
 exit
server p4lang p4
 interconnect eth2
 export-vrf v1 1
 export-vrf v2 2
 export-port sdn1 1
 export-port sdn2 2
 export-port sdn3 3
 export-port sdn4 4
 export-port hair11 dynamic
 export-port hair12 dynamic
 vrf v9
 exit
ipv4 route v1 1.1.4.0 255.255.255.0 1.1.5.2
ipv6 route v1 1234:4:: ffff:ffff:: 1234:5::2
ipv4 route v1 2.2.2.100 255.255.255.255 1.1.5.2
ipv4 route v1 2.2.2.103 255.255.255.255 1.1.1.2
ipv4 route v1 2.2.2.104 255.255.255.255 1.1.2.2
ipv4 route v1 2.2.2.105 255.255.255.255 1.1.3.2
ipv4 route v1 2.2.2.106 255.255.255.255 1.1.5.2
ipv6 route v1 4321::100 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff 1234:5::2
ipv6 route v1 4321::103 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff 1234:1::2
ipv6 route v1 4321::104 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff 1234:2::2
ipv6 route v1 4321::105 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff 1234:3::2
ipv6 route v1 4321::106 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff 1234:5::2
ipv4 route v2 1.1.1.0 255.255.255.0 1.1.5.1
ipv4 route v2 1.1.2.0 255.255.255.0 1.1.5.1
ipv4 route v2 1.1.3.0 255.255.255.0 1.1.5.1
ipv6 route v2 1234:1:: ffff:ffff:: 1234:5::1
ipv6 route v2 1234:2:: ffff:ffff:: 1234:5::1
ipv6 route v2 1234:3:: ffff:ffff:: 1234:5::1
ipv4 route v2 2.2.2.101 255.255.255.255 1.1.5.1
ipv4 route v2 2.2.2.103 255.255.255.255 1.1.5.1
ipv4 route v2 2.2.2.104 255.255.255.255 1.1.5.1
ipv4 route v2 2.2.2.105 255.255.255.255 1.1.5.1
ipv4 route v2 2.2.2.106 255.255.255.255 1.1.4.2
ipv6 route v2 4321::101 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff 1234:5::1
ipv6 route v2 4321::103 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff 1234:5::1
ipv6 route v2 4321::104 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff 1234:5::1
ipv6 route v2 4321::105 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff 1234:5::1
ipv6 route v2 4321::106 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff 1234:4::2
!

addother r2 feature hairpin route sgt
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
policy-map p1
 seq 10 act drop
  match sgt 4321
 seq 20 act trans
 exit
bridge 1
 mac-learn
 block-unicast
 exit
int eth1
 sgt ena
 bridge-gr 1
 exit
int bvi1
 service-policy-in p1
 vrf for v1
 ipv4 addr 1.1.1.2 255.255.255.0
 ipv6 addr 1234:1::2 ffff:ffff::
 exit
ipv4 route v1 1.1.2.0 255.255.255.0 1.1.1.1
ipv4 route v1 1.1.3.0 255.255.255.0 1.1.1.1
ipv4 route v1 1.1.4.0 255.255.255.0 1.1.1.1
ipv4 route v1 1.1.5.0 255.255.255.0 1.1.1.1
ipv6 route v1 1234:2:: ffff:ffff:: 1234:1::1
ipv6 route v1 1234:3:: ffff:ffff:: 1234:1::1
ipv6 route v1 1234:4:: ffff:ffff:: 1234:1::1
ipv6 route v1 1234:5:: ffff:ffff:: 1234:1::1
ipv4 route v1 2.2.2.100 255.255.255.255 1.1.1.1
ipv4 route v1 2.2.2.101 255.255.255.255 1.1.1.1
ipv4 route v1 2.2.2.104 255.255.255.255 1.1.1.1
ipv4 route v1 2.2.2.105 255.255.255.255 1.1.1.1
ipv4 route v1 2.2.2.106 255.255.255.255 1.1.1.1
ipv6 route v1 4321::100 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff 1234:1::1
ipv6 route v1 4321::101 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff 1234:1::1
ipv6 route v1 4321::104 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff 1234:1::1
ipv6 route v1 4321::105 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff 1234:1::1
ipv6 route v1 4321::106 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff 1234:1::1
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
 sgt ena
 vrf for v1
 ipv4 addr 1.1.2.2 255.255.255.0
 ipv6 addr 1234:2::2 ffff:ffff::
 exit
ipv4 route v1 1.1.1.0 255.255.255.0 1.1.2.1
ipv4 route v1 1.1.3.0 255.255.255.0 1.1.2.1
ipv4 route v1 1.1.4.0 255.255.255.0 1.1.2.1
ipv4 route v1 1.1.5.0 255.255.255.0 1.1.2.1
ipv6 route v1 1234:1:: ffff:ffff:: 1234:2::1
ipv6 route v1 1234:3:: ffff:ffff:: 1234:2::1
ipv6 route v1 1234:4:: ffff:ffff:: 1234:2::1
ipv6 route v1 1234:5:: ffff:ffff:: 1234:2::1
ipv4 route v1 2.2.2.100 255.255.255.255 1.1.2.1
ipv4 route v1 2.2.2.101 255.255.255.255 1.1.2.1
ipv4 route v1 2.2.2.103 255.255.255.255 1.1.2.1
ipv4 route v1 2.2.2.105 255.255.255.255 1.1.2.1
ipv4 route v1 2.2.2.106 255.255.255.255 1.1.2.1
ipv6 route v1 4321::100 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff 1234:2::1
ipv6 route v1 4321::101 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff 1234:2::1
ipv6 route v1 4321::103 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff 1234:2::1
ipv6 route v1 4321::105 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff 1234:2::1
ipv6 route v1 4321::106 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff 1234:2::1
!

addrouter r5
int eth1 eth 0000.0000.5555 $5b$ $5a$
!
vrf def v1
 rd 1:1
 exit
int lo0
 vrf for v1
 ipv4 addr 2.2.2.105 255.255.255.255
 ipv6 addr 4321::105 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
 exit
int eth1
 vrf for v1
 ipv4 addr 1.1.3.2 255.255.255.0
 ipv6 addr 1234:3::2 ffff:ffff::
 exit
ipv4 route v1 1.1.1.0 255.255.255.0 1.1.3.1
ipv4 route v1 1.1.2.0 255.255.255.0 1.1.3.1
ipv4 route v1 1.1.4.0 255.255.255.0 1.1.3.1
ipv4 route v1 1.1.5.0 255.255.255.0 1.1.3.1
ipv6 route v1 1234:1:: ffff:ffff:: 1234:3::1
ipv6 route v1 1234:2:: ffff:ffff:: 1234:3::1
ipv6 route v1 1234:4:: ffff:ffff:: 1234:3::1
ipv6 route v1 1234:5:: ffff:ffff:: 1234:3::1
ipv4 route v1 2.2.2.100 255.255.255.255 1.1.3.1
ipv4 route v1 2.2.2.101 255.255.255.255 1.1.3.1
ipv4 route v1 2.2.2.103 255.255.255.255 1.1.3.1
ipv4 route v1 2.2.2.104 255.255.255.255 1.1.3.1
ipv4 route v1 2.2.2.106 255.255.255.255 1.1.3.1
ipv6 route v1 4321::100 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff 1234:3::1
ipv6 route v1 4321::101 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff 1234:3::1
ipv6 route v1 4321::103 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff 1234:3::1
ipv6 route v1 4321::104 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff 1234:3::1
ipv6 route v1 4321::106 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff 1234:3::1
!

addrouter r6
int eth1 eth 0000.0000.6666 $6b$ $6a$
!
vrf def v1
 rd 1:1
 exit
int lo0
 vrf for v1
 ipv4 addr 2.2.2.106 255.255.255.255
 ipv6 addr 4321::106 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
 exit
int eth1
 vrf for v1
 ipv4 addr 1.1.4.2 255.255.255.0
 ipv6 addr 1234:4::2 ffff:ffff::
 exit
ipv4 route v1 1.1.1.0 255.255.255.0 1.1.4.1
ipv4 route v1 1.1.2.0 255.255.255.0 1.1.4.1
ipv4 route v1 1.1.3.0 255.255.255.0 1.1.4.1
ipv4 route v1 1.1.5.0 255.255.255.0 1.1.4.1
ipv6 route v1 1234:1:: ffff:ffff:: 1234:4::1
ipv6 route v1 1234:2:: ffff:ffff:: 1234:4::1
ipv6 route v1 1234:3:: ffff:ffff:: 1234:4::1
ipv6 route v1 1234:5:: ffff:ffff:: 1234:4::1
ipv4 route v1 2.2.2.100 255.255.255.255 1.1.4.1
ipv4 route v1 2.2.2.101 255.255.255.255 1.1.4.1
ipv4 route v1 2.2.2.103 255.255.255.255 1.1.4.1
ipv4 route v1 2.2.2.104 255.255.255.255 1.1.4.1
ipv4 route v1 2.2.2.105 255.255.255.255 1.1.4.1
ipv6 route v1 4321::100 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff 1234:4::1
ipv6 route v1 4321::101 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff 1234:4::1
ipv6 route v1 4321::103 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff 1234:4::1
ipv6 route v1 4321::104 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff 1234:4::1
ipv6 route v1 4321::105 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff 1234:4::1
!


r1 tping 100 10 1.1.5.2 vrf v1
r1 tping 100 10 1234:5::2 vrf v1
r1 tping 100 10 1.1.5.1 vrf v2
r1 tping 100 10 1234:5::1 vrf v2

r1 tping 100 10 1.1.1.2 vrf v1
r1 tping 100 10 1234:1::2 vrf v1
r1 tping 100 10 1.1.2.2 vrf v1
r1 tping 100 10 1234:2::2 vrf v1
r1 tping 100 10 1.1.3.2 vrf v1
r1 tping 100 10 1234:3::2 vrf v1
r1 tping 100 10 1.1.4.2 vrf v1
r1 tping 100 10 1234:4::2 vrf v1

r3 tping 100 10 1.1.1.2 vrf v1
r3 tping 100 10 1234:1::2 vrf v1
r3 tping 100 10 1.1.2.2 vrf v1
r3 tping 100 10 1234:2::2 vrf v1
r3 tping 100 10 1.1.3.2 vrf v1
r3 tping 100 10 1234:3::2 vrf v1
r3 tping 0 10 1.1.4.2 vrf v1
r3 tping 0 10 1234:4::2 vrf v1

r4 tping 100 10 1.1.1.2 vrf v1
r4 tping 100 10 1234:1::2 vrf v1
r4 tping 100 10 1.1.2.2 vrf v1
r4 tping 100 10 1234:2::2 vrf v1
r4 tping 100 10 1.1.3.2 vrf v1
r4 tping 100 10 1234:3::2 vrf v1
r4 tping 100 10 1.1.4.2 vrf v1
r4 tping 100 10 1234:4::2 vrf v1

r5 tping 100 10 1.1.1.2 vrf v1
r5 tping 100 10 1234:1::2 vrf v1
r5 tping 100 10 1.1.2.2 vrf v1
r5 tping 100 10 1234:2::2 vrf v1
r5 tping 100 10 1.1.3.2 vrf v1
r5 tping 100 10 1234:3::2 vrf v1
r5 tping 100 10 1.1.4.2 vrf v1
r5 tping 100 10 1234:4::2 vrf v1

r6 tping 0 10 1.1.1.2 vrf v1
r6 tping 0 10 1234:1::2 vrf v1
r6 tping 100 10 1.1.2.2 vrf v1
r6 tping 100 10 1234:2::2 vrf v1
r6 tping 100 10 1.1.3.2 vrf v1
r6 tping 100 10 1234:3::2 vrf v1
r6 tping 100 10 1.1.4.2 vrf v1
r6 tping 100 10 1234:4::2 vrf v1

r1 tping 100 10 2.2.2.100 vrf v1 int lo0
r1 tping 100 10 4321::100 vrf v1 int lo0
r1 tping 100 10 2.2.2.101 vrf v1 int lo0
r1 tping 100 10 4321::101 vrf v1 int lo0
r1 tping 100 10 2.2.2.103 vrf v1 int lo0
r1 tping 100 10 4321::103 vrf v1 int lo0
r1 tping 100 10 2.2.2.104 vrf v1 int lo0
r1 tping 100 10 4321::104 vrf v1 int lo0
r1 tping 100 10 2.2.2.105 vrf v1 int lo0
r1 tping 100 10 4321::105 vrf v1 int lo0
r1 tping 100 10 2.2.2.106 vrf v1 int lo0
r1 tping 100 10 4321::106 vrf v1 int lo0

r1 tping 100 10 2.2.2.100 vrf v2 int lo1
r1 tping 100 10 4321::100 vrf v2 int lo1
r1 tping 100 10 2.2.2.101 vrf v2 int lo1
r1 tping 100 10 4321::101 vrf v2 int lo1
r1 tping 100 10 2.2.2.103 vrf v2 int lo1
r1 tping 100 10 4321::103 vrf v2 int lo1
r1 tping 100 10 2.2.2.104 vrf v2 int lo1
r1 tping 100 10 4321::104 vrf v2 int lo1
r1 tping 100 10 2.2.2.105 vrf v2 int lo1
r1 tping 100 10 4321::105 vrf v2 int lo1
r1 tping 100 10 2.2.2.106 vrf v2 int lo1
r1 tping 100 10 4321::106 vrf v2 int lo1

r3 tping 100 10 2.2.2.101 vrf v1 int lo0
r3 tping 100 10 4321::101 vrf v1 int lo0
r3 tping 100 10 2.2.2.103 vrf v1 int lo0
r3 tping 100 10 4321::103 vrf v1 int lo0
r3 tping 100 10 2.2.2.104 vrf v1 int lo0
r3 tping 100 10 4321::104 vrf v1 int lo0
r3 tping 100 10 2.2.2.105 vrf v1 int lo0
r3 tping 100 10 4321::105 vrf v1 int lo0
r3 tping 0 10 2.2.2.106 vrf v1 int lo0
r3 tping 0 10 4321::106 vrf v1 int lo0

r4 tping 100 10 2.2.2.101 vrf v1 int lo0
r4 tping 100 10 4321::101 vrf v1 int lo0
r4 tping 100 10 2.2.2.103 vrf v1 int lo0
r4 tping 100 10 4321::103 vrf v1 int lo0
r4 tping 100 10 2.2.2.104 vrf v1 int lo0
r4 tping 100 10 4321::104 vrf v1 int lo0
r4 tping 100 10 2.2.2.105 vrf v1 int lo0
r4 tping 100 10 4321::105 vrf v1 int lo0
r4 tping 100 10 2.2.2.106 vrf v1 int lo0
r4 tping 100 10 4321::106 vrf v1 int lo0

r5 tping 100 10 2.2.2.101 vrf v1 int lo0
r5 tping 100 10 4321::101 vrf v1 int lo0
r5 tping 100 10 2.2.2.103 vrf v1 int lo0
r5 tping 100 10 4321::103 vrf v1 int lo0
r5 tping 100 10 2.2.2.104 vrf v1 int lo0
r5 tping 100 10 4321::104 vrf v1 int lo0
r5 tping 100 10 2.2.2.105 vrf v1 int lo0
r5 tping 100 10 4321::105 vrf v1 int lo0
r5 tping 100 10 2.2.2.106 vrf v1 int lo0
r5 tping 100 10 4321::106 vrf v1 int lo0

r6 tping 100 10 2.2.2.101 vrf v1 int lo0
r6 tping 100 10 4321::101 vrf v1 int lo0
r6 tping 0 10 2.2.2.103 vrf v1 int lo0
r6 tping 0 10 4321::103 vrf v1 int lo0
r6 tping 100 10 2.2.2.104 vrf v1 int lo0
r6 tping 100 10 4321::104 vrf v1 int lo0
r6 tping 100 10 2.2.2.105 vrf v1 int lo0
r6 tping 100 10 4321::105 vrf v1 int lo0
r6 tping 100 10 2.2.2.106 vrf v1 int lo0
r6 tping 100 10 4321::106 vrf v1 int lo0

r1 dping sdn . r4 2.2.2.105 vrf v1 int lo0
r1 dping sdn . r4 4321::105 vrf v1 int lo0
