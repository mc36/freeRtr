description p4lang: replay window with openvpn

addrouter r1
int eth1 eth 0000.0000.1111 $1a$ $1b$
int eth2 eth 0000.0000.1111 $2b$ $2a$
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
int sdn1
 vrf for v2
 ipv4 addr 9.9.9.1 255.255.255.0
 exit
crypto ipsec ips
 cipher des
 hash md5
 key 22f9c676f655336c3f3188b8d9cc759903733212ed77231bc38126b6000b043f1f56d07b885f4d00676afd8fea25c88fa917294d8f1e89b84922d5d2556de977beac2f254ba2b67477131f4d4708cb509f4c9f784780465462e502d29183665bbd5eff6bdc27370f05aa1d856b497a1f7ef5f20bad7aff155619a4b09849fab814ee76e7121c2adf85326db4c1cce132200ca3e4d03930f765ba96a8c46f1ab374beb73e79093d60879a8d9585f2feb987d89e65a33ef3857f3b09df80a2403f6c50dc50439e258d61c7dac377514a8d281c10feeea79ae7b063064aec3989b4d867bb24182f7d007ad41284ee6577053dae2cc289dd39e66cd8fe7089b7015f
 replay 512
 exit
int tun1
 tun vrf v2
 tun source sdn1
 tun destination 9.9.9.2
 tun prot ips
 tun mode openvpn
 vrf for v3
 ipv4 addr 1.1.1.1 255.255.255.0
 ipv6 addr 1234:1::1 ffff:ffff::
 ipv6 ena
 exit
hair 1
 ether
 exit
int hair11
 vrf for v3
 ipv4 addr 1.1.9.2 255.255.255.0
 ipv6 addr 1234:9::2 ffff:ffff::
 ipv6 ena
 exit
int hair12
 vrf for v1
 ipv4 addr 1.1.9.1 255.255.255.0
 ipv6 addr 1234:9::1 ffff:ffff::
 ipv6 ena
 exit
int sdn2
 vrf for v1
 ipv4 addr 1.1.2.1 255.255.255.0
 ipv6 addr 1234:2::1 ffff:ffff::
 ipv6 ena
 exit
int sdn3
 vrf for v1
 ipv4 addr 1.1.3.1 255.255.255.0
 ipv6 addr 1234:3::1 ffff:ffff::
 ipv6 ena
 exit
int sdn4
 vrf for v1
 ipv4 addr 1.1.4.1 255.255.255.0
 ipv6 addr 1234:4::1 ffff:ffff::
 ipv6 ena
 exit
server p4lang p4
 interconnect eth2
 export-vrf v1 1
 export-vrf v2 2
 export-vrf v3 3
 export-port sdn1 1
 export-port sdn2 2
 export-port sdn3 3
 export-port sdn4 4
 export-port hair11 dynamic -1
 export-port hair12 dynamic -1
 export-port tun1 dynamic
 vrf v9
 exit
ipv4 route v3 2.2.2.103 255.255.255.255 1.1.1.2
ipv6 route v3 4321::103 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff 1234:1::2
ipv4 route v3 0.0.0.0 0.0.0.0 1.1.9.1
ipv6 route v3 :: :: 1234:9::1
ipv4 route v3 1.1.1.2 255.255.255.255 1.1.1.2
ipv6 route v3 1234:1::2 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff 1234:1::2
ipv4 route v1 1.1.1.0 255.255.255.0 1.1.9.2
ipv4 route v1 2.2.2.103 255.255.255.255 1.1.9.2
ipv4 route v1 2.2.2.104 255.255.255.255 1.1.2.2
ipv4 route v1 2.2.2.105 255.255.255.255 1.1.3.2
ipv4 route v1 2.2.2.106 255.255.255.255 1.1.4.2
ipv6 route v1 1234:1:: ffff:ffff:: 1234:9::2
ipv6 route v1 4321::103 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff 1234:9::2
ipv6 route v1 4321::104 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff 1234:2::2
ipv6 route v1 4321::105 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff 1234:3::2
ipv6 route v1 4321::106 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff 1234:4::2
!

addother r2 feature route hairpin openvpn
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
vrf def v2
 rd 1:1
 exit
int lo0
 vrf for v1
 ipv4 addr 2.2.2.103 255.255.255.255
 ipv6 addr 4321::103 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
 exit
bridge 1
 mac-learn
 block-unicast
 exit
int eth1
 bridge-gr 1
 exit
int bvi1
 vrf for v2
 ipv4 addr 9.9.9.2 255.255.255.0
 exit
crypto ipsec ips
 cipher des
 hash md5
 key 22f9c676f655336c3f3188b8d9cc759903733212ed77231bc38126b6000b043f1f56d07b885f4d00676afd8fea25c88fa917294d8f1e89b84922d5d2556de977beac2f254ba2b67477131f4d4708cb509f4c9f784780465462e502d29183665bbd5eff6bdc27370f05aa1d856b497a1f7ef5f20bad7aff155619a4b09849fab814ee76e7121c2adf85326db4c1cce132200ca3e4d03930f765ba96a8c46f1ab374beb73e79093d60879a8d9585f2feb987d89e65a33ef3857f3b09df80a2403f6c50dc50439e258d61c7dac377514a8d281c10feeea79ae7b063064aec3989b4d867bb24182f7d007ad41284ee6577053dae2cc289dd39e66cd8fe7089b7015f
 replay 512
 exit
int tun1
 tun vrf v2
 tun source bvi1
 tun destination 9.9.9.1
 tun prot ips
 tun mode openvpn
 vrf for v1
 ipv4 addr 1.1.1.2 255.255.255.0
 ipv6 addr 1234:1::2 ffff:ffff::
 exit
ipv4 route v1 1.1.2.0 255.255.255.0 1.1.1.1
ipv4 route v1 1.1.3.0 255.255.255.0 1.1.1.1
ipv4 route v1 1.1.4.0 255.255.255.0 1.1.1.1
ipv4 route v1 1.1.9.0 255.255.255.0 1.1.1.1
ipv6 route v1 1234:2:: ffff:ffff:: 1234:1::1
ipv6 route v1 1234:3:: ffff:ffff:: 1234:1::1
ipv6 route v1 1234:4:: ffff:ffff:: 1234:1::1
ipv6 route v1 1234:9:: ffff:ffff:: 1234:1::1
ipv4 route v1 2.2.2.101 255.255.255.255 1.1.1.1
ipv4 route v1 2.2.2.104 255.255.255.255 1.1.1.1
ipv4 route v1 2.2.2.105 255.255.255.255 1.1.1.1
ipv4 route v1 2.2.2.106 255.255.255.255 1.1.1.1
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
 vrf for v1
 ipv4 addr 1.1.2.2 255.255.255.0
 ipv6 addr 1234:2::2 ffff:ffff::
 exit
ipv4 route v1 1.1.1.0 255.255.255.0 1.1.2.1
ipv4 route v1 1.1.3.0 255.255.255.0 1.1.2.1
ipv4 route v1 1.1.4.0 255.255.255.0 1.1.2.1
ipv6 route v1 1234:1:: ffff:ffff:: 1234:2::1
ipv6 route v1 1234:3:: ffff:ffff:: 1234:2::1
ipv6 route v1 1234:4:: ffff:ffff:: 1234:2::1
ipv4 route v1 2.2.2.101 255.255.255.255 1.1.2.1
ipv4 route v1 2.2.2.103 255.255.255.255 1.1.2.1
ipv4 route v1 2.2.2.105 255.255.255.255 1.1.2.1
ipv4 route v1 2.2.2.106 255.255.255.255 1.1.2.1
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
ipv6 route v1 1234:1:: ffff:ffff:: 1234:3::1
ipv6 route v1 1234:2:: ffff:ffff:: 1234:3::1
ipv6 route v1 1234:4:: ffff:ffff:: 1234:3::1
ipv4 route v1 2.2.2.101 255.255.255.255 1.1.3.1
ipv4 route v1 2.2.2.103 255.255.255.255 1.1.3.1
ipv4 route v1 2.2.2.104 255.255.255.255 1.1.3.1
ipv4 route v1 2.2.2.106 255.255.255.255 1.1.3.1
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
ipv6 route v1 1234:1:: ffff:ffff:: 1234:4::1
ipv6 route v1 1234:2:: ffff:ffff:: 1234:4::1
ipv6 route v1 1234:3:: ffff:ffff:: 1234:4::1
ipv4 route v1 2.2.2.101 255.255.255.255 1.1.4.1
ipv4 route v1 2.2.2.103 255.255.255.255 1.1.4.1
ipv4 route v1 2.2.2.104 255.255.255.255 1.1.4.1
ipv4 route v1 2.2.2.105 255.255.255.255 1.1.4.1
ipv6 route v1 4321::101 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff 1234:4::1
ipv6 route v1 4321::103 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff 1234:4::1
ipv6 route v1 4321::104 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff 1234:4::1
ipv6 route v1 4321::105 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff 1234:4::1
!



r1 tping 100 10 9.9.9.2 /vrf v2
r3 tping 100 10 9.9.9.1 /vrf v2

r1 tping 100 10 1.1.1.2 /vrf v1
r1 tping 100 10 1234:1::2 /vrf v1
r1 tping 100 10 1.1.2.2 /vrf v1
r1 tping 100 10 1234:2::2 /vrf v1
r1 tping 100 10 1.1.3.2 /vrf v1
r1 tping 100 10 1234:3::2 /vrf v1
r1 tping 100 10 1.1.4.2 /vrf v1
r1 tping 100 10 1234:4::2 /vrf v1

r3 tping 100 10 1.1.1.2 /vrf v1
r3 tping 100 10 1234:1::2 /vrf v1
r3 tping 100 10 1.1.2.2 /vrf v1
r3 tping 100 10 1234:2::2 /vrf v1
r3 tping 100 10 1.1.3.2 /vrf v1
r3 tping 100 10 1234:3::2 /vrf v1
r3 tping 100 10 1.1.4.2 /vrf v1
r3 tping 100 10 1234:4::2 /vrf v1

r4 tping 100 10 1.1.1.2 /vrf v1
r4 tping 100 10 1234:1::2 /vrf v1
r4 tping 100 10 1.1.2.2 /vrf v1
r4 tping 100 10 1234:2::2 /vrf v1
r4 tping 100 10 1.1.3.2 /vrf v1
r4 tping 100 10 1234:3::2 /vrf v1
r4 tping 100 10 1.1.4.2 /vrf v1
r4 tping 100 10 1234:4::2 /vrf v1

r5 tping 100 10 1.1.1.2 /vrf v1
r5 tping 100 10 1234:1::2 /vrf v1
r5 tping 100 10 1.1.2.2 /vrf v1
r5 tping 100 10 1234:2::2 /vrf v1
r5 tping 100 10 1.1.3.2 /vrf v1
r5 tping 100 10 1234:3::2 /vrf v1
r5 tping 100 10 1.1.4.2 /vrf v1
r5 tping 100 10 1234:4::2 /vrf v1

r6 tping 100 10 1.1.1.2 /vrf v1
r6 tping 100 10 1234:1::2 /vrf v1
r6 tping 100 10 1.1.2.2 /vrf v1
r6 tping 100 10 1234:2::2 /vrf v1
r6 tping 100 10 1.1.3.2 /vrf v1
r6 tping 100 10 1234:3::2 /vrf v1
r6 tping 100 10 1.1.4.2 /vrf v1
r6 tping 100 10 1234:4::2 /vrf v1

r1 tping 100 10 2.2.2.101 /vrf v1 /int lo0
r1 tping 100 10 4321::101 /vrf v1 /int lo0
r1 tping 100 10 2.2.2.103 /vrf v1 /int lo0
r1 tping 100 10 4321::103 /vrf v1 /int lo0
r1 tping 100 10 2.2.2.104 /vrf v1 /int lo0
r1 tping 100 10 4321::104 /vrf v1 /int lo0
r1 tping 100 10 2.2.2.105 /vrf v1 /int lo0
r1 tping 100 10 4321::105 /vrf v1 /int lo0
r1 tping 100 10 2.2.2.106 /vrf v1 /int lo0
r1 tping 100 10 4321::106 /vrf v1 /int lo0

r3 tping 100 10 2.2.2.101 /vrf v1 /int lo0
r3 tping 100 10 4321::101 /vrf v1 /int lo0
r3 tping 100 10 2.2.2.103 /vrf v1 /int lo0
r3 tping 100 10 4321::103 /vrf v1 /int lo0
r3 tping 100 10 2.2.2.104 /vrf v1 /int lo0
r3 tping 100 10 4321::104 /vrf v1 /int lo0
r3 tping 100 10 2.2.2.105 /vrf v1 /int lo0
r3 tping 100 10 4321::105 /vrf v1 /int lo0
r3 tping 100 10 2.2.2.106 /vrf v1 /int lo0
r3 tping 100 10 4321::106 /vrf v1 /int lo0

r4 tping 100 10 2.2.2.101 /vrf v1 /int lo0
r4 tping 100 10 4321::101 /vrf v1 /int lo0
r4 tping 100 10 2.2.2.103 /vrf v1 /int lo0
r4 tping 100 10 4321::103 /vrf v1 /int lo0
r4 tping 100 10 2.2.2.104 /vrf v1 /int lo0
r4 tping 100 10 4321::104 /vrf v1 /int lo0
r4 tping 100 10 2.2.2.105 /vrf v1 /int lo0
r4 tping 100 10 4321::105 /vrf v1 /int lo0
r4 tping 100 10 2.2.2.106 /vrf v1 /int lo0
r4 tping 100 10 4321::106 /vrf v1 /int lo0

r5 tping 100 10 2.2.2.101 /vrf v1 /int lo0
r5 tping 100 10 4321::101 /vrf v1 /int lo0
r5 tping 100 10 2.2.2.103 /vrf v1 /int lo0
r5 tping 100 10 4321::103 /vrf v1 /int lo0
r5 tping 100 10 2.2.2.104 /vrf v1 /int lo0
r5 tping 100 10 4321::104 /vrf v1 /int lo0
r5 tping 100 10 2.2.2.105 /vrf v1 /int lo0
r5 tping 100 10 4321::105 /vrf v1 /int lo0
r5 tping 100 10 2.2.2.106 /vrf v1 /int lo0
r5 tping 100 10 4321::106 /vrf v1 /int lo0

r6 tping 100 10 2.2.2.101 /vrf v1 /int lo0
r6 tping 100 10 4321::101 /vrf v1 /int lo0
r6 tping 100 10 2.2.2.103 /vrf v1 /int lo0
r6 tping 100 10 4321::103 /vrf v1 /int lo0
r6 tping 100 10 2.2.2.104 /vrf v1 /int lo0
r6 tping 100 10 4321::104 /vrf v1 /int lo0
r6 tping 100 10 2.2.2.105 /vrf v1 /int lo0
r6 tping 100 10 4321::105 /vrf v1 /int lo0
r6 tping 100 10 2.2.2.106 /vrf v1 /int lo0
r6 tping 100 10 4321::106 /vrf v1 /int lo0

r1 tping 0 10 1.1.1.2 /vrf v3
r1 tping 0 10 1234:1::2 /vrf v3

r1 dping sdn . r3 2.2.2.105 /vrf v1 /int lo0
r1 dping sdn . r3 4321::105 /vrf v1 /int lo0
