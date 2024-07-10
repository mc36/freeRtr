description openflow: mpls routing over backplane

addrouter r1
int eth1 eth 0000.0000.1111 $1a$ $1b$
int eth2 eth 0000.0000.1111 $13a$ $13b$
int eth3 eth 0000.0000.1111 $14a$ $14b$
!
vrf def v1
 rd 1:1
 label-mode per-prefix
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
server dhcp4 eth1
 pool 10.11.12.1 10.11.12.99
 gateway 10.11.12.254
 netmask 255.255.255.0
 dns-server 10.10.10.227
 domain-name ovs
 static 0000.0000.2222 10.11.12.111
 interface eth1
 vrf v9
 exit
int eth2
 vrf for v9
 ipv4 addr 10.12.13.254 255.255.255.0
 exit
server dhcp4 eth2
 pool 10.12.13.1 10.12.13.99
 gateway 10.12.13.254
 netmask 255.255.255.0
 dns-server 10.10.10.227
 domain-name ovs
 static 0000.0000.3333 10.12.13.111
 interface eth2
 vrf v9
 exit
int eth3
 vrf for v9
 ipv4 addr 10.13.14.254 255.255.255.0
 exit
server dhcp4 eth3
 pool 10.13.14.1 10.13.14.99
 gateway 10.13.14.254
 netmask 255.255.255.0
 dns-server 10.10.10.227
 domain-name ovs
 static 0000.0000.4444 10.13.14.111
 interface eth3
 vrf v9
 exit
int lo0
 vrf for v1
 ipv4 addr 2.2.2.101 255.255.255.255
 ipv6 addr 4321::101 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
 exit
int sdn11
 vrf for v1
 ipv4 addr 1.1.1.1 255.255.255.0
 ipv6 addr 1234:1::1 ffff:ffff::
 mpls enable
 mpls ldp4
 mpls ldp6
 exit
int sdn12
 vrf for v1
 ipv4 addr 1.1.2.1 255.255.255.0
 ipv6 addr 1234:2::1 ffff:ffff::
 mpls enable
 mpls ldp4
 mpls ldp6
 exit
int sdn13
 vrf for v1
 mpls enable
 exit
int sdn14
 vrf for v1
 mpls enable
 exit
int sdn21
 vrf for v1
 ipv4 addr 1.1.3.1 255.255.255.0
 ipv6 addr 1234:3::1 ffff:ffff::
 mpls enable
 mpls ldp4
 mpls ldp6
 exit
int sdn22
 vrf for v1
 ipv4 addr 1.1.4.1 255.255.255.0
 ipv6 addr 1234:4::1 ffff:ffff::
 mpls enable
 mpls ldp4
 mpls ldp6
 exit
int sdn23
 vrf for v1
 mpls enable
 exit
int sdn24
 vrf for v1
 mpls enable
 exit
int sdn31
 vrf for v1
 mpls enable
 exit
int sdn32
 vrf for v1
 mpls enable
 exit
int sdn33
 vrf for v1
 mpls enable
 exit
int sdn34
 vrf for v1
 mpls enable
 exit
server openflow a
 export-vrf v1
 export-port sdn11 1
 export-port sdn12 2
 export-port sdn13 3
 export-port sdn14 4
 exit
server openflow b
 export-vrf v1
 export-port sdn21 1
 export-port sdn22 2
 export-port sdn23 3
 export-port sdn24 4
 exit
server openflow c
 export-vrf v1
 export-port sdn31 1
 export-port sdn32 2
 export-port sdn33 3
 export-port sdn34 4
 exit
server stack s
 dataplanes 4
 forwarder 1 openflow a
 forwarder 1 backplane sdn13 1
 forwarder 1 backplane sdn14 1
 forwarder 1 remote 10.11.12.111
 forwarder 2 openflow b
 forwarder 2 backplane sdn23 1
 forwarder 2 backplane sdn24 1
 forwarder 2 remote 10.12.13.111
 forwarder 3 openflow c
 forwarder 3 backplane sdn31 1
 forwarder 3 backplane sdn32 1
 forwarder 3 backplane sdn33 1
 forwarder 3 backplane sdn34 1
 forwarder 3 remote 10.13.14.111
 port 6653
 vrf v9
 exit
ipv4 route v1 2.2.2.103 255.255.255.255 1.1.1.2
ipv4 route v1 2.2.2.104 255.255.255.255 1.1.2.2
ipv4 route v1 2.2.2.105 255.255.255.255 1.1.3.2
ipv4 route v1 2.2.2.106 255.255.255.255 1.1.4.2
ipv6 route v1 4321::103 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff 1234:1::2
ipv6 route v1 4321::104 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff 1234:2::2
ipv6 route v1 4321::105 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff 1234:3::2
ipv6 route v1 4321::106 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff 1234:4::2
!

addother r2
int eth1 eth 0000.0000.2222 $1b$ $1a$
int eth2 eth 0000.0000.2222 $2a$ $2b$
int eth3 eth 0000.0000.2222 $3a$ $3b$
int eth4 eth 0000.0000.2222 $7a$ $7b$
int eth5 eth 0000.0000.2222 $8a$ $8b$
int eth6 eth 0000.0000.2222 $6a$ $6b$
!
!

addother r3
int eth1 eth 0000.0000.3333 $13b$ $13a$
int eth2 eth 0000.0000.3333 $4a$ $4b$
int eth3 eth 0000.0000.3333 $5a$ $5b$
int eth4 eth 0000.0000.3333 $10a$ $10b$
int eth5 eth 0000.0000.3333 $11a$ $11b$
int eth6 eth 0000.0000.3333 $9a$ $9b$
!
!

addother r4
int eth1 eth 0000.0000.4444 $14b$ $14a$
int eth2 eth 0000.0000.4444 $7b$ $7a$
int eth3 eth 0000.0000.4444 $8b$ $8a$
int eth4 eth 0000.0000.4444 $10b$ $10a$
int eth5 eth 0000.0000.4444 $11b$ $11a$
int eth6 eth 0000.0000.4444 $12a$ $12b$
!
!

addrouter r5
int eth1 eth 0000.0000.5555 $2b$ $2a$
!
vrf def v1
 rd 1:1
 label-mode per-prefix
 exit
access-list test4
 deny 1 any all any all
 permit all any all any all
 exit
access-list test6
 deny all 4321:: ffff:: all 4321:: ffff:: all
 permit all any all any all
 exit
int lo0
 vrf for v1
 ipv4 addr 2.2.2.103 255.255.255.255
 ipv6 addr 4321::103 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
 exit
int eth1
 vrf for v1
 ipv4 addr 1.1.1.2 255.255.255.0
 ipv6 addr 1234:1::2 ffff:ffff::
 ipv4 access-group-in test4
 ipv6 access-group-in test6
 no ipv4 unreachables
 no ipv6 unreachables
 mpls enable
 mpls ldp4
 mpls ldp6
 exit
ipv4 route v1 1.1.2.0 255.255.255.0 1.1.1.1
ipv4 route v1 1.1.3.0 255.255.255.0 1.1.1.1
ipv4 route v1 1.1.4.0 255.255.255.0 1.1.1.1
ipv6 route v1 1234:2:: ffff:ffff:: 1234:1::1
ipv6 route v1 1234:3:: ffff:ffff:: 1234:1::1
ipv6 route v1 1234:4:: ffff:ffff:: 1234:1::1
ipv4 route v1 2.2.2.101 255.255.255.255 1.1.1.1
ipv4 route v1 2.2.2.104 255.255.255.255 1.1.1.1
ipv4 route v1 2.2.2.105 255.255.255.255 1.1.1.1
ipv4 route v1 2.2.2.106 255.255.255.255 1.1.1.1
ipv6 route v1 4321::101 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff 1234:1::1
ipv6 route v1 4321::104 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff 1234:1::1
ipv6 route v1 4321::105 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff 1234:1::1
ipv6 route v1 4321::106 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff 1234:1::1
!

addrouter r6
int eth1 eth 0000.0000.6666 $3b$ $3a$
!
vrf def v1
 rd 1:1
 label-mode per-prefix
 exit
access-list test4
 deny 1 any all any all
 permit all any all any all
 exit
access-list test6
 deny all 4321:: ffff:: all 4321:: ffff:: all
 permit all any all any all
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
 ipv4 access-group-in test4
 ipv6 access-group-in test6
 no ipv4 unreachables
 no ipv6 unreachables
 mpls enable
 mpls ldp4
 mpls ldp6
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

addrouter r7
int eth1 eth 0000.0000.7777 $4b$ $4a$
!
vrf def v1
 rd 1:1
 label-mode per-prefix
 exit
access-list test4
 deny 1 any all any all
 permit all any all any all
 exit
access-list test6
 deny all 4321:: ffff:: all 4321:: ffff:: all
 permit all any all any all
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
 ipv4 access-group-in test4
 ipv6 access-group-in test6
 no ipv4 unreachables
 no ipv6 unreachables
 mpls enable
 mpls ldp4
 mpls ldp6
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

addrouter r8
int eth1 eth 0000.0000.8888 $5b$ $5a$
!
vrf def v1
 rd 1:1
 label-mode per-prefix
 exit
access-list test4
 deny 1 any all any all
 permit all any all any all
 exit
access-list test6
 deny all 4321:: ffff:: all 4321:: ffff:: all
 permit all any all any all
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
 ipv4 access-group-in test4
 ipv6 access-group-in test6
 no ipv4 unreachables
 no ipv6 unreachables
 mpls enable
 mpls ldp4
 mpls ldp6
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


r1 tping 100 10 2.2.2.101 vrf v1 sou lo0
r1 tping 100 10 4321::101 vrf v1 sou lo0
r1 tping 100 10 2.2.2.103 vrf v1 sou lo0
r1 tping 100 10 4321::103 vrf v1 sou lo0
r1 tping 100 10 2.2.2.104 vrf v1 sou lo0
r1 tping 100 10 4321::104 vrf v1 sou lo0
r1 tping 100 10 2.2.2.105 vrf v1 sou lo0
r1 tping 100 10 4321::105 vrf v1 sou lo0
r1 tping 100 10 2.2.2.106 vrf v1 sou lo0
r1 tping 100 10 4321::106 vrf v1 sou lo0

r5 tping 100 10 2.2.2.101 vrf v1 sou lo0
r5 tping 100 10 4321::101 vrf v1 sou lo0
r5 tping 100 10 2.2.2.103 vrf v1 sou lo0
r5 tping 100 10 4321::103 vrf v1 sou lo0
r5 tping 100 10 2.2.2.104 vrf v1 sou lo0
r5 tping 100 10 4321::104 vrf v1 sou lo0
r5 tping 100 10 2.2.2.105 vrf v1 sou lo0
r5 tping 100 10 4321::105 vrf v1 sou lo0
r5 tping 100 10 2.2.2.106 vrf v1 sou lo0
r5 tping 100 10 4321::106 vrf v1 sou lo0

r6 tping 100 10 2.2.2.101 vrf v1 sou lo0
r6 tping 100 10 4321::101 vrf v1 sou lo0
r6 tping 100 10 2.2.2.103 vrf v1 sou lo0
r6 tping 100 10 4321::103 vrf v1 sou lo0
r6 tping 100 10 2.2.2.104 vrf v1 sou lo0
r6 tping 100 10 4321::104 vrf v1 sou lo0
r6 tping 100 10 2.2.2.105 vrf v1 sou lo0
r6 tping 100 10 4321::105 vrf v1 sou lo0
r6 tping 100 10 2.2.2.106 vrf v1 sou lo0
r6 tping 100 10 4321::106 vrf v1 sou lo0

r7 tping 100 10 2.2.2.101 vrf v1 sou lo0
r7 tping 100 10 4321::101 vrf v1 sou lo0
r7 tping 100 10 2.2.2.103 vrf v1 sou lo0
r7 tping 100 10 4321::103 vrf v1 sou lo0
r7 tping 100 10 2.2.2.104 vrf v1 sou lo0
r7 tping 100 10 4321::104 vrf v1 sou lo0
r7 tping 100 10 2.2.2.105 vrf v1 sou lo0
r7 tping 100 10 4321::105 vrf v1 sou lo0
r7 tping 100 10 2.2.2.106 vrf v1 sou lo0
r7 tping 100 10 4321::106 vrf v1 sou lo0

r8 tping 100 10 2.2.2.101 vrf v1 sou lo0
r8 tping 100 10 4321::101 vrf v1 sou lo0
r8 tping 100 10 2.2.2.103 vrf v1 sou lo0
r8 tping 100 10 4321::103 vrf v1 sou lo0
r8 tping 100 10 2.2.2.104 vrf v1 sou lo0
r8 tping 100 10 4321::104 vrf v1 sou lo0
r8 tping 100 10 2.2.2.105 vrf v1 sou lo0
r8 tping 100 10 4321::105 vrf v1 sou lo0
r8 tping 100 10 2.2.2.106 vrf v1 sou lo0
r8 tping 100 10 4321::106 vrf v1 sou lo0

r1 dping sdn . r8 2.2.2.103 vrf v1 sou lo0
r1 dping sdn . r8 4321::103 vrf v1 sou lo0

r1 send tclsh
r1 output exec "telnet 10.11.12.111 2323 vrf v9 sou lo9"
output ../binTmp/opnflw-rout08.html
<html><body bgcolor="#000000" text="#FFFFFF" link="#00FFFF" vlink="#00FFFF" alink="#00FFFF">
here are the flows:
<pre>
<!>show:0
</pre>
</body></html>
!
