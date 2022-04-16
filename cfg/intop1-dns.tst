description interop1: dns

addrouter r1
int eth1 eth 0000.0000.1111 $per1$
!
vrf def v1
 rd 1:1
 exit
int eth1
 vrf for v1
 ipv4 addr 1.1.1.1 255.255.255.0
 ipv6 addr 1234::1 ffff::
 exit
server dns dns
 zone test.corp defttl 43200
 rr ip4.test.corp ip4a 2.2.2.2
 rr ip6.test.corp ip6a 1234::1
 vrf v1
 exit
int lo1
 vrf for v1
 ipv4 addr 4.4.4.4 255.255.255.255
 exit
server tel tel
 vrf v1
 security protocol tel
 exit
!

addpersist r2
int eth1 eth 0000.0000.2222 $per1$
int eth2 eth 0000.0000.2211 $per2$
!
ip routing
ipv6 unicast-routing
interface gigabit1
 ip address 1.1.1.2 255.255.255.0
 ipv6 address 1234::2/64
 no shutdown
 exit
interface gigabit2
 ip address 2.2.2.1 255.255.255.0
 ipv6 address 4321::1/64
 no shutdown
 exit
ip name-server 1.1.1.1
ip domain lookup
line vty 0 4
 transport input all
 transport output all
 no motd-banner
 no exec-banner
 no vacant-message
 no login
 exit
!

addrouter r3
int eth1 eth 0000.0000.1111 $per2$
!
vrf def v1
 rd 1:1
 exit
int eth1
 vrf for v1
 ipv4 addr 2.2.2.2 255.255.255.0
 ipv6 addr 4321::2 ffff::
 exit
int lo1
 vrf for v1
 ipv4 addr 3.3.3.3 255.255.255.255
 exit
server tel tel
 vrf v1
 security protocol tel
 exit
!


r1 tping 100 10 1.1.1.2 vrf v1
r1 tping 100 10 1234::2 vrf v1

r3 tping 100 10 2.2.2.1 vrf v1
r3 tping 100 10 4321::1 vrf v1

r1 send telnet 1.1.1.2 vrf v1 telnet
sleep 3000
r1 char 13
r1 read vxe#
r1 send telnet ip4.test.corp ipv4
sleep 3000
r1 tping 100 10 3.3.3.3 vrf v1

r3 send telnet 2.2.2.1 vrf v1 telnet
sleep 3000
r3 char 13
r3 read vxe#
r3 send telnet ip6.test.corp ipv6
sleep 3000
r3 tping 100 10 4.4.4.4 vrf v1
