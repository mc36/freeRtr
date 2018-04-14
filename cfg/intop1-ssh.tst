description interop1: ssh

exit

addrouter r1
int eth1 eth 0000.0000.1111 $1a$ $1b$
!
vrf def v1
 rd 1:1
 exit
int eth1
 vrf for v1
 ipv4 addr 1.1.1.1 255.255.255.0
 ipv6 addr 1234::1 ffff::
 exit
!

addother r2
int eth1 eth 0000.0000.2211 $1b$ $1a$
int eth2 eth 0000.0000.2222 $2a$ $2b$
!
ip routing
ipv6 unicast-routing
interface gigabit2
 ip address 1.1.1.2 255.255.255.0
 ipv6 address 1234::2/64
 no shutdown
 exit
interface gigabit1
 ip address 2.2.2.1 255.255.255.0
 ipv6 address 4321::1/64
 no shutdown
 exit
ip domain name lab
aaa new-model
aaa authentication login default local
username usr password pwd
crypto key generate rsa modulus 1024
line vty 0 4
 transport input all
 transport output all
 no motd-banner
 no exec-banner
 no vacant-message
 exit
!

addrouter r3
int eth1 eth 0000.0000.1111 $2b$ $2a$
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
 sec prot tel
 exit
!


r1 tping 100 10 1.1.1.2 /vrf v1
r1 tping 100 10 1234::2 /vrf v1

r3 tping 100 10 2.2.2.1 /vrf v1
r3 tping 100 10 4321::1 /vrf v1

r1 send telnet 1.1.1.2 /vrf v1 /ssh
r1 send usr
r1 send pwd
sleep 5
r1 char 13
r1 read r2>
r1 send telnet 2.2.2.2
sleep 5
r1 tping 100 10 3.3.3.3 /vrf v1
