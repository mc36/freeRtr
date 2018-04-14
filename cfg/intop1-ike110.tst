description interop1: ike1 with group2

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
crypto ipsec ips
 group 02
 cipher des
 hash md5
 seconds 3600
 bytes 67108864
 key tester
 role init
 isakmp 1
 protected ipv4
 exit
int tun1
 tunnel vrf v1
 tunnel prot ips
 tunnel mode ipsec
 tunnel source ethernet1
 tunnel destination 1.1.1.2
 vrf for v1
 ipv4 addr 2.2.2.1 255.255.255.0
 ipv6 addr 4321::1 ffff::
 exit
!

addother r2
int eth1 eth 0000.0000.2222 $1b$ $1a$
!
ip routing
ipv6 unicast-routing
interface gigabit1
 ip address 1.1.1.2 255.255.255.0
 ipv6 address 1234::2/64
 no shutdown
 exit
crypto isakmp policy 10
 encryption des
 hash md5
 authentication pre-share
 group 2
 lifetime 3600
 exit
crypto isakmp key tester address 1.1.1.1
crypto ipsec transform-set ts1 esp-des esp-md5-hmac
 mode tunnel
 exit
crypto ipsec profile pr1
 set security-association lifetime seconds 3600
 set security-association lifetime kilobytes 65536
 set transform-set ts1
 exit
interface tunnel1
 ip address 2.2.2.2 255.255.255.0
 tunnel source gigabit1
 tunnel destination 1.1.1.1
 tunnel mode ipsec ipv4
 tunnel protection ipsec profile pr1
 exit
!


r1 tping 100 10 1.1.1.2 /vrf v1
r1 tping 100 120 2.2.2.2 /vrf v1
