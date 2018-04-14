description interop1: ike2 with group14

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
 group 14
 cipher des
 hash md5
 seconds 3600
 bytes 67108864
 key tester
 role init
 isakmp 2
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
crypto ikev2 proposal pr1
 encryption des
 integrity md5
 group 14
 exit
crypto ikev2 policy pl1
 proposal pr1
 exit
crypto ikev2 keyring kr1
 peer p1
  address 1.1.1.1
  pre-shared-key tester
 exit
crypto ikev2 profile pr1
 match identity remote address 1.1.1.1 255.255.255.255
 authentication local pre-share
 authentication remote pre-share
 lifetime 3600
 keyring local kr1
 exit
crypto ipsec transform-set ts1 esp-des esp-md5-hmac
 mode tunnel
 exit
crypto ipsec profile pr1
 set security-association lifetime seconds 3600
 set security-association lifetime kilobytes 65536
 set transform-set ts1
 set ikev2-profile pr1
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
