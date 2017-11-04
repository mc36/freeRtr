description ike2 with blowfish

addrouter r1
int eth1 eth 0000.0000.1111 $1a$ $1b$
!
vrf def v1
 rd 1:1
 exit
int eth1
 vrf for v1
 ipv4 addr 1.1.1.1 255.255.255.0
 exit
crypto ipsec ips
 group 02
 cipher blowfish
 hash md5
 seconds 3600
 bytes 1024000
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
 exit
!

addrouter r2
int eth1 eth 0000.0000.2222 $1b$ $1a$
!
vrf def v1
 rd 1:1
 exit
crypto ipsec ips
 group 02
 cipher blowfish
 hash md5
 seconds 3600
 bytes 1024000
 key tester
 role resp
 isakmp 2
 protected ipv4
 exit
int eth1
 vrf for v1
 ipv4 addr 1.1.1.2 255.255.255.0
 exit
int tun1
 tunnel vrf v1
 tunnel prot ips
 tunnel mode ipsec
 tunnel source ethernet1
 tunnel destination 1.1.1.1
 vrf for v1
 ipv4 addr 2.2.2.2 255.255.255.0
 exit
!


r1 tping 100 10 2.2.2.2 /vrf v1
r2 tping 100 10 2.2.2.1 /vrf v1
