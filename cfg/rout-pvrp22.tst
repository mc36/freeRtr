description pvrp tls encryption

addrouter r1
int eth1 eth 0000.0000.1111 $1a$ $1b$
!
crypto rsakey rsa generate 2048
crypto dsakey dsa generate 1024
crypto ecdsakey ecdsa generate 256
crypto certificate dsa generate dsa dsa
crypto certificate rsa generate rsa rsa
crypto certificate ecdsa generate ecdsa ecdsa
vrf def v1
 rd 1:1
 exit
router pvrp4 1
 vrf v1
 router 4.4.4.1
 red conn
 exit
router pvrp6 1
 vrf v1
 router 6.6.6.1
 red conn
 exit
int lo1
 vrf for v1
 ipv4 addr 2.2.2.1 255.255.255.255
 ipv6 addr 4321::1 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
 exit
int eth1
 vrf for v1
 ipv4 addr 1.1.1.1 255.255.255.252
 router pvrp4 1 ena
 router pvrp4 1 encryption tls rsa dsa ecdsa rsa dsa ecdsa
 ipv6 addr 1234:1::1 ffff:ffff::
 router pvrp6 1 ena
 router pvrp6 1 encryption tls rsa dsa ecdsa rsa dsa ecdsa
 exit
!

addrouter r2
int eth1 eth 0000.0000.2222 $1b$ $1a$
!
crypto rsakey rsa generate 2048
crypto dsakey dsa generate 1024
crypto ecdsakey ecdsa generate 256
crypto certificate dsa generate dsa dsa
crypto certificate rsa generate rsa rsa
crypto certificate ecdsa generate ecdsa ecdsa
vrf def v1
 rd 1:1
 exit
router pvrp4 1
 vrf v1
 router 4.4.4.2
 red conn
 exit
router pvrp6 1
 vrf v1
 router 6.6.6.2
 red conn
 exit
int lo1
 vrf for v1
 ipv4 addr 2.2.2.2 255.255.255.255
 ipv6 addr 4321::2 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
 exit
int eth1
 vrf for v1
 ipv4 addr 1.1.1.2 255.255.255.252
 router pvrp4 1 ena
 router pvrp4 1 encryption tls rsa dsa ecdsa rsa dsa ecdsa
 ipv6 addr 1234:1::2 ffff:ffff::
 router pvrp6 1 ena
 router pvrp6 1 encryption tls rsa dsa ecdsa rsa dsa ecdsa
 exit
!




r1 tping 100 40 2.2.2.2 vrf v1
r1 tping 100 40 4321::2 vrf v1
r2 tping 100 40 2.2.2.1 vrf v1
r2 tping 100 40 4321::1 vrf v1

r2 output show ipv4 pvrp 1 sum
r2 output show ipv6 pvrp 1 sum
r2 output show ipv4 pvrp 1 rou
r2 output show ipv6 pvrp 1 rou
r2 output show ipv4 route v1
r2 output show ipv6 route v1
