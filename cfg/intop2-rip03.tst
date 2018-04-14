description interop2: rip authentication

exit

addrouter r1
int eth1 eth 0000.0000.1111 $rem1$
!
vrf def v1
 rd 1:1
 exit
router rip4 1
 vrf v1
 red conn
 exit
router rip6 1
 vrf v1
 red conn
 exit
int eth1
 vrf for v1
 ipv4 addr 1.1.1.1 255.255.255.0
 ipv6 addr fe80::1 ffff::
 router rip4 1 ena
 router rip4 1 pass tester
 router rip6 1 ena
 exit
int lo0
 vrf for v1
 ipv4 addr 2.2.2.1 255.255.255.255
 ipv6 addr 4321::1 ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
 exit
!

addremote r2
int eth1 eth 0000.0000.2222 $rem1$
!
interface gigabit0/0/0/0
 ipv4 address 1.1.1.2 255.255.255.0
 ipv6 address fe80::2 link-local
 no shutdown
 exit
interface loopback0
 ipv4 addr 2.2.2.2 255.255.255.255
 ipv6 addr 4321::2/128
 exit
route-policy a
 set rip-metric 5
 pass
end-policy
key chain kc1 key 0 key-string clear tester
router rip
 redistribute connected route-policy a
 interface gigabit0/0/0/0 authentication keychain kc1 mode text
root
commit
!


r1 tping 100 10 1.1.1.2 /vrf v1
r1 tping 100 120 2.2.2.2 /vrf v1 /int lo0
!r1 tping 100 120 4321::2 /vrf v1 /int lo0
