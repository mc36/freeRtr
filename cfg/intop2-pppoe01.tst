description interop2: pppoe with pap

exit

addrouter r1
int eth1 eth 0000.0000.1111 $per1$
!
vrf def v1
 rd 1:1
 exit
int di1
 enc ppp
 vrf for v1
 ipv4 addr 3.3.3.1 255.255.255.0
 ipv6 addr fe80::1234 ffff::
 ppp ip4cp local 0.0.0.0
 ppp ip4cp open
 ppp ip6cp open
 ppp user usr
 ppp pass pwd
 exit
int eth1
 vrf for v1
 ipv4 address 2.2.2.2 255.255.255.0
 exit
int eth1.123
 p2poe client di1
 exit
aaa userlist aaa
 username usr password pwd
 exit
server radius v1
 authentication aaa
 secret tester
 vrf v1
 exit
!

addpersist r2
int eth1 eth 0000.0000.2222 $per1$
!
aaa authorization subscriber default group radius
aaa authentication subscriber default group radius
radius-server host 2.2.2.2 auth-port 1812 acct-port 1813 key tester
pool vrf default ipv4 p1
 address-range 1.1.1.201 1.1.1.255
dynamic-template
 type ppp dt1
  ppp authentication pap
  ppp ipcp peer-address pool p1
  ipv4 unnumbered Loopback0
  ipv6 enable
pppoe bba-group bg1
 ac name inet
 service selection disable
class-map type control subscriber match-any cm1
 match protocol ppp
 end-class-map
policy-map type control subscriber pm1
 event session-start match-first
  class type control subscriber cm1 do-until-failure
   10 activate dynamic-template dt1
 event session-activate match-first
  class type control subscriber cm1 do-until-failure
   10 authenticate aaa list default
 end-policy-map
interface Loopback0
 ipv4 address 1.1.1.111 255.255.255.255
 ipv6 address 4321::2/128
 exit
interface gigabit0/0/0/0
 ipv4 address 2.2.2.1 255.255.255.0
 no shutdown
 exit
interface gigabit0/0/0/0.123
 encapsulation dot1q 123
 service-policy type control subscriber pm1
 pppoe enable bba-group bg1
 exit
root
commit
!


r1 tping 100 60 2.2.2.1 vrf v1
r1 tping 100 60 1.1.1.111 vrf v1
