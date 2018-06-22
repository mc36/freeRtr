description interop1: modem with alaw

addrouter r1
int eth1 eth 0000.0000.1111 $1a$ $1b$
!
vrf def v1
 rd 1:1
 exit
int lo0
 vrf for v1
 ipv4 addr 2.2.2.2 255.255.255.255
 exit
int eth1
 vrf for v1
 ipv4 addr 1.1.1.1 255.255.255.0
 ipv6 addr 1234::1 ffff::
 exit
server modem sm
 codec alaw
 vrf v1
 exit
!

addrouter r2
int eth1 eth 0000.1234.2222 $1b$ $1a$
int eth2 eth 0000.1234.2222 $2a$ $2b$
!
vrf def v1
 rd 1:1
 exit
int eth1
 vrf for v1
 ipv4 addr 1.1.1.2 255.255.255.0
 ipv6 addr 1234::2 ffff::
 exit
int eth2
 vrf for v1
 ipv4 addr 1.1.2.2 255.255.255.0
 ipv6 addr 4321::2 ffff::
 exit
dial-peer 1
 codec alaw
 match-calling .*
 match-called .*
 vrf v1
 myname 99
 port-local 5060
 target 1.1.1.1
 direction both
 exit
dial-peer 2
 codec alaw
 match-calling .*
 match-called .*
 vrf v1
 myname 99
 port-local 5060
 target 1.1.2.1
 direction both
 exit
!

addother r3
int eth1 eth 0000.0000.2211 $3a$ $3b$
int eth2 eth 0000.0000.2222 $2b$ $2a$
!
ip routing
ipv6 unicast-routing
interface gigabit2
 ip address 1.1.3.1 255.255.255.0
 no shutdown
 exit
interface gigabit1
 ip address 1.1.2.1 255.255.255.0
 no shutdown
 exit
voice service voip
 no ip address trusted authenticate

 allow-connections h323 to h323
 allow-connections h323 to sip
 allow-connections sip to h323
 allow-connections sip to sip
sip-ua
 connection-reuse
dial-peer voice 1 voip
 destination-pattern 2[0-9]
 media flow-through
 session protocol sipv2
 session target ipv4:1.1.2.2
 session transport udp
 codec g711alaw
 no vad
dial-peer voice 2 voip
 destination-pattern 3[0-9]
 media flow-through
 session protocol sipv2
 session target ipv4:1.1.3.2
 session transport udp
 codec g711alaw
 no vad
!

addrouter r4
int eth1 eth 0000.0000.4444 $3b$ $3a$
!
vrf def v1
 rd 1:1
 exit
int eth1
 vrf for v1
 ipv4 addr 1.1.3.2 255.255.255.0
 ipv6 addr 1234::2 ffff::
 exit
translation-rule 1
 match ^.*<sip:(?<n>.*)@(?<d>.*)>.*$
 match ^sip:(?<n>.*)@(?<d>.*)$
 match ^(?<n>.*)$
 text "
 variable n
 text "
 character 32
 text <sip:
 variable n
 text @1.1.3.1>
 exit
dial-peer 1
 codec alaw
 match-calling .*
 match-called .*
 port-local 5060
 translate-out-calling 1
 translate-out-called 1
 vrf v1
 myname 99
 target 1.1.3.1
 direction both
 exit
!


r2 tping 100 10 1.1.1.1 /vrf v1
r2 tping 100 10 1.1.2.2 /vrf v1
r4 tping 100 10 1.1.3.1 /vrf v1
r4 send pack modem 22 33
r4 tping 100 5 2.2.2.2 /vrf v1
