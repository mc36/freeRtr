description wireguard over ipv6

addrouter r1
int ser1 ser - $1a$ $1b$
!
vrf def v1
 rd 1:1
 exit
int ser1
 vrf for v1
 ipv4 addr 1.1.1.1 255.255.255.0
 ipv6 addr 1234::1 ffff::
 exit
crypto ipsec ips
 key $v10$RUZ3MnJKRWRxRkdEZ0M4MHVtM2Z3TW1BYWZ3cVhubytQc2JNSFBaMHVtTT1NNnZEVjhRZGlXRFFwcFZLaktmOHhqb0t0eUdBZVJLL1VlNDhrd0tJNVNzPQ==
 exit
int tun1
 tunnel vrf v1
 tunnel prot ips
 tunnel mode wireguard
 tunnel source ser1
 tunnel destination 1234::2
 vrf for v1
 ipv4 addr 2.2.2.1 255.255.255.0
 ipv6 addr 4321::1 ffff::
 exit
!

addrouter r2
int ser1 ser - $1b$ $1a$
!
vrf def v1
 rd 1:1
 exit
int ser1
 vrf for v1
 ipv4 addr 1.1.1.2 255.255.255.0
 ipv6 addr 1234::2 ffff::
 exit
crypto ipsec ips
 key $v10$NkpoeXZLUHV0UTlETkx1cE9QbURuUUxSV3RVV2xVakk2UFRKL0laOWwxdz1iUU1tcENhR1Z5cTlmK3Y0OFhHbWZINURNTHl0a3F6aUlEK3JCSCtxUWljPQ==
 exit
int tun1
 tunnel vrf v1
 tunnel prot ips
 tunnel mode wireguard
 tunnel source ser1
 tunnel destination 1234::1
 vrf for v1
 ipv4 addr 2.2.2.2 255.255.255.0
 ipv6 addr 4321::2 ffff::
 exit
!


r1 tping 100 5 2.2.2.2 /vrf v1
r2 tping 100 5 2.2.2.1 /vrf v1
r1 tping 100 5 4321::2 /vrf v1
r2 tping 100 5 4321::1 /vrf v1
