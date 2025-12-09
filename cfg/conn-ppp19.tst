description ppp policy with local authentication

addrouter r1
int ser1 ser - $1a$ $1b$
!
vrf def v1
 rd 1:1
 exit
int ser1
 enc ppp
 ppp user c
 ppp pass c
 vrf for v1
 ipv4 addr 1.1.1.1 255.255.255.0
 ipv6 addr 1234::1 ffff::
 exit
!

addrouter r2
int ser1 ser - $1b$ $1a$
!
vrf def v1
 rd 1:1
 exit
aaa userlist usr
 username c password c
 username c filter rate-limit-in 819 100
 exit
int ser1
 enc ppp
 ppp auth usr
 vrf for v1
 ipv4 addr 1.1.1.2 255.255.255.0
 ipv6 addr 1234::2 ffff::
 exit
!

r1 tping 100 30 1.1.1.2 vrf v1
r2 tping 100 30 1.1.1.1 vrf v1
r1 tping 100 30 1234::2 vrf v1
r2 tping 100 30 1234::1 vrf v1

r2 tping 85-95 5 1.1.1.1 vrf v1 rep 100 tim 500 siz 100
r2 tping 85-95 5 1234::1 vrf v1 rep 100 tim 500 siz 100
r1 tping 85-95 5 1.1.1.2 vrf v1 rep 100 tim 500 siz 100
r1 tping 85-95 5 1234::2 vrf v1 rep 100 tim 500 siz 100
