description transparent proxy

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
ipv4 route v1 0.0.0.0 0.0.0.0 1.1.1.2
ipv6 route v1 :: :: 1234::2
proxy-profile p1
 vrf v1
 exit
!

addrouter r2
int ser1 ser - $1b$ $1a$
int ser2 ser - $2a$ $2b$
!
vrf def v1
 rd 1:1
 exit
proxy-profile p1
 vrf v1
 exit
int ser1
 transproxy p1
 exit
int ser2
 vrf for v1
 ipv4 addr 2.2.2.2 255.255.255.0
 ipv6 addr 4321::2 ffff::
 exit
!

addrouter r3
int ser1 ser - $2b$ $2a$
!
vrf def v1
 rd 1:1
 exit
int ser1
 vrf for v1
 ipv4 addr 2.2.2.3 255.255.255.0
 ipv6 addr 4321::3 ffff::
 exit
int lo0
 vrf for v1
 ipv4 addr 3.3.3.3 255.255.255.255
 ipv6 addr 3333::3 ffff::
 exit
server telnet telnet
 vrf v1
 exit
!


r2 tping 100 10 2.2.2.3 /vrf v1
r1 tping 0 5 3.3.3.3 /vrf v1

r1 send telnet 2.2.2.3 /prox p1
r1 tping 100 5 3.3.3.3 /vrf v1
r1 send exit
r1 read closed
r1 tping 0 60 3.3.3.3 /vrf v1

r1 send telnet 4321::3 /prox p1
r1 tping 100 5 3.3.3.3 /vrf v1
r1 send exit
r1 read closed
r1 tping 0 60 3.3.3.3 /vrf v1

r2 output show transprox ser1
output ../binTmp/crypt-proxy.html
<html><body bgcolor="#000000" text="#FFFFFF" link="#00FFFF" vlink="#00FFFF" alink="#00FFFF">
here is the session list:
<pre>
<!>show:0
</pre>
</body></html>
!
