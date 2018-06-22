description dummy test

addrouter r1
int eth1 eth 0000.0000.1111 $1a$ $1b$
!
vrf def v1
 rd 1:1
 exit
int lo0
 vrf for v1
 ipv4 addr 1.1.1.1 255.255.255.0
 exit
!


r1 send sh ver
r1 read place
r1 read #
r1 send sh plat
r1 tping 100 5 1.1.1.1 /vrf v1 /rep 1

r1 output show version
r1 output show platform

output ../binTmp/basic.html
<html><body bgcolor="#000000" text="#FFFFFF" link="#00FFFF" vlink="#00FFFF" alink="#00FFFF">
here is the version information:
<pre>
<!>show:0
</pre>
here is the platform information:
<pre>
<!>show:1
</pre>
</body></html>
!
