description ppp over tls

addrouter r1
int eth1 eth 0000.0000.1111 $1a$ $1b$
!
vrf def v1
 rd 1:1
 exit
int lo0
 vrf for v1
 ipv4 addr 1.1.1.1 255.255.255.255
 exit
ipv4 pool p4 2.2.2.1 0.0.0.1 254
int di1
 enc ppp
 vrf for v1
 ipv4 addr 2.2.2.0 255.255.255.255
 ppp ip4cp local 2.2.2.0
 ipv4 pool p4
 ppp ip4cp open
 exit
int eth1
 vrf for v1
 ipv4 addr 3.3.3.1 255.255.255.252
 exit
aaa userlist usr
 username c password c
 username c privilege 14
 exit
crypto rsakey rsa import MIICWwIBAAKBgH3oGPAPxpD7SS4c2fQqO4uPsTslERKLU/LQpTUj1V6bs1E7v8ucVemLgwmFD+HD3tT5fIHV8qOX8lMIiFwUsTjObYYovLrD+I40cX8MC0SbiyXN1Tty9yx72+xBlpBoCVgjsc1CoYhJrE+bl4Mi5U8s+gLU6Z8il30591UrnOaTAgMBAAECgYBDJ2cdq5xiOdUXZkYNx/TIhFSoUiXf+TZGXWiwhjNI6czjt/WdWP4tub6jdjg5V2pjt8njykRHY0TZasSoMFUxSYMmcMTEbqcit7m68wMCSyX/5y2GZlY1mAb6a86O9fvYhqIQjPkHsjvuIPpqMF4A2mPWKvUFCZIK++f9NeddaQJBAPTUgl+hZKLW/93RFAjnpHmy/ItEPa2fv3XDHFizI+IhGzsxjEk2NwIMaZJFVcxcKx5FFe/TCPp05gqNPGyZnvUCQQCDpp9g0rSwooYQSVlR8Ue4JYVkEbVcyKw7ouWzBc92WmGj8IKaCdOpZhUmhEFetSH5gcPl9zgb2klzqgn/OupnAkAdpZ4j9mc5UM+rDKZgbax9EC+Erb7KR43ntLi6BPdTaq7hfB6Avw/qL5aZH8xD8uVFxRfi8qsjqQQyQugOPowBAkBwtsGRthS5fgxl+Uad34PV51nzOS4byDudu3QJ+BmNbQhXwd0vYOZ1DE9jcKw/F7mE01MTmrvRLOziOFzixmBVAkEA5jY+pivJSXjr3UNzHw5XibJu6RGb+zB8C5qfiXgkSb1RJrr8WmA1J6esCdk3HIFWPbkmk51U6gJJbMLC7VBUhw==
crypto dsakey dsa import MIH2AgEAAkARuCVR2ibdMg9RzBpmTAF/H6m9GjhO+7PN6WA0kXwKJq9BYqx/fOW81zli2iJ92Sob46qTwkI2xlWon2U9ViPjAhUA2rBTFJAuhMUuPw8Z7ID4vE68nIcCQAkY1+pjOUBr9kDhFIMLJoypT8NGaOMM1FgBo+KVDldXzJiNupSYd+C/U5YjUdCVXeTv6vx5KvsBslCiBQf3dfgCQA6y17JdXeMHIkEMyc3kFWActGgFQFD1rdis3DMTHAsODJPjVUVQmjTcg+yYzVvAe//Vb3uxRTh4ZUTzYq0SlwkCFE1kOS+kYKHGzwmn+rJceR/15I95
crypto ecdsakey ecdsa import MEQCAQEEEACtT1ZwRkfDhndc4zXN/1GgBwYFK4EEAByhJAMiAASucERsdb81h9FyhNEdSrMbnK55VVz3W22q6dz+ggcGQA==
crypto certificate dsa import dsa dsa MIIBiTCCAUigAwIBAgIEGYt11zAJBgcqhkjOOAQDMA4xDDAKBgNVBAMTA3J0cjAeFw0xMTExMjYyMzU1NTJaFw0xMjExMjUyMzU1NTJaMA4xDDAKBgNVBAMTA3J0cjCB7zCBpwYHKoZIzjgEATCBmwJAEbglUdom3TIPUcwaZkwBfx+pvRo4TvuzzelgNJF8CiavQWKsf3zlvNc5YtoifdkqG+Oqk8JCNsZVqJ9lPVYj4wIVANqwUxSQLoTFLj8PGeyA+LxOvJyHAkAJGNfqYzlAa/ZA4RSDCyaMqU/DRmjjDNRYAaPilQ5XV8yYjbqUmHfgv1OWI1HQlV3k7+r8eSr7AbJQogUH93X4A0MAAkAOsteyXV3jByJBDMnN5BVgHLRoBUBQ9a3YrNwzExwLDgyT41VFUJo03IPsmM1bwHv/1W97sUU4eGVE82KtEpcJMAkGByqGSM44BAMDMAAwLQIUAkcIL2/c3U1UY1eF+r8sdT7Glv8CFQClUXhdcULO8gjpyKvt5e/nlkJBzA==
crypto certificate rsa import rsa rsa MIIBjDCB+aADAgECAgQBejsQMAsGCSqGSIb3DQEBBTAOMQwwCgYDVQQDEwNydHIwHhcNMTExMTI2MjM1NTUyWhcNMTIxMTI1MjM1NTUyWjAOMQwwCgYDVQQDEwNydHIwgZ4wDQYJKoZIhvcNAQEBBQADgYwAMIGIAoGAfegY8A/GkPtJLhzZ9Co7i4+xOyUREotT8tClNSPVXpuzUTu/y5xV6YuDCYUP4cPe1Pl8gdXyo5fyUwiIXBSxOM5thii8usP4jjRxfwwLRJuLJc3VO3L3LHvb7EGWkGgJWCOxzUKhiEmsT5uXgyLlTyz6AtTpnyKXfTn3VSuc5pMCAwEAATALBgkqhkiG9w0BAQUDgYA84W/4XYB0ryB1wd1/XYWXzPlOTts8Ziwm4xWalk+2F/aiJbRKkAYxQ5yrK0Nmrla7UjlKS5GcafBgIFR2muxrIbpfKFD2XmQ0uOOItQzvDnv2vgfe6IyK5hWXwddf+PFreCOGcvwY9hc1HTC1P+K4Hhn1Oo9xzyjpgNYd89iddA==
crypto certificate ecdsa import ecdsa ecdsa MIHlMIGOoAMCAQICBHdcELYwCQYHKoZIzj0EATAOMQwwCgYDVQQDEwNydHIwHhcNMTQxMDIzMDc1NjQ4WhcNMjQxMDIwMDc1NjQ4WjAOMQwwCgYDVQQDEwNydHIwNjAQBgcqhkjOPQIBBgUrgQQAHAMiAASucERsdb81h9FyhNEdSrMbnK55VVz3W22q6dz+ggcGQDAJBgcqhkjOPQQBA0cAMEQCEQCj1kFVhYyMmgeEsIMMCTalAi8Wvrl1ZghtS9ybZuiheuKZCFHKHPDOWPd4C6dKxyvvBsLep0GvqeRn/Un7+8QB0w==
server tel tel
 vrf v1
 security rsakey rsa
 security dsakey dsa
 security ecdsakey ecdsa
 security rsacert rsa
 security dsacert dsa
 security ecdsacert ecdsa
 security protocol tls
 login authen usr
 exec int di1
 exit
!

addrouter r2
int eth1 eth 0000.0000.2222 $1b$ $1a$
!
vrf def v1
 rd 1:1
 exit
proxy-profile p1
 vrf v1
 exit
prefix-list p1
 permit 0.0.0.0/0
 exit
int di1
 enc ppp
 vrf for v1
 ipv4 addr 4.4.4.4 255.255.255.128
 ppp ip4cp open
 ppp ip4cp local 0.0.0.0
 ipv4 gateway-prefix p1
 exit
int eth1
 vrf for v1
 ipv4 addr 3.3.3.2 255.255.255.252
 exit
chat-script login
 recv 5000 .*ser
 send c
 binsend 13
 recv 5000 .*ass
 send c
 binsend 13
 send ppp
 binsend 13
 exit
vpdn tel
 interface di1
 proxy p1
 script login
 target 3.3.3.1
 vcid 23
 protocol tls
 exit
!


r2 tping 100 40 2.2.2.0 /vrf v1
r2 tping 100 5 1.1.1.1 /vrf v1

r2 output show inter dia1 full
output ../binTmp/conn-tls.html
<html><body bgcolor="#000000" text="#FFFFFF" link="#00FFFF" vlink="#00FFFF" alink="#00FFFF">
here is the interface:
<pre>
<!>show:0
</pre>
</body></html>
!
