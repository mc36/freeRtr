set a [exec "show ipv6 bgp 1955 unicast allroute 2001:738::/32 | inc preference"]
puts "$a"
