set a [exec "show ipv6 bgp 1955 multicast allroute 2001:738::/32 | inc preference"]
puts "$a"
