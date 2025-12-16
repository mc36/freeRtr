set a [exec "show ipv6 bgp 1955 multicast allroute ::/0 | inc preference"]
puts "$a"
