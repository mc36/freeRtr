set a [exec "show ipv4 bgp 1955 unicast allroute 193.6.0.0/16 | inc preference"]
puts "$a"
