set a [exec "show ipv4 bgp 1955 unicast allroute 195.199.0.0/16 | inc preference"]
puts "$a"
