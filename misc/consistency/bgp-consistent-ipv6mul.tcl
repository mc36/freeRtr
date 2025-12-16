set a [exec "show ipv6 bgp 1955 multicast compare 2001:738::c 2001:738::179:1 cluster | first 20"]
puts "$a"
