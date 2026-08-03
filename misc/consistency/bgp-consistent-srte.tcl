set a [exec "show ipv4 bgp 1955 srte compare 195.111.97.93 195.111.97.179 cluster | first 20"]
puts "$a"
