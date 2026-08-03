set res [exec "term tab raw" "show ipv4 lsrp 1 graph as-json"]
puts "$res"
