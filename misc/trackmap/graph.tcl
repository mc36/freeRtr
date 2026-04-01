set res [exec "term tab raw" "show ipv4 lsrp 1 graph fmt-json"]
puts "$res"
