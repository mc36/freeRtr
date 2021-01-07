puts "<html><head><title>arp</title><meta http-equiv=refresh content=1;url=arp.tcl></head>"
puts "<body bgcolor=black text=white>"
puts "<table border=1>"
puts [exec "terminal table html" "show ipv4 arp eth91"]
puts "</table>"
puts "<table border=1>"
puts [exec "terminal table html" "show ipv6 neigh eth91"]
puts "</table>"
puts "</body></html>"
