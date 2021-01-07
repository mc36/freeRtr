puts "<html><head><title>vrfs</title><meta http-equiv=refresh content=3;url=vrf.tcl></head>"
puts "<body bgcolor=black text=white>"
puts "<table border=1>"
puts [exec "terminal table html" "show vrf routing"]
puts "</table>"
puts "</body></html>"
