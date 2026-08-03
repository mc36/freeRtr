puts "<html><head><title>trackers</title><meta http-equiv=refresh content=3;url=tracker.tcl></head>"
puts "<body bgcolor=black text=white>"
puts "<table border=1>"
puts [exec "terminal table html" "show tracker"]
puts "</table>"
puts "</body></html>"
