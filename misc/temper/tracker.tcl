set a [exec "term tab html" "show tracker"]
puts "<html><head><title>network map</title><meta http-equiv=refresh content=1;url=tracker.tcl></head>"
puts "<body bgcolor=black text=white>"
puts "<table border=1>$a</table>"
puts "</body></html>"
