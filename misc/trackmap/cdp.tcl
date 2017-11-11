proc dosh { cmd } {
set res [exec "term tab html" $cmd]
puts "<b><u>$cmd</b></u><br>"
puts "<table border=1>$res</table>"
}


puts "<html><head><title>cdp</title></head>"
puts "<body bgcolor=black text=white>"
dosh "show cdp neigh"
puts "</body></html>"
