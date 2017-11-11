proc dosh { cmd } {
set res [exec "term tab html" $cmd]
puts "<b><u>$cmd</b></u><br>"
puts "<table border=1>$res</table>"
}


puts "<html><head><title>lldp</title></head>"
puts "<body bgcolor=black text=white>"
dosh "show lldp neigh"
puts "</body></html>"
