proc dosh { cmd } {
set res [exec "term tab html" $cmd]
puts "<b><u>$cmd</b></u><br/>"
puts "<table border=1>$res</table>"
}


puts "<html><head><title>lsa</title></head>"
puts "<body bgcolor=black text=white>"
dosh "show ipv4 segrout inet"
dosh "show ipv6 segrout inet"
puts "</body></html>"
