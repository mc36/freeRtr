proc dosh { cmd } {
set res [exec "term tab html" $cmd]
puts "<b><u>$cmd</b></u><br>"
puts "<table border=1>$res</table>"
}


puts "<html><head><title>lsa</title></head>"
puts "<body bgcolor=black text=white>"
dosh "show ipv4 lsrp 1 software"
dosh "show ipv6 lsrp 1 software"
puts "</body></html>"
