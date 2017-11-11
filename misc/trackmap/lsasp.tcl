proc dosh { cmd } {
set res [exec $cmd]
puts "<b><u>$cmd</b></u><br>"
puts "<pre>$res</pre>"
}


puts "<html><head><title>lsa</title></head>"
puts "<body bgcolor=black text=white>"
dosh "show ipv4 lsrp 1 spf"
dosh "show ipv6 lsrp 1 spf"
puts "</body></html>"
