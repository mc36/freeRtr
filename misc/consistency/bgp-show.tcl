proc dosh { cmd } {
global sh
set res [exec "term tab html" $cmd]
set sh "$sh<b><u>$cmd</b></u><br><table border=1><pre>$res</pre></table>"
}
set sh ""
dosh "show clock | html"
dosh "show platform | html"
dosh "show banner"
dosh "show logo"
dosh "show ipv4 bgp 1955 unicast summary"
dosh "show ipv6 bgp 1955 unicast summary"
puts "<html><head><title>hbone lg</title></head>"
puts "<body bgcolor=black text=white>"
puts "$sh"
puts "</body></html>"
