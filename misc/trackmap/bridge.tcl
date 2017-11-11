proc dosh { cmd } {
set res [exec "term tab fancy" $cmd]
puts "<b><u>$cmd</b></u><br>"
puts "<pre>$res</pre>"
}


puts "<html><head><title>bridge</title></head>"
puts "<body bgcolor=black text=white>"
dosh "show bridge 1"
dosh "show bridge 2"
dosh "show bridge 3"
dosh "show bridge 4"
puts "</body></html>"
