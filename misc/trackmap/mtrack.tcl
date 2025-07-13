proc doer {cmd tit} {
puts "<b><u>$tit</u></b><br/>"
puts "<pre><table border=1>"
puts [exec "terminal table html" "$cmd stat"]
puts "</table></pre><pre><table border=1>"
puts [exec "terminal table html" "$cmd peer"]
puts "</table></pre><pre><table border=1>"
puts [exec "terminal table html" "$cmd reach"]
puts "</table></pre><pre><table border=1>"
puts [exec "terminal table html" "$cmd time"]
puts "</table></pre>"
puts "<br/><br/>"
}


puts "<html><head><title>mesh tracker</title></head>"
puts "<body bgcolor=black text=white>"
doer "show mtrack v4" "ipv4 unicast"
doer "show mtrack v6" "ipv6 unicast"
doer "show mtrack v4m" "ipv4 multicast"
doer "show mtrack v6m" "ipv6 multicast"
puts "</body></html>"
