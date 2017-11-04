proc doer {cmd tit} {
puts "<b>$tit</b><br>"
puts "<pre><table border=1>"
puts [exec "terminal table html" $cmd]
puts "</table></pre>"
puts "<br><br>"
}


puts "<html><head><title>network map</title></head>"
doer "show mtrack" "summary"
doer "show mtrack v4" "ipv4"
doer "show mtrack v6" "ipv6"
puts "</body></html>"
