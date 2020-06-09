proc doer {cmd tit} {
puts "<b><u>$tit</u></b><br>"
puts "<pre><table border=1>"
puts [exec "terminal table html" $cmd]
puts "</table></pre>"
puts "<br><br>"
}


puts "<html><head><title>mesh tracker</title></head>"
puts "<body>"
doer "show mtrack" "summary"
doer "show mtrack probe" "show mtrack"
puts "</body></html>"