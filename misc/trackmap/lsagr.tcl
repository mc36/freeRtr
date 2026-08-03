set a [exec "show ipv4 lsrp 1 graph half"]
set b [string urlencode $a]
puts "<html><head><title>lsa</title>"
puts "<meta http-equiv=refresh content=1;url=http://dreampuf.github.io/GraphvizOnline/?engine=dot#$b></head>"
puts "<body>redirecting</body></html>"
