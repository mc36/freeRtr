set asn a
script local.tcl
local
set a [exec "terminal tablemode html" "show ipv4 msdp $asn neighbor"]
puts "<html><body>peers:<br><br><table border=1>$a</table></body></html>"
