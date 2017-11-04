set asn a
set adr a
script local.tcl
local
param
set a [exec "show ipv4 bgp $asn unicast database $adr"]
puts "<html><body>result:<br><br><pre>$a</pre></body></html>"
