set asn a
set adr a
script local.tcl
local
param
set a [exec "show ipv6 bgp $asn unicast allroute $adr"]
puts "<html><body>result:<br><br><pre>$a</pre></body></html>"
