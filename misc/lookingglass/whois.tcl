set vrf a
set int a
set adr a
script local.tcl
local
param
set a [exec "show whois $adr"]
puts "<html><body>result:<br><br><pre>$a</pre></body></html>"
