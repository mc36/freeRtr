set vrf a
set int a
set adr a
script local.tcl
local
param
set a [exec "traceroute $adr vrf $vrf sou $int"]
puts "<html><body>result:<br/><br/><pre>$a</pre></body></html>"
