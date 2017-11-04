set adr a
set vrf a
set int a
script local.tcl
local
param
set a [exec "ping $adr /vrf $vrf /int $int"]
puts "<html><body>result:<br><br><pre>$a</pre></body></html>"
