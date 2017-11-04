set ipa a
set ipb a
set tag a
set vrf a
set adr a
script local.tcl
local
param
set a [config "no ipv6 route $vrf $adr ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff $ipb tag $tag"]
set a "$a[exec write]"
set b "write network"
set a "$a[exec $b]"
puts "<html><body>result:<br><pre>$a</pre></body></html>"
