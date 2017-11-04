set ipa a
set ipb a
set tag a
set vrf a
set adr a
script local.tcl
local
param
set a [config "no ipv4 route $vrf $adr 255.255.255.255 $ipa tag $tag"]
set a "$a[exec write]"
set b "write network"
set a "$a[exec $b]"
puts "<html><body>result:<br><pre>$a</pre></body></html>"
