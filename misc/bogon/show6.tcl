set ipa a
set ipb a
set tag a
set vrf a
script local.tcl
local
set a [exec "show startup | reginc ipv6.route.*tag.$tag"]
puts "<html><body>bogons:<br><pre>$a</pre></body></html>"
