set sh ""
set domain "d.nop.hu"
set auth ""
set clnt ""
set prot ""
for {} {1<2} {} {
 set ln [gets -]
 if {[string equal $ln "."]} break
 set sh "$sh$ln<br>"
 if {[string first auth $ln] >= 0} {set auth [string range $ln 5 666]}
 if {[string first clnt $ln] >= 0} {set clnt [string range $ln 5 666]}
 }
if {[string first . $clnt] >= 0} {set prot "ip4a"} else {set prot "ip6a"}
if {[string length $auth] > 0} {
 config "server dns ns" "zone $domain rr $auth.$domain $prot $clnt"
 }
puts "<html><head><title>dyndns</title><meta http-equiv=refresh content=120;url=dyndns.tcl></head>"
puts "<body bgcolor=black text=white>"
puts "updated $auth.$domain to $clnt address.<br>"
puts "<u>parameters</u><br>$sh<br>"
puts "</body></html>"
