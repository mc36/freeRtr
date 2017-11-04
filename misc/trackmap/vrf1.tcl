set sh ""
set vrf ""
for {} {1<2} {} {
  set ln [gets -]
  if {[string equal $ln "."]} break
  set sh "$sh$ln<br>"
  if {[string first auth $ln] >= 0} {set auth [string range $ln 5 666]}
  if {[string first clnt $ln] >= 0} {set clnt [string range $ln 5 666]}
  if {[string first par.vrf $ln] >= 0} {set vrf [string range $ln 8 666]}
  }
set ifc [exec "show vrf $vrf full"]
puts "<html><head><title>$vrf traffic</title><meta http-equiv=refresh content=3;url=vrf1.tcl?vrf=$vrf></head>"
puts "<body bgcolor=black text=white>"
puts "<pre>$ifc</pre>"
puts "</body></html>"
