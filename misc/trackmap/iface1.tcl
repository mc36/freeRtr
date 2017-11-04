set sh ""
set iface ""
for {} {1<2} {} {
  set ln [gets -]
  if {[string equal $ln "."]} break
  set sh "$sh$ln<br>"
  if {[string first auth $ln] >= 0} {set auth [string range $ln 5 666]}
  if {[string first clnt $ln] >= 0} {set clnt [string range $ln 5 666]}
  if {[string first par.iface $ln] >= 0} {set iface [string range $ln 10 666]}
  }
set ifc [exec "terminal tablemode fancy" "show interfaces $iface full"]
puts "<html><head><title>$iface traffic</title><meta http-equiv=refresh content=3;url=iface1.tcl?iface=$iface></head>"
puts "<body bgcolor=black text=white>"
puts "<pre>$ifc</pre>"
puts "</body></html>"
