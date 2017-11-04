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
set ifa [exec "show interface $iface numhist"]
set ifa [split $ifa "\r"]
set ifa [split $ifa "\n"]
set ifa [split $ifa "  "]
set ifa [split $ifa "  "]
puts "<html><head><title>$iface traffic</title><meta http-equiv=refresh content=3;url=iface2.tcl?iface=$iface></head>"
puts "<body bgcolor=black text=white>"
puts "<img src='grapher.class?ifa=$ifa'>"
puts "</body></html>"
