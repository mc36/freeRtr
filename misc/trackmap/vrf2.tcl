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
set ifa [exec "show vrf $vrf numhist"]
set ifa [split $ifa "\r"]
set ifa [split $ifa "\n"]
set ifa [split $ifa "  "]
set ifa [split $ifa "  "]
puts "<html><head><title>$vrf traffic</title><meta http-equiv=refresh content=3;url=vrf2.tcl?vrf=$vrf></head>"
puts "<body bgcolor=black text=white>"
puts "<img src='grapher.class?ifa=$ifa'>"
puts "</body></html>"
