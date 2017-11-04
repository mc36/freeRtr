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

proc getline {} {
global b
global c
global d
global e
global f
global g
global h

proc getword {} {
global b
set b [string trim $b]
set p [string first " " $b]
if {$p < 0} {
  set p $b
  set b ""
  return $p
  }
set c [string range $b 0 [expr $p - 1]]
set b [string range $b $p [string length $b]]
set b [string trim $b]
return $c
}

set c [getword]
set d [getword]
set e [getword]
set f [getword]
set g [getword]
set h [getword]
}

set b [exec "show vrf"]
set b [split $b "\r"]
set b [split $b "\n"]
set c ""
set d ""
set e ""
set f ""
set g ""
set h ""
getline

puts "<html><head><title>vrfs</title><meta http-equiv=refresh content=3;url=vrf.tcl></head>"
puts "<body bgcolor=black text=white>"
puts "<table border=1>"
puts "<tr><td>vrf</td><td>rd</td><td>int4</td><td>int6</td><td>pack</td><td>byte</td></tr>"
for {} {[string length $b] > 0} {} {
  getline
  puts "<tr><td><a href=vrf1.tcl?vrf=$c>$c</a></td><td>$d</td><td>$e</td><td>$f</td><td>$g</td><td>$h</td></tr>"
  }
puts "</table>"
puts "</body></html>"
