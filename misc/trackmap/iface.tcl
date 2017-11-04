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

proc getline {} {
global b
global c
global d
global e
global f

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
set q [getword]
set d [getword]
set e [getword]
set f [getword]
}

set b [exec "show interfaces traffic"]
set b [split $b "\r"]
set b [split $b "\n"]
set c ""
set d ""
set e ""
set f ""
getline

puts "<html><head><title>interfaces</title><meta http-equiv=refresh content=3;url=iface.tcl></head>"
puts "<body bgcolor=black text=white>"
puts "<table border=1>"
puts "<tr><td>interface</td><td>tx</td><td>rx</td><td>drop</td></tr>"
for {} {[string length $b] > 0} {} {
  getline
  puts "<tr><td><a href=iface1.tcl?iface=$c>$c</a></td><td>$d</td><td>$e</td><td>$f</td></tr>"
  }
puts "</table>"
puts "</body></html>"
