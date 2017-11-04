set sh ""
set iface ""

proc getline {} {
global c
global d
global e


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
}

set b [exec "show ipv4 arp bvi4"]
set b [split $b "\r"]
set b [split $b "\n"]
set c ""
set d ""
set e ""
getline

puts "<html><head><title>arp</title><meta http-equiv=refresh content=1;url=arp.tcl></head>"
puts "<body bgcolor=black text=white>"
puts "<table border=1>"
puts "<tr><td>mac</td><td>ip</td><td>time</td></tr>"
for {} {[string length $b] > 0} {} {
  getline
  puts "<tr><td>$c</a></td><td>$d</td><td>$e</td></tr>"
  }
puts "</table>"
puts "</body></html>"
