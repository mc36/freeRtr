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


set b [exec "attach shell2 python3 /rtr/temper/temper.py"]
getword
getword
getword
getword
getword
getword
set a [getword]
set p [string first "C" $a]
set c [string range $a 0 [expr $p - 1]]
puts $c
