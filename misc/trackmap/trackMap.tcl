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



set b [exec "show tracker"]
set b [split $b "\r"]
set b [split $b "\n"]
set c ""
set d ""
set e ""
set f ""
set g ""
set h ""
getline
puts "<html><head><title>network map</title><meta http-equiv=refresh content=3;url=trackMap.tcl></head>"
puts "<body bgcolor=black text=white>"
set a "trackMap.class?home.png"
for {} {[string length $b] > 0} {} {
  getline
  set a "$a&$c-$g"
  }
puts "<a href=trackMap.imgmap><img alt=map src=$a ismap></a><br>"
set a [exec "term tab html" "show tracker"]
puts "<table border=1>$a</table>"
puts "</body></html>"
