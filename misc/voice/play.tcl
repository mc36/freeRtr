set a [exec "show clock raw"]
set a [split $a "\r"]
set a [split $a "\n"]
set a "$a end"
set a [string trim $a]
set p [string first " " $a]
set a [string range $a 0 [expr $p - 1]]

for {} {1<2} {} {
  puts "play-start /nfs2/own/voice/play.wav"
  puts "play-wait"
  gets
  }
