set a [exec "show clock raw"]
set a [split $a "\r"]
set a [split $a "\n"]
set a "$a end"
set a [string trim $a]
set p [string first " " $a]
set a [string range $a 0 [expr $p - 1]]

puts "dtmf-start"

for {} {1<2} {} {
  puts "play-start /nfs2/own/voice/music.wav"
  for {} {1<2} {} {
    sleep 1
    puts "play-running"
    for {} {1<2} {} {
      set a [gets -]
      if {[string first play-running $a] >= 0} {break;}
      }
    if {[string first false $a] >= 0} {break;}
    }
  puts "play-stop"
  puts "play-wait"
  sleep 1
  }
puts "hangup"
