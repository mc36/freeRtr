set a [exec "show clock raw"]
set a [split $a "\r"]
set a [split $a "\n"]
set a "$a end"
set a [string trim $a]
set p [string first " " $a]
set a [string range $a 0 [expr $p - 1]]

puts "record-start /nfs2/own/voice/got$a.wav"

puts "play-start /nfs2/own/voice/leave.wav"
puts "play-wait"

sleep 30
puts "record-stop"
puts "record-wait"

puts "play-start /nfs2/own/voice/got$a.wav"
sleep 30
puts "play-wait"

puts "hangup"
sleep 2
