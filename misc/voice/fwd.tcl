puts "calling"
set t [gets -]
set p [string first " " $t]
set t [string range $t [expr $p + 1] 666]

puts "play-start /nfs2/own/voice/fwd.wav"
puts "play-wait"

puts "forward-start 0 $t"

for {} {1<2} {} {
  sleep 1
  puts "forward-running"
  for {} {1<2} {} {
    set a [gets -]
    if {[string first forward-running $a] >= 0} {break;}
    }
  if {[string first false $a] >= 0} {break;}
}
puts "forward-stop"
puts "forward-wait"
puts "hangup"
sleep 2
