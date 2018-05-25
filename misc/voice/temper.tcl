puts "called"
set trg [gets -]

set tmp ""

if {[string first sip:20@ $trg] >= 0} {set tmp "20"}
if {[string first sip:21@ $trg] >= 0} {set tmp "21"}
if {[string first sip:22@ $trg] >= 0} {set tmp "22"}
if {[string first sip:23@ $trg] >= 0} {set tmp "23"}
if {[string first sip:24@ $trg] >= 0} {set tmp "24"}
if {[string first sip:25@ $trg] >= 0} {set tmp "25"}


if {[string length $tmp] < 1} {
  puts "hangup"
  sleep 2
  return
  }

exec "flash receive /rtr/zzz6.html http://door.mchome.nop.hu/temper.class?temp=$tmp&cmd=heat"

puts "play-start /nfs2/own/voice/temper.wav"
puts "play-wait"
puts "play-start /nfs2/own/voice/temper$tmp.wav"
puts "play-wait"
sleep 10
puts "hangup"
sleep 2

