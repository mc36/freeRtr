puts "calling"
puts "called"
puts "echo echoed"
set src ""
set trg ""

for {} {1<2} {} {
 set ln [gets -]
 if {[string equal $ln "echoed"]} break
 if {[string first called $ln] >= 0} {set trg [string range $ln 6 666]}
 if {[string first calling $ln] >= 0} {set src [string range $ln 7 666]}
 }

set url ""
set cmd ""

if {[string first sip:12@ $src] >= 0} {set url "http://speaker.mchome.nop.hu/player.class"}
if {[string first sip:13@ $src] >= 0} {set url "http://10.10.10.16/player.class"}
if {[string first sip:14@ $src] >= 0} {set url "http://player.mchome.nop.hu/player.class"}
if {[string first sip:15@ $src] >= 0} {set url "http://speaker.mchome.nop.hu/player.class"}
if {[string first sip:16@ $src] >= 0} {set url "http://speaker.mchome.nop.hu/player.class"}
if {[string first sip:17@ $src] >= 0} {set url "http://pince.mchome.nop.hu/player.class"}
if {[string first sip:31@ $src] >= 0} {set url "http://player.mchome.nop.hu/player.class"}
if {[string first sip:32@ $src] >= 0} {set url "http://player.mchome.nop.hu/player.class"}
if {[string first sip:33@ $src] >= 0} {set url "http://speaker.mchome.nop.hu/player.class"}
if {[string first sip:34@ $src] >= 0} {set url "http://lenovo.mchome.nop.hu/player.class"}
if {[string first sip:35@ $src] >= 0} {set url "http://10.10.10.16/player.class"}

if {[string first sip:01@ $trg] >= 0} {set cmd "cmd=prev"}
if {[string first sip:02@ $trg] >= 0} {set cmd "cmd=stop"}
if {[string first sip:03@ $trg] >= 0} {set cmd "cmd=next"}
if {[string first sip:04@ $trg] >= 0} {set cmd "cmd=vol&song=0"}
if {[string first sip:05@ $trg] >= 0} {set cmd "cmd=vol&song=50"}
if {[string first sip:06@ $trg] >= 0} {set cmd "cmd=vol&song=100"}
if {[string first sip:08@ $trg] >= 0} {set cmd "cmd=replay"}

if {[string length $url] < 1} {
  puts "play-start /nfs2/own/voice/player.wav"
  puts "play-wait"
  sleep 5
  puts "hangup"
  sleep 2
  return
  }

if {[string first sip:07 $trg] >= 0} {
  set tit [exec "attach shell1 curl -s $url?cmd=title"]
  puts "hangup"
  exec "packet message $src $trg $tit"
  sleep 2
  return
  }

if {[string length $cmd] < 1} {
  puts "play-start /nfs2/own/voice/player.wav"
  puts "play-wait"
  sleep 5
  puts "hangup"
  sleep 2
  return
  }

exec "flash receive /rtr/zzz5.html $url?$cmd"
puts "hangup"
sleep 2
