set frst ""
set scnd ""
for {} {1<2} {} {
 set ln [gets -]
 if {[string equal $ln "."]} break
 set sh "$sh$ln<br>"
 if {[string first par.n1 $ln] >= 0} {set frst [string range $ln 7 666]}
 if {[string first par.n2 $ln] >= 0} {set scnd [string range $ln 7 666]}
 }

if {[string length $frst] < 1} {
 puts "no first number"
 return
 }
if {[string length $scnd] < 1} {
 puts "no second number"
 return
 }

puts "connecting $frst to $scnd"
execbg "packet voice $frst $scnd /nfs2/own/voice/fwd.tcl"
