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
 puts "no device"
 return
 }
if {[string length $scnd] < 1} {
 puts "no number"
 return
 }

puts "executing $scnd at $frst"
exec "attach shell1 java -cp /nfs2/own/web/home/ phoneExecute $frst $scnd"
