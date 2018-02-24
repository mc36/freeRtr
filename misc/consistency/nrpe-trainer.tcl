set sh ""
set check ""
for {} {1<2} {} {
 set ln [gets -]
 if {[string equal $ln "."]} break
 if {[string first par.check $ln] >= 0} {set check [string range $ln 10 666]}
 }

if {[string length $check] < 1} {
 puts "got no check."
 return
 }

set diff [exec "show config"]

if {[string length $diff] > 3} {
 puts "temporary config in effect, not updated anything!"
 return
 }


config "server nrpe hbone" "check $check train"
exec "write"
exec "write network"

puts "check $check trained."
