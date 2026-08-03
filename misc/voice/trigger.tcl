set frst ""
set scnd ""
for {} {1<2} {} {
 set ln [gets -]
 if {[string equal $ln "."]} break
 set sh "$sh$ln<br/>"
 if {[string first par.n1 $ln] >= 0} {set frst [string range $ln 7 666]}
 if {[string first par.n2 $ln] >= 0} {set scnd [string range $ln 7 666]}
 }

if {[string length $frst] < 1} {
 puts "no interface"
 return
 }
if {[string length $scnd] < 1} {
 puts "no command"
 return
 }

config "int $frst" "$scnd"

puts "<html><head><title>trigger</title></head>"
puts "executed $scnd at $frst<br/>"
set a [exec "term tab html" "show interfaces description"]
puts "<table border=1>$a</table>"
puts "</body></html>"
