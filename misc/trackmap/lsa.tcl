proc dosh { cmd } {
set res [exec "term tab html" $cmd]
puts "<b><u>$cmd</b></u><br>"
puts "<table border=1>$res</table>"
}


set path ""
set rtrid ""
for {} {1<2} {} {
 set ln [gets -]
 if {[string equal $ln "."]} break
 set sh "$sh$ln<br>"
 if {[string first path $ln] >= 0} {set path [string range $ln 5 666]}
 if {[string first par.rtrid $ln] >= 0} {set rtrid [string range $ln 10 666]}
 }


puts "<html><head><title>lsa</title></head>"
puts "<body bgcolor=black text=white>"
puts "<form action=$path>"
puts "rtrid: <input type=text name=rtrid value=$rtrid><br>"
puts "<br><input type=submit value=query><br></form>"
dosh "show ipv4 lsrp 1 database $rtrid"
dosh "show ipv6 lsrp 1 database $rtrid"
puts "</body></html>"
