set path ""
set domain ""
set server ""
set type ""
for {} {1<2} {} {
 set ln [gets -]
 if {[string equal $ln "."]} break
 if {[string first path $ln] >= 0} {set path [string range $ln 5 666]}
 if {[string first par.domain $ln] >= 0} {set domain [string range $ln 11 666]}
 if {[string first par.server $ln] >= 0} {set server [string range $ln 11 666]}
 if {[string first par.type $ln] >= 0} {set type [string range $ln 9 666]}
 }
set cmd "lookup $type $domain $server"
set res [exec $cmd]
puts "<html><head><title>lookup</title></head>"
puts "<body>"
puts "<u>result:</u><br><pre>$res</pre><br>"
puts "</body></html>"
