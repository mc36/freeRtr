proc putOpt {typ} {
global type
if {[string compare $type $typ] == 0} {set par " selected"} else {set par ""}
puts "<option$par>$typ</option>"
}

set sh ""
set path ""
set domain ""
set server ""
set type ""
for {} {1<2} {} {
 set ln [gets -]
 if {[string equal $ln "."]} break
 set sh "$sh$ln<br>"
 if {[string first path $ln] >= 0} {set path [string range $ln 5 666]}
 if {[string first par.domain $ln] >= 0} {set domain [string range $ln 11 666]}
 if {[string first par.server $ln] >= 0} {set server [string range $ln 11 666]}
 if {[string first par.type $ln] >= 0} {set type [string range $ln 9 666]}
 }
set cmd "lookup $type $domain $server"
set res [exec $cmd]
puts "<html><head><title>lookup</title></head>"
puts "<body bgcolor=black text=white>"
puts "<form action=$path>"
puts "domain: <input type=text name=domain value=$domain><br>"
puts "server: <input type=text name=server value=$server><br>"
puts "type: <select name=type>"
putOpt ipv4
putOpt ipv6
putOpt mail
putOpt dns
putOpt srv
putOpt soa
putOpt txt
putOpt reverse
putOpt recur-ipv4
putOpt recur-ipv6
putOpt recur-mail
putOpt recur-dns
putOpt recur-soa
putOpt recur-srv
putOpt recur-txt
putOpt zone
puts "</select><br><input type=submit value=lookup><br>"
puts "</form>"
puts "<u>command</u>: '$cmd'<br>"
puts "<u>results</u><br><pre>$res</pre><br>"
puts "<u>parameters</u><br>$sh<br>"
puts "</body></html>"
