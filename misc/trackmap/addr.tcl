set clnt ""
for {} {1<2} {} {
 set ln [gets -]
 if {[string equal $ln "."]} break
 set sh "$sh$ln<br/>"
 if {[string first clnt $ln] >= 0} {set clnt [string range $ln 5 666]}
 }
puts "<html><head><title>address</title></head>"
puts "<body bgcolor=black text=white>"
puts "$clnt"
puts "</body></html>"
