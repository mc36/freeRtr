set sh ""
set mac "0000.1111.2222"
for {} {1<2} {} {
 set ln [gets -]
 if {[string equal $ln "."]} break
 set sh "$sh$ln<br>"
 if {[string first par.mac $ln] < 0} continue
 set mac [string range $ln 8 666]
 }
set cmd "packet wakeup eth2 $mac"
puts "<html><head><title>waker</title></head>"
puts "<body bgcolor=black text=white>"
puts "<b>mac=$mac</b><br><br>"
puts "<u>command</u><br>$cmd<br>[exec $cmd]<br><br>"
puts "<u>parameters</u><br>$sh<br>"
puts "</body></html>"
