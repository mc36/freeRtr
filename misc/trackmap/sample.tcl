set sh ""
for {} {1<2} {} {
 set ln [gets -]
 if {[string equal $ln "."]} break
 set sh "$sh$ln<br>"
 }
puts "<html><head><title>sample</title></head>"
puts "<body bgcolor=black text=white>"
puts "<u>parameters</u><br>$sh<br>"
puts "</body></html>"
