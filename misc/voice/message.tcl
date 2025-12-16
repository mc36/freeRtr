set src ""
set trg ""
set msg ""

for {} {1<2} {} {
 set ln [gets -]
 if {[string equal $ln "."]} break
 if {[string first from= $ln] == 0} {set src [string range $ln 5 666]}
 if {[string first to= $ln] == 0} {set trg [string range $ln 3 666]}
 if {[string first text= $ln] == 0} {set msg "$msg [string range $ln 5 666]"}
 }



exec "packet message $src $trg dear $src, i've got $msg, bye: $trg"
