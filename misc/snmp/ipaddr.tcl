set ipbase "1.3.6.1.2.1.4.20.1."
set ipbl [string length $ipbase]
set ipcol 0
set ipsfx ""
if {[string first $ipbase $oid] == 0} {
set iprest [string range $oid $ipbl [string length $oid]]
set ippos [string first "." $iprest]
if {$ippos < 0} {
set ipcol $iprest
} else {
set ipcol [string range $iprest 0 [expr $ippos-1]]
set ipsfx [string range $iprest [expr $ippos+1] [string length $iprest]]
}
}
set ra 0
set rb 0
set rc 0
set rd 0
set rn 0
set ipt $ipsfx
while { [string length $ipt] > 0 }{
set ippos [string first "." $ipt]
if {$ippos < 0} {
set ipitem $ipt
set ipt ""
} else {
set ipitem [string range $ipt 0 [expr $ippos-1]]
set ipt [string range $ipt [expr $ippos+1] [string length $ipt]]
}
set rn [expr $rn+1]
if {$rn == 1} {set ra $ipitem}
if {$rn == 2} {set rb $ipitem}
if {$rn == 3} {set rc $ipitem}
if {$rn == 4} {set rd $ipitem}
}
set qh [expr $ra*256]
set qh [expr $qh+$rb]
set qh [expr $qh+1]
set ql [expr $rc*256]
set ql [expr $ql+$rd]
set ql [expr $ql+1]
set ipstr 1
if {$rn < 4} {
set ipstr 0
}
if {$ipcol < 1} {
set qh 0
set ql 0
set ipstr 1
set ipcol 1
}
set qlx $ql
if {$ipstr == 0} {
set qlx [expr $ql-1]
}
set ipwant $ipcol
set ipnext 0
if {[string equal $mode next]} {
set ipnext 1
}
set ipall [exec "show ipv4 interface"]
set ipall [string trim $ipall]
set ipdone 0
if {$ipnext == 0} {
if {$rn < 4} {
set ipdone 1
}
if {$rn > 4} {
set ipdone 1
}
if {$ipwant > 3} {
set ipdone 1
}
}
while { $ipdone < 1 }{
set ipfound 0
set bph 0
set bpl 0
set baddr ""
set bmask ""
set bname ""
set iptxt $ipall
set ippos [string first "\n" $iptxt]
if {$ippos < 0} {
set iptxt ""
} else {
set iptxt [string range $iptxt [expr $ippos+1] [string length $iptxt]]
}
while { [string length $iptxt] > 0 }{
set ippos [string first "\n" $iptxt]
if {$ippos < 0} {
set iprow $iptxt
set iptxt ""
} else {
set iprow [string range $iptxt 0 [expr $ippos-1]]
set iptxt [string range $iptxt [expr $ippos+1] [string length $iptxt]]
}
set iprow [string trim $iprow]
set ipnm ""
set ipad ""
set ipmk ""
set ipw 0
set ipr $iprow
while { $ipw < 4 }{
set ipr [string trim $ipr]
set ippos [string first " " $ipr]
if {$ippos < 0} {
set ipcell $ipr
set ipr ""
} else {
set ipcell [string range $ipr 0 [expr $ippos-1]]
set ipr [string range $ipr $ippos [string length $ipr]]
}
set ipw [expr $ipw+1]
if {$ipw == 1} {set ipnm $ipcell}
if {$ipw == 3} {set ipad $ipcell}
if {$ipw == 4} {set ipmk $ipcell}
}
if {[string length $ipad] > 0} {
set oa 0
set ob 0
set oc 0
set od 0
set ipw 0
set ipr $ipad
while { [string length $ipr] > 0 }{
set ippos [string first "." $ipr]
if {$ippos < 0} {
set ipcell $ipr
set ipr ""
} else {
set ipcell [string range $ipr 0 [expr $ippos-1]]
set ipr [string range $ipr [expr $ippos+1] [string length $ipr]]
}
set ipw [expr $ipw+1]
if {$ipw == 1} {set oa $ipcell}
if {$ipw == 2} {set ob $ipcell}
if {$ipw == 3} {set oc $ipcell}
if {$ipw == 4} {set od $ipcell}
}
set ph [expr $oa*256]
set ph [expr $ph+$ob]
set ph [expr $ph+1]
set pl [expr $oc*256]
set pl [expr $pl+$od]
set pl [expr $pl+1]
set ipok 0
if {$ipnext == 1} {
if {$ph > $qh} {set ipok 1}
if {$ph == $qh} {
if {$pl > $qlx} {set ipok 1}
}
} else {
if {$ph == $qh} {
if {$pl == $ql} {set ipok 1}
}
}
if {$ipok == 1} {
set ipbetter 0
if {$ipfound == 0} {set ipbetter 1}
if {$ph < $bph} {set ipbetter 1}
if {$ph == $bph} {
if {$pl < $bpl} {set ipbetter 1}
}
if {$ipbetter == 1} {
set ipfound 1
set bph $ph
set bpl $pl
set baddr $ipad
set bmask $ipmk
set bname $ipnm
}
}
}
}
if {$ipnext == 1} {
if {$ipfound == 1} {
puts "$ipbase$ipwant.$baddr"
set ipdone 1
} else {
if {$ipwant < 3} {
set ipwant [expr $ipwant+1]
set qh 0
set ql 0
set qlx 0
} else {
puts "1.3.6.1.2.1.4.22.1"
set ipdone 1
}
}
} else {
if {$ipfound == 1} {
if {$ipwant == 1} {
puts "tag=0 class=1  adrip4=$baddr"
}
if {$ipwant == 2} {
set ipidx [nametonumber $bname]
puts "tag=integer int32=$ipidx"
}
if {$ipwant == 3} {
puts "tag=0 class=1  adrip4=$bmask"
}
}
set ipdone 1
}
}
