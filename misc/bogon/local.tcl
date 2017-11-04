proc local {} {
global ipa
global ipb
global tag
global vrf
set ipa "10.10.10.8"
set ipb "2001:db8:1101::8"
set tag "666"
set vrf "home"
}

proc param {} {
global adr
for {} {1<2} {} {
 set ln [gets -]
 if {[string equal $ln "."]} break
 set sh "$sh$ln<br>"
 if {[string first adr $ln] >= 0} {set adr [string range $ln 8 666]}
 }
}
