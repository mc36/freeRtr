proc local {} {
global asn
global vrf
global int
set asn "1"
set vrf "dn42"
set int "lo1"
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
