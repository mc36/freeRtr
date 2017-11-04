set ifnum 0
set interface ""
set z [string range $oid [expr [string last "." $oid]+1] [string length $oid]]

if {$z / 10000 == 7} {
        set ifnum [expr $z % 70000]
        set interface  "ethernet$ifnum"

}

if {$z / 10000 == 2} {
        set ifnum [expr $z % 20000]
        set interface  "atm$ifnum"
}


if {$z / 10000 == 9} {
        set ifnum [expr $z % 90000]
        set interface  "serial$ifnum"
}

if {$z / 10000 == 5} {
        set ifnum [expr $z % 50000]
        set interface  "cellular$ifnum"
}

if {$z / 10000 == 8} {
        set ifnum [expr $z % 80000]
        set interface  "loopback$ifnum"
}

if {$z / 10000 == 6} {
        set ifnum [expr $z % 60000]
        set interface  "dialer$ifnum"
}


if {$z / 10000 == 4} {
        set ifnum [expr $z % 40000]
        set interface  "bvi$ifnum"
}

if {$z / 10000 == 3} {
        set ifnum [expr $z % 30000]
        set interface  "bundle$ifnum"
}

if {$z / 10000 == 10} {
        set ifnum [expr $z % 100000]
        set interface  "tunnel$ifnum"
}



if {$z / 10000 == 1} {
        set ifnum [expr $z % 10000]
        set interface  "access$ifnum"
}



set b [exec "show interfaces $interface"]

