puts [exec "attach shell2 acpi -t | cut -d"," -f2 | cut -d" " -f2 | cut -d"." -f1"]
