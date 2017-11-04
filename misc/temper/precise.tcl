puts "<html><head><title>temperature</title></head><body><pre>"
puts [exec "attach shell2 python /rtr/web/tempercli.py"]
puts "</pre></body></html>"
