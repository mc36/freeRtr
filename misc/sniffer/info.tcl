set a [exec "flash info /rtr/web/capture.pcap"]
set b [exec "flash disk /rtr/web/"]
puts "<html><body>capture:<br><pre>$a</pre><br>storage:<br><pre>$b</pre></body></html>"
