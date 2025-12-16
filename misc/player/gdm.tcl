puts "restarting gdm"
exec "attach shell1 /etc/init.d/gdm3 stop"
exec "attach shell1 /etc/init.d/gdm3 start"
