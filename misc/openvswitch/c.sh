apt-get install openvswitch-switch socat busybox gcc telnet
gcc -O3 -o cons.bin cons.c
cp initd /etc/init.d/rtr
chmod 777 /etc/init.d/rtr
update-rc.d rtr defaults
