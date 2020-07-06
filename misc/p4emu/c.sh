apt-get install gcc telnet libpcap-dev wget dpdk dpdk-dev
gcc -O3 -o cons.bin cons.c
cp initd /etc/init.d/rtr
chmod 777 /etc/init.d/rtr
update-rc.d rtr defaults
