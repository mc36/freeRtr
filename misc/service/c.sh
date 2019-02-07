#!/bin/sh
echo net.ipv6.conf.all.disable_ipv6=1 > /etc/sysctl.d/disableipv6.conf
cp systemd /lib/systemd/system/rtr.service
cp initd /etc/init.d/rtr
cp network /etc/network/interfaces
cp resolv /etc/resolv.conf
chmod 777 /etc/init.d/rtr
rm /usr/lib/systemd/network/*
systemctl set-default multi-user.target
export SVC="network-manager NetworkManager ModemManager systemd-networkd systemd-networkd-wait-online wpa_supplicant"
systemctl disable $SVC
systemctl mask $SVC
systemctl stop $SVC
systemctl enable rtr
update-rc.d rtr defaults
mkdir /rtr
cp ../default.cfg /rtr/rtr-sw.txt
cp ../../src/rtr.jar /rtr/
cp /proc/net/dev /rtr/hwdet.eth
cp /proc/tty/driver/serial /rtr/hwdet.ser
ifconfig -a > /rtr/hwdet.mac
java -jar /rtr/rtr.jar test hwdet tuntap 10.255.255.1/24 path /rtr/
chmod 777 /rtr/*.sh
ls -l /rtr/
