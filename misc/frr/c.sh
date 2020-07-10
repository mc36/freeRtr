#!/bin/sh
apt-get install frr socat telnet tshark
echo "mpls_router" >> /etc/modules-load.d/modules.conf
echo "mpls_iptunnel" >> /etc/modules-load.d/modules.conf
cp sysctl /etc/sysctl.d/00-frr.conf
cp initd /etc/init.d/rtr
chmod 755 /etc/init.d/rtr
update-rc.d rtr defaults
