#!/bin/sh
apt-get install frr frr-rpki-rtrlib psmisc iproute2 net-tools socat telnet tshark iperf
echo "mpls_router" >> /etc/modules-load.d/modules.conf
echo "mpls_iptunnel" >> /etc/modules-load.d/modules.conf
sed -i "s/=no/=yes/g" /etc/frr/daemons
cp sysctl /etc/sysctl.d/00-frr.conf
cp initd /etc/init.d/rtr
chmod 755 /etc/init.d/rtr
update-rc.d rtr defaults
