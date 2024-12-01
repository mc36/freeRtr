#!/bin/sh
#sudo apt-get install psmisc iproute2 net-tools socat tshark iperf gcc git telnet python3-yappi python3-grpcio
#  default-jdk-headless default-jre-headless bc jq libthrift-dev libboost-dev libboost-all-dev i2c-tools libi2c-dev linux-headers-amd64
#sudo $SDE/p4studio/install-p4studio-dependencies.sh
#$SDE/p4studio/p4studio interactive
#$SDE/p4studio/p4studio profile apply ~/model.yaml
#sudo rm -rf $SDE/build
#sudo rm -rf $SDE/packages
#sudo rm -rf $SDE/install/p4i
#sudo rm -rf $SDE/p4studio/dependencies
#sudo rm -f `find $SDE -name *.a`
#
#git clone https://bitbucket.software.geant.org/scm/rare/rare.git
#git clone https://bitbucket.software.geant.org/scm/rare/rare-bf2556x-1t.git
#git clone https://bitbucket.software.geant.org/scm/rare/rare-nix.git
#wget http://src.freertr.org/misc/p4bf/ports1.json
#gcc -O3 -o dummyCon.bin ../native/dummyCon.c
#cp initd /etc/init.d/rtr
#chmod 755 /etc/init.d/rtr
#update-rc.d rtr defaults
#systemctl mask serial-getty@ttyS0
#systemctl disable serial-getty@ttyS0
#echo "mc36 ALL=(ALL) NOPASSWD:ALL" >> /etc/sudoers
#
#fdisk /dev/sdb / p
#fsck -f /dev/sdb1
#resize2fs /dev/sdb1 7G
#fsck -f /dev/sdb1
#cfdisk /dev/sdb / resize 7.1G
#qemu-img resize --shrink p4bf.img 7.2G
#fallocate -d p4bf.img
#
cd ~/rare/p4src
export SDE=/home/mc36/bf-sde-9.13.4
export SDE_INSTALL=$SDE/install
rm -rf $SDE/install/bf_router.tofino
#-b tofino2 -a t2na
$SDE/install/bin/bf-p4c -I. -I../profiles $@ -Xp4c="--disable-parse-depth-limit" --create-graphs --display-power-budget bf_router.p4
if [ -f bf_router.tofino/pipe/tofino.bin ] ; then
  echo "******* compilation finished successfully *******"
  else
  echo "************************** COMPiLATiON FAiLED **************************"
  fi
#jq '.p4_devices[0].p4_programs[0] += {"board-port-map":"share/port_map.json"}' bf_router.tofino/bf_router.conf > $SDE/install/share/p4/targets/tofino/bf_router.conf
mv bf_router.tofino/bf_router.conf $SDE/install/share/p4/targets/tofino/
mv bf_router.tofino $SDE/install/
#
#$SDE/tools/p4_build.sh -I. -I../profiles $@ ./bf_router.p4
#cd $SDE/logs/p4-build/bf_router
#csplit make.log /p4c/ /p4c/
#tail -n+2 xx01
#rm xx0*
