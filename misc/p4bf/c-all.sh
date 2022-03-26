#!/bin/sh
#sudo apt install psmisc iproute2 net-tools socat tshark iperf gcc git telnet default-jdk-headless default-jre-headless bc
#sudo $SDE/p4studio/install-p4studio-dependencies.sh
#$SDE/p4studio/p4studio interactive
#rm -rf $SDE/build
#rm -rf $SDE/p4studio/dependencies
#rm -rf $SDE/packages
#rm -f `find $SDE -name *.a`
#
#git clone ssh://git@bitbucket.software.geant.org:7999/rare/rare.git
#git clone ssh://git@bitbucket.software.geant.org:7999/rare/rare-bf2556x-1t.git
#git clone ssh://git@bitbucket.software.geant.org:7999/rare/rare-nix.git
#wget http://src.nop.hu/misc/p4bf/ports.json
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
#resize2fs /dev/sdb1 6G
#fsck -f /dev/sdb1
#cfdisk /dev/sdb / resize 6.1G
#qemu-img resize --shrink p4bf.img 6.2G
#fallocate -d p4bf.img
#
cd ~/rare/p4src
export SDE=/home/mc36/bf-sde-9.8.0
export SDE_INSTALL=$SDE/install
$SDE/install/bin/bf-p4c -I. -I../profiles $@ -Xp4c="--disable-parse-depth-limit" bf_router.p4
if [ -f bf_router.tofino/pipe/tofino.bin ] ; then
  echo "******* compilation finished successfully *******"
  fi
rm -rf $SDE/install/bf_router.tofino
mv bf_router.tofino/bf_router.conf $SDE/install/share/p4/targets/tofino/
mv bf_router.tofino $SDE/install/
#
#$SDE/tools/p4_build.sh -I. -I../profiles $@ ./bf_router.p4
#cd $SDE/logs/p4-build/bf_router
#csplit make.log /p4c/ /p4c/
#tail -n+2 xx01
#rm xx0*
