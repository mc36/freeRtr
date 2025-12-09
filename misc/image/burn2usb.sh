#!/bin/sh
sudo umount /mnt
sudo mkfs.vfat /dev/sdb1
sudo lilo -S /dev/null -M /dev/sdb
sudo syslinux /dev/sdb1
sudo mount -t vfat -o rw /dev/sdb1 /mnt
sudo cp boot.sys /mnt/syslinux.cfg
sudo cp ../../binImg/rtr-x86_64.krn /mnt/rtr.krn
sudo cp ../../binImg/rtr-x86_64.ird /mnt/rtr.ird
sudo umount /mnt
