#!/bin/sh
sudo umount /mnt
sudo mkdosfs /dev/sdb1
sudo lilo  -S /dev/null -M /dev/sdb
sudo syslinux /dev/sdb1
sudo mount -t vfat -o rw /dev/sdb1 /mnt
sudo cp boot.cfg /mnt/syslinux.cfg
sudo cp ../../binImg/rtr.krn /mnt/rtr.krn
sudo cp ../../binImg/rtr.ird /mnt/rtr.ird
sudo umount /mnt
