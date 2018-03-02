#!/bin/sh
qemu-system-i386 -enable-kvm -m 1024 -no-reboot -hda ../../binImg/rtr.dsk
