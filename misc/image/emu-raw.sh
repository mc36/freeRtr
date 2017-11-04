#!/bin/sh
qemu-system-i386 -enable-kvm -k en-us -m 2048 -no-reboot -kernel ../../binImg/rtr.krn -initrd ../binImg/rtr.ird
