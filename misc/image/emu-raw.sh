#!/bin/sh
qemu-system-x86_64 -enable-kvm -k en-us -m 2048 -no-reboot -kernel ../../binImg/rtr.krn -initrd ../binImg/rtr.ird
