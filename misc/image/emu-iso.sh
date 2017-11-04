#!/bin/sh
qemu-system-i386 -enable-kvm -m 2048 -no-reboot -cdrom ../../binImg/rtr.iso -boot d
