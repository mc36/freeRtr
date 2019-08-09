#!/bin/sh
qemu-system-x86_64 -enable-kvm -m 2048 -no-reboot -cdrom ../../binImg/rtr.iso -boot d
