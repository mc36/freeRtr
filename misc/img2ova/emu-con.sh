#!/bin/sh
qemu-system-i386 -enable-kvm -m 1024 -no-reboot -monitor none -serial stdio -nographic -hda ../../binImg/rtr.dsk
