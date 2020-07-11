#!/bin/sh
qemu-system-x86_64 -enable-kvm -m 1024 -no-reboot -monitor none -serial stdio -nographic -hda ../../binImg/rtr.dsk
