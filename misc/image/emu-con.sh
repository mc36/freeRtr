#!/bin/sh
qemu-system-i386 -enable-kvm -m 2048 -no-reboot  -monitor none -serial stdio -nographic -cdrom ../../binImg/rtr.iso -boot d
