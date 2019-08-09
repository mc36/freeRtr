#!/bin/sh
qemu-system-x86_64 -enable-kvm -m 2048 -no-reboot  -monitor none -serial stdio -nographic -cdrom ../../binImg/rtr.iso -boot d
