#!/bin/sh
qemu-system-x86_64 -enable-kvm -m 1024 -netdev user,id=a1 -device virtio-net-pci,netdev=a1 -no-reboot -monitor none -serial stdio -nographic -cdrom ../../binImg/rtr.iso
