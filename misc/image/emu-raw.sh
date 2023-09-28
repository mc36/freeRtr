#!/bin/sh
qemu-system-x86_64 -enable-kvm -cpu host -smp 2 -k en-us -m 1024 -netdev user,id=a1 -device virtio-net-pci,netdev=a1 -no-reboot -kernel ../../binImg/rtr-amd64.krn -initrd ../../binImg/rtr-amd64.ird
