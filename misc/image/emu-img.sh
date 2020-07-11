#!/bin/sh
qemu-system-x86_64 -enable-kvm -m 1024 -no-reboot -hda ../../binImg/rtr.iso
