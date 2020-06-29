#!/bin/sh
qemu-img create ../../binImg/rtr.dsk 2G
sudo ./c2.sh
qemu-img convert -O qcow2 -c ../../binImg/rtr.dsk ../../binImg/rtr.qcow2
qemu-img convert -O vmdk -o subformat=streamOptimized ../../binImg/rtr.dsk ../../binImg/rtr.vmdk

hashFile()
{
sum=`sha1sum $1 | awk '{ print $1 }'`
echo SHA1\($2\)= $sum >> ../../binImg/rtr.mf
}

echo -n "" > ../../binImg/rtr.mf
hashFile ../../binImg/rtr.qcow2 rtr.qcow2
hashFile ../../binImg/rtr.vmdk rtr.vmdk
hashFile package.ovf package.ovf
hashFile package.ver package.ver
hashFile package.vsh package.vsh
hashFile package.yaml package.yaml
tar cf ../../binImg/rtr.ova package.ovf package.ver package.vsh package.yaml
echo `cd ../../binImg/;tar rf rtr.ova rtr.qcow2`
echo `cd ../../binImg/;tar rf rtr.ova rtr.vmdk`
echo `cd ../../binImg/;tar rf rtr.ova rtr.mf`
ls -l ../../binImg/
