#!/bin/sh
IMG=`cd ../../binImg/;pwd`
qemu-img create $IMG/rtr.dsk 2G
sudo ./d.sh
qemu-img convert -O qcow2 -c $IMG/rtr.dsk $IMG/rtr.qcow2
qemu-img convert -O vmdk -o subformat=streamOptimized $IMG/rtr.dsk $IMG/rtr.vmdk

hashFile()
{
sum=`sha1sum $1 | awk '{ print $1 }'`
echo SHA1\($2\)= $sum >> $IMG/rtr.mf
}

echo -n "" > $IMG/rtr.mf
hashFile $IMG/rtr.qcow2 rtr.qcow2
hashFile $IMG/rtr.vmdk rtr.vmdk
hashFile package.ovf package.ovf
hashFile package.ver package.ver
hashFile package.vsh package.vsh
hashFile package.yaml package.yaml
tar cf $IMG/rtr.ova package.ovf package.ver package.vsh package.yaml
echo `cd $IMG/;tar rf rtr.ova rtr.qcow2`
echo `cd $IMG/;tar rf rtr.ova rtr.vmdk`
echo `cd $IMG/;tar rf rtr.ova rtr.mf`
ls -l $IMG/
