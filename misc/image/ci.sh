#!/bin/sh

ARCH=`uname -m`
IMG=`cd ../../binImg/;pwd`
MNT=../../binDsk/
mkdir $MNT
MNT=`cd $MNT;pwd`
qemu-img create $IMG/rtr-$ARCH.dsk 2G
/sbin/sfdisk -q $IMG/rtr-$ARCH.dsk << EOF
2048 6144 ef
8192 4169729 83 *
EOF
/sbin/mkfs.ext4 -q -U 999dff7c-0eed-48a5-b605-bb7d675a49ab -b 4096 -E offset=4194304 -F $IMG/rtr-$ARCH.dsk 475000
dd bs=1 conv=notrunc if=/usr/lib/syslinux/mbr/mbr.bin of=$IMG/rtr-$ARCH.dsk
dd bs=1K seek=1024 conv=notrunc if=$IMG/rtr-$ARCH.flp of=$IMG/rtr-$ARCH.dsk

cp $IMG/rtr-$ARCH.krn $MNT/rtr.krn
echo -n `cd $MNT/;cpio --quiet -H newc -i <$IMG/rtr-$ARCH.cpio`
echo -n `cd $MNT/;csplit -s init /^###rootfs###$/`
mv $MNT/xx00 $MNT/init
echo mount -t ext4 /dev/disk/by-uuid/999dff7c-0eed-48a5-b605-bb7d675a49ab /mnt >> $MNT/init
echo exec switch_root /mnt /init2 >> $MNT/init
echo "#!/bin/sh" > $MNT/init2
echo sh /init.sys >> $MNT/init2
cat $MNT/xx01 >> $MNT/init2
chmod 755 $MNT/init*
rm $MNT/xx0*
echo -n `cd $MNT/;find init*>filist`
echo -n `cd $MNT/;find dev/>>filist`
echo -n `cd $MNT/;find sys/>>filist`
echo -n `cd $MNT/;find proc/>>filist`
echo -n `cd $MNT/;find mnt/>>filist`
echo -n `cd $MNT/;find run/>>filist`
echo -n `cd $MNT/;find usr/>>filist`
echo -n `cd $MNT/;echo lib>>filist`
echo -n `cd $MNT/;echo lib32>>filist`
echo -n `cd $MNT/;echo lib64>>filist`
echo -n `cd $MNT/;echo bin>>filist`
echo -n `cd $MNT/;echo sbin>>filist`
echo -n `cd $MNT/;cpio --quiet -H newc -O cpio -o <filist`
rm $MNT/filist
zstd -9 $MNT/cpio
mv $MNT/cpio.zst $MNT/rtr.ird
rm $MNT/cpio
rm -rf $MNT/lib/modules
cp boot.cfg $MNT/syslinux.cfg
guestfish -a $IMG/rtr-$ARCH.dsk -m /dev/sda2 extlinux / : copy-in $MNT/* /
rm -rf $MNT

qemu-img convert -O qcow2 -c $IMG/rtr-$ARCH.dsk $IMG/rtr-$ARCH.qcow2
qemu-img convert -O vmdk -o subformat=streamOptimized $IMG/rtr-$ARCH.dsk $IMG/rtr-$ARCH.vmdk
SIZ=`stat --printf="%s" $IMG/rtr-$ARCH.qcow2`
VER=`java -jar ../../src/rtr.jar show version number | dos2unix | head -n 1`
SUM=`md5sum $IMG/rtr-$ARCH.qcow2 | awk '{ print $1 }'`
cat > $IMG/rtr-$ARCH.gns3a << EOF
{
    "name": "freeRouter",
    "category": "router",
    "description": "networking swiss army knife - it speaks routing protocols, and (re)encapsulates packets on interfaces",
    "vendor_name": "freeRouter",
    "vendor_url": "http://www.freertr.org/",
    "documentation_url": "http://www.freertr.org/",
    "product_name": "freeRouter",
    "product_url": "http://www.freertr.org/",
    "registry_version": 3,
    "status": "stable",
    "maintainer": "GNS3 Team",
    "maintainer_email": "developers@gns3.net",
    "usage": "there is no default password. a default configuration is present.",
    "port_name_format": "ethernet{port1}",
    "qemu": {
        "adapter_type": "virtio-net-pci",
        "adapters": 8,
        "ram": 1024,
        "hda_disk_interface": "ide",
        "arch": "x86_64",
        "console_type": "telnet",
        "kvm": "require"
    },
    "images": [
        {
            "filename": "rtr-$ARCH.qcow2",
            "version": "$VER",
            "md5sum": "$SUM",
            "filesize": $SIZ,
            "download_url": "http://www.freertr.org/",
            "direct_download_url": "http://dl.nop.hu/rtr-$ARCH.qcow2"
        }
    ],
    "versions": [
        {
            "name": "$VER",
            "images": {
                "hda_disk_image": "rtr-$ARCH.qcow2"
            }
        }
    ]
}
EOF

hashFile()
{
sum=`sha1sum $1 | awk '{ print $1 }'`
echo SHA1\($2\)= $sum >> $IMG/rtr-$ARCH.mf
}

echo -n "" > $IMG/rtr-$ARCH.mf
hashFile $IMG/rtr-$ARCH.qcow2 rtr-$ARCH.qcow2
hashFile $IMG/rtr-$ARCH.vmdk rtr-$ARCH.vmdk
hashFile package.ovf package.ovf
hashFile package.ver package.ver
hashFile package.vsh package.vsh
hashFile package.yaml package.yaml
tar cf $IMG/rtr-$ARCH.ova package.ovf package.ver package.vsh package.yaml
echo `cd $IMG/;tar rf rtr-$ARCH.ova rtr-$ARCH.qcow2`
echo `cd $IMG/;tar rf rtr-$ARCH.ova rtr-$ARCH.vmdk`
echo `cd $IMG/;tar rf rtr-$ARCH.ova rtr-$ARCH.mf`
ls -l $IMG/
