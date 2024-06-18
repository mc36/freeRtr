#!/bin/sh

ARCH=$1
IMG=`cd ../../binImg/;pwd`
MNT=`cd ../../binMnt/;pwd`

sfdisk -q $IMG/rtr$ARCH.dsk << EOF
2048 6144 ef
8192 4169729 83 *
EOF

dd bs=1 conv=notrunc if=/usr/lib/syslinux/mbr/mbr.bin of=$IMG/rtr$ARCH.dsk
dd bs=1K conv=notrunc if=$IMG/rtr$ARCH.flp of=$IMG/rtr$ARCH.dsk seek=1024
mkfs.ext4 -q -U 999dff7c-0eed-48a5-b605-bb7d675a49ab -b 4096 -E offset=4194304 -F $IMG/rtr$ARCH.dsk 475000
mount -o loop,offset=4194304 $IMG/rtr$ARCH.dsk $MNT
cp $IMG/rtr$ARCH.krn $MNT/rtr.krn
cp $IMG/rtr$ARCH.grb $MNT/rtr.grb
cp $IMG/rtr$ARCH.flp $MNT/rtr.flp

echo -n `cd $MNT/;cpio --quiet -H newc -i <$IMG/rtr$ARCH.cpio`
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
zstd -9 $MNT/cpio
mv $MNT/cpio.zst $MNT/rtr.ird
rm $MNT/cpio
rm -rf $MNT/lib/modules

cp ../image/boot.cfg $MNT/syslinux.cfg
echo -n `cd $MNT/;tar cf $IMG/rtr$ARCH.update *`
tar -vf $IMG/rtr$ARCH.update --delete rtr/
extlinux -i $MNT
sync
fstrim -v $MNT
umount $MNT
echo done
