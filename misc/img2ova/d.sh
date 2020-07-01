#!/bin/sh
IMG=`cd ../../binImg/;pwd`
MNT=/mnt
parted -s $IMG/rtr.dsk mklabel msdos
parted -s $IMG/rtr.dsk mkpart primary ext4 1MB 2002MB
parted -s $IMG/rtr.dsk mkpart primary fat32 2042MB 2044MB
parted -s $IMG/rtr.dsk set 1 boot on
parted -s $IMG/rtr.dsk set 2 esp on
dd bs=1 conv=notrunc if=/usr/lib/syslinux/mbr/mbr.bin of=$IMG/rtr.dsk
dd bs=1K conv=notrunc if=$IMG/rtr.efi of=$IMG/rtr.dsk seek=1993728
mkfs.ext4 -q -U 999dff7c-0eed-48a5-b605-bb7d675a49ab -b 1024 -E offset=1048576 -F $IMG/rtr.dsk 1900000
mount -o loop,offset=1048576 $IMG/rtr.dsk $MNT
mkdir $MNT/rtr
cp $IMG/rtr.krn $MNT/rtr.krn
echo -n `cd $MNT/;cpio --quiet -H newc -i <$IMG/rtr.cpio`
echo -n `cd $MNT/;csplit -s init /^###rootfs###$/`
mv $MNT/xx00 $MNT/init
echo mount -t ext4 /dev/disk/by-uuid/999dff7c-0eed-48a5-b605-bb7d675a49ab $MNT >> $MNT/init
echo exec switch_root $MNT /init2 >> $MNT/init
echo "#!/bin/sh" > $MNT/init2
echo sh /init.sys >> $MNT/init2
cat $MNT/xx01 >> $MNT/init2
chmod +x $MNT/init*
rm $MNT/xx0*
mv $MNT/usr/lib/jvm $MNT/usr-lib-jvm
echo -n `cd $MNT/;find init*>filist`
echo -n `cd $MNT/;find dev/>>filist`
echo -n `cd $MNT/;find sys/>>filist`
echo -n `cd $MNT/;find proc/>>filist`
echo -n `cd $MNT/;find mnt/>>filist`
echo -n `cd $MNT/;find run/>>filist`
echo -n `cd $MNT/;find lib/>>filist`
echo -n `cd $MNT/;find lib64/>>filist`
echo -n `cd $MNT/;find bin/>>filist`
echo -n `cd $MNT/;find sbin/>>filist`
echo -n `cd $MNT/;find usr/>>filist`
mv $MNT/usr-lib-jvm $MNT/usr/lib/jvm
echo -n `cd $MNT/;cpio --quiet -H newc -O cpio -o <filist`
gzip $MNT/cpio
mv $MNT/cpio.gz $MNT/rtr.ird
cp ../image/boot.cfg $MNT/syslinux.cfg
dd bs=1M if=/dev/zero of=$MNT/zzz
rm $MNT/zzz
extlinux -i $MNT
umount $MNT
