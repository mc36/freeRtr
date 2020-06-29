#!/bin/sh
parted -s ../../binImg/rtr.dsk mklabel msdos
parted -s ../../binImg/rtr.dsk mkpart primary ext4 1MB 2002MB
parted -s ../../binImg/rtr.dsk mkpart primary fat32 2042MB 2044MB
parted -s ../../binImg/rtr.dsk set 1 boot on
parted -s ../../binImg/rtr.dsk set 2 esp on
dd bs=1 conv=notrunc if=/usr/lib/syslinux/mbr/mbr.bin of=../../binImg/rtr.dsk
dd bs=1K conv=notrunc if=../../binImg/rtr.efi of=../../binImg/rtr.dsk seek=1993728
mkfs.ext4 -q -U 999dff7c-0eed-48a5-b605-bb7d675a49ab -b 1024 -E offset=1048576 -F ../../binImg/rtr.dsk 1900000
mount -o loop,offset=1048576 ../../binImg/rtr.dsk /mnt
mkdir /mnt/rtr
cp ../../binImg/rtr.krn /mnt/rtr.krn
gunzip -d -c -k ../../binImg/rtr.ird > /mnt/rtr.tmp
echo -n `cd /mnt/;cpio --quiet -H newc -i </mnt/rtr.tmp`
rm /mnt/rtr.tmp
echo -n `cd /mnt/;csplit -s init /^###rootfs###$/`
mv /mnt/xx00 /mnt/init
echo mount -t ext4 /dev/disk/by-uuid/999dff7c-0eed-48a5-b605-bb7d675a49ab /mnt >> /mnt/init
echo exec switch_root /mnt /init2 >> /mnt/init
echo "#!/bin/sh" > /mnt/init2
echo sh /init.sys >> /mnt/init2
cat /mnt/xx01 >> /mnt/init2
chmod +x /mnt/init*
rm /mnt/xx0*
mv /mnt/usr/lib/jvm /mnt/usr-lib-jvm
echo -n `cd /mnt/;find init*>filist`
echo -n `cd /mnt/;find dev/>>filist`
echo -n `cd /mnt/;find sys/>>filist`
echo -n `cd /mnt/;find proc/>>filist`
echo -n `cd /mnt/;find mnt/>>filist`
echo -n `cd /mnt/;find run/>>filist`
echo -n `cd /mnt/;find lib/>>filist`
echo -n `cd /mnt/;find lib64/>>filist`
echo -n `cd /mnt/;find bin/>>filist`
echo -n `cd /mnt/;find sbin/>>filist`
echo -n `cd /mnt/;find usr/>>filist`
mv /mnt/usr-lib-jvm /mnt/usr/lib/jvm
echo -n `cd /mnt/;cpio --quiet -H newc -O cpio -o <filist`
gzip /mnt/cpio
mv /mnt/cpio.gz /mnt/rtr.ird
cp ../image/boot.cfg /mnt/syslinux.cfg
dd bs=1M if=/dev/zero of=/mnt/zzz
rm /mnt/zzz
extlinux -i /mnt
umount /mnt
