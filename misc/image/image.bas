download 3
reget-time 8

del-ifdn %dwn%
exec mkdir %dwn%
del-alw %tmp%
exec mkdir -m 0755 %tmp%
exec mkdir -m 0755 %tmp%/rtr
exec mkdir -m 0755 %tmp%/sys
exec mkdir -m 0755 %tmp%/proc
exec mkdir -m 0755 %tmp%/tmp
exec mkdir -m 0755 %tmp%/mnt
exec mkdir -m 0755 %tmp%/var
exec mkdir -m 0755 %tmp%/var/lock
exec mkdir -m 0755 %tmp%/var/run
exec mkdir -m 0755 %tmp%/dev
exec mkdir -m 0755 %tmp%/run
exec mkdir -m 0755 %tmp%/etc
exec mkdir -m 0755 %tmp%/lib
exec mkdir -m 0755 %tmp%/lib32
exec mkdir -m 0755 %tmp%/lib64
exec mkdir -m 0755 %tmp%/bin
exec mkdir -m 0755 %tmp%/sbin
exec mkdir -m 0755 %tmp%/usr
exec mkdir -m 0755 %tmp%/usr/bin
exec mkdir -m 0755 %tmp%/usr/sbin

exec cp rtr.ver %tmp%/rtr/
exec cp ../binTmp/*.bin %tmp%/rtr/

catalog-read sid xz %mirr% sid main
catalog-read exp xz %mirr% experimental main
catalog-sum

select-dis debconf.*
select-dis adduser.*
select-dis passwd.*
select-dis util-linux.*
select-dis dpkg.*
select-dis perl.*
select-dis gcc.*
select-dis init-system-helper.*
select-dis initramfs-tool.*
select-dis linux-initramfs-tool.*
select-dis linux-headers.*
select-dis multiarch-support.*
select-dis ucf.*
select-dis fontconfig-config.*
select-dis dmsetup.*
select-dis ca-certificate.*
select-dis mount.*
select-dis x11.*
select-dis lsb-base.*
select-dis linux-base.*
select-dis linux-initramfs.*
select-dis openssl.*
select-dis systemd.*
select-dis libasound.*
select-dis libavahi.*
select-dis libcups.*
select-dis libharfbuzz.*
select-dis liblcms.*
select-dis libfontconfig.*
select-dis libjpeg.*
select-dis libnss.*
select-dis libfreetype.*
select-dis libpcsc.*
select-dis libx11.*
select-dis libxext.*
select-dis libxi.*
select-dis libxrender.*
select-dis libxtst.*

select-one busybox                        #small utils
select-one socat                          #small utils
select-one udev                           #bring up hw
select-one ethtool                        #disable hw chksum
select-one libpcap0.8                     #dataplane requirement
select-one libssl3                        #dataplane requirement
