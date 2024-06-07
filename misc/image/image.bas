download 3
reget-time 8

del-ifdn %dwn%
exec mkdir %dwn%
del-alw %tmp%
exec mkdir -m 0755 %tmp%
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
exec mkdir -m 0755 %tmp%/usr
exec mkdir -m 0755 %tmp%/usr/bin
exec mkdir -m 0755 %tmp%/usr/sbin
exec mkdir -m 0755 %tmp%/usr/lib
exec mkdir -m 0755 %tmp%/usr/lib32
exec mkdir -m 0755 %tmp%/usr/lib64
exec ln -s ./usr/bin %tmp%/bin
exec ln -s ./usr/lib %tmp%/lib
exec ln -s ./usr/sbin %tmp%/sbin
exec ln -s ./usr/lib32 %tmp%/lib32
exec ln -s ./usr/lib64 %tmp%/lib64
