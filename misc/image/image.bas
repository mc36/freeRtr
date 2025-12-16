download 3
reget-time 8

del-ifdn %dwn%
mkdir %dwn%
del-alw %tmp%
mkdir %tmp%
mkdir %tmp%/sys
mkdir %tmp%/proc
mkdir %tmp%/tmp
mkdir %tmp%/mnt
mkdir %tmp%/var
mkdir %tmp%/var/lock
mkdir %tmp%/var/run
mkdir %tmp%/dev
mkdir %tmp%/run
mkdir %tmp%/etc
mkdir %tmp%/usr
mkdir %tmp%/usr/bin
mkdir %tmp%/usr/sbin
mkdir %tmp%/usr/lib
mkdir %tmp%/usr/lib32
mkdir %tmp%/usr/lib64
exec ln -s ./usr/bin %tmp%/bin
exec ln -s ./usr/lib %tmp%/lib
exec ln -s ./usr/sbin %tmp%/sbin
exec ln -s ./usr/lib32 %tmp%/lib32
exec ln -s ./usr/lib64 %tmp%/lib64
