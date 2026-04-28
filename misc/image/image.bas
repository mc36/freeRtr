download 3
reget-time 8

mkdir %dwn%
del %tmp%
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
link ./usr/bin %tmp%/bin
link ./usr/lib %tmp%/lib
link ./usr/sbin %tmp%/sbin
link ./usr/lib32 %tmp%/lib32
link ./usr/lib64 %tmp%/lib64
