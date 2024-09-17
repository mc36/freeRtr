download 3
reget-time 8

del-ifdn %dwn%
exec mkdir %dwn%
del-alw %tmp%
exec mkdir %tmp% %tmp%/sys %tmp%/proc %tmp%/tmp %tmp%/mnt %tmp%/var %tmp%/var/lock %tmp%/var/run %tmp%/dev %tmp%/run
exec mkdir %tmp%/etc %tmp%/usr %tmp%/usr/bin %tmp%/usr/sbin %tmp%/usr/lib %tmp%/usr/lib32 %tmp%/usr/lib64
exec ln -s ./usr/bin %tmp%/bin
exec ln -s ./usr/lib %tmp%/lib
exec ln -s ./usr/sbin %tmp%/sbin
exec ln -s ./usr/lib32 %tmp%/lib32
exec ln -s ./usr/lib64 %tmp%/lib64
