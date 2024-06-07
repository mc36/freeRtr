include ../misc/image/image.bas

catalog-read sid xz %mirr% sid main
catalog-sum

select-one libc-bin                       #library
select-one libssl3t64                     #dataplane

select-lst
select-sum
package-down
package-inst

exec cp /usr/bin/qemu-%qemu%-static %tmp%/qemu-static
exec mkdir -m 0755 %tmp%/rtr
exec cd %tmp%/rtr/;tar xf ../../binImg/rtr-%unam%.tar
exec proot -r %tmp%/ -w / /qemu-static /rtr/p4bench.bin
