include ../misc/image/image.bas

catalog-read exp xz %mirr% experimental main
catalog-read sid xz %mirr% sid main
catalog-sum

select-one libc6                          #library
select-one libssl3t64                     #dataplane
select-one openssl-provider-legacy        #dataplane

select-lst
select-sum
package-down
package-inst

mkdir %tmp%/rtr
exec cd %tmp%/rtr/;tar xfz ../../binImg/rtr-%unam%.tgz
exec /usr/bin/qemu-%qemu% -L %tmp%/ %tmp%/rtr/p4bench.bin
