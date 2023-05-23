exec \utils\developer\vm-compiler\bugos-powerpc.code \temp\vm.vma \temp\qemu-powerpc.asm raw-powerpc.table
copyAP qemu-powerpc.inc \temp\qemu-powerpc.asm
exec \utils\developer\sasm\sasm-powerpc.code \temp\qemu-powerpc.asm
del \temp\qemu-powerpc.asm
exec \utils\developer\sasm\link4sasm.code \temp\qemu-powerpc.obj /dob /extcode
exec \utils\developer\utils\appendCrc32.code \temp\qemu-powerpc.code
copyOW elf.header \temp\qemu-powerpc.elf
copyAP \temp\qemu-powerpc.code \temp\qemu-powerpc.elf
del \temp\qemu-powerpc.code
exec \utils\developer\utils\makeElf.code \temp\qemu-powerpc.elf msb $80010000 $14 $20000
