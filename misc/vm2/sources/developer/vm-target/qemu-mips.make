exec \utils\developer\vm-compiler\bugos-mips.code \temp\vm.vma \temp\qemu-mips.asm raw-mips.table
copyAP qemu-mips.inc \temp\qemu-mips.asm
exec \utils\developer\sasm\sasm-mips.code \temp\qemu-mips.asm
del \temp\qemu-mips.asm
exec \utils\developer\sasm\link4sasm.code \temp\qemu-mips.obj /dob /extcode
exec \utils\developer\utils\appendCrc32.code \temp\qemu-mips.code
copyOW elf.header \temp\qemu-mips.elf
copyAP \temp\qemu-mips.code \temp\qemu-mips.elf
del \temp\qemu-mips.code
exec \utils\developer\utils\makeElf.code \temp\qemu-mips.elf msb $80010000 $8 $20000
