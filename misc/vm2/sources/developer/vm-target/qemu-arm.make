exec \utils\developer\vm-compiler\bugos-arm.code \temp\vm.vma \temp\qemu-arm.asm raw-arm.table
copyAP qemu-arm.inc \temp\qemu-arm.asm
exec \utils\developer\sasm\sasm-arm.code \temp\qemu-arm.asm
del \temp\qemu-arm.asm
exec \utils\developer\sasm\link4sasm.code \temp\qemu-arm.obj /dob /extcode
exec \utils\developer\utils\appendCrc32.code \temp\qemu-arm.code
copyOW elf.header \temp\qemu-arm.elf
copyAP \temp\qemu-arm.code \temp\qemu-arm.elf
del \temp\qemu-arm.code
exec \utils\developer\utils\makeElf.code \temp\qemu-arm.elf lsb $10000 $28 $20000
