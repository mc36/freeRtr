exec \utils\developer\vm-compiler\bugos-mips.code \temp\vm.vma \temp\cisco7200.asm raw-mips.table
copyAP cisco7200.inc \temp\cisco7200.asm
exec \utils\developer\sasm\sasm-mips.code \temp\cisco7200.asm
del \temp\cisco7200.asm
exec \utils\developer\sasm\link4sasm.code \temp\cisco7200.obj /dob /extcode
exec \utils\developer\utils\appendCrc32.code \temp\cisco7200.code
copyOW elf.header \temp\cisco7200.elf
copyAP \temp\cisco7200.code \temp\cisco7200.elf
del \temp\cisco7200.code
exec \utils\developer\utils\makeElf.code \temp\cisco7200.elf msb $80010000 $19 $20000
