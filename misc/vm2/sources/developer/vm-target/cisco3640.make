exec \utils\developer\vm-compiler\bugos-mips.code \temp\vm.vma \temp\cisco3640.asm raw-mips.table
copyAP cisco3600.inc \temp\cisco3640.asm
exec \utils\developer\sasm\sasm-mips.code \temp\cisco3640.asm
del \temp\cisco3640.asm
exec \utils\developer\sasm\link4sasm.code \temp\cisco3640.obj /dob /extcode
exec \utils\developer\utils\appendCrc32.code \temp\cisco3640.code
copyOW elf.header \temp\cisco3640.elf
copyAP \temp\cisco3640.code \temp\cisco3640.elf
del \temp\cisco3640.code
exec \utils\developer\utils\makeElf.code \temp\cisco3640.elf msb $80010000 $1e $20000
