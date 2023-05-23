exec \utils\developer\vm-compiler\bugos-powerpc.code \temp\vm.vma \temp\cisco2600.asm raw-powerpc.table
copyAP cisco2600.inc \temp\cisco2600.asm
exec \utils\developer\sasm\sasm-powerpc.code \temp\cisco2600.asm
del \temp\cisco2600.asm
exec \utils\developer\sasm\link4sasm.code \temp\cisco2600.obj /dob /extcode
exec \utils\developer\utils\appendCrc32.code \temp\cisco2600.code
copyOW elf.header \temp\cisco2600.elf
copyAP \temp\cisco2600.code \temp\cisco2600.elf
del \temp\cisco2600.code
exec \utils\developer\utils\makeElf.code \temp\cisco2600.elf msb $10000 $2b $20000
