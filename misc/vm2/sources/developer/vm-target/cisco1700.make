exec \utils\developer\vm-compiler\bugos-powerpc.code \temp\vm.vma \temp\cisco1700.asm raw-powerpc.table
copyAP cisco1700.inc \temp\cisco1700.asm
exec \utils\developer\sasm\sasm-powerpc.code \temp\cisco1700.asm
del \temp\cisco1700.asm
exec \utils\developer\sasm\link4sasm.code \temp\cisco1700.obj /dob /extcode
exec \utils\developer\utils\appendCrc32.code \temp\cisco1700.code
copyOW elf.header \temp\cisco1700.elf
copyAP \temp\cisco1700.code \temp\cisco1700.elf
del \temp\cisco1700.code
exec \utils\developer\utils\makeElf.code \temp\cisco1700.elf msb $10000 $33 $20000
