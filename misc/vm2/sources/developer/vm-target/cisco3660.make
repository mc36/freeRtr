exec \utils\developer\vm-compiler\bugos-mips.code \temp\vm.vma \temp\cisco3660.asm raw-mips.table
copyAP cisco3600.inc \temp\cisco3660.asm
exec \utils\developer\sasm\sasm-mips.code \temp\cisco3660.asm
del \temp\cisco3660.asm
exec \utils\developer\sasm\link4sasm.code \temp\cisco3660.obj /dob /extcode
exec \utils\developer\utils\appendCrc32.code \temp\cisco3660.code
copyOW elf.header \temp\cisco3660.elf
copyAP \temp\cisco3660.code \temp\cisco3660.elf
del \temp\cisco3660.code
exec \utils\developer\utils\makeElf.code \temp\cisco3660.elf msb $80010000 $34 $20000
