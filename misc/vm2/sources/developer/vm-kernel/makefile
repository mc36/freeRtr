exec \utils\developer\spas\spas.code vm.pas i80386p
chmode vm.vma 11
renOW vm.vma \temp\vm.vma
exec \utils\developer\vm-compiler\bugos-i80386.code \temp\vm.vma vm.asm
exec \utils\developer\sasm\sasm-i80386.code vm.asm
del vm.asm
exec \utils\developer\sasm\link4sasm.code vm.obj
del vm.obj
exec \utils\developer\utils\appendcrc32.code vm.code
chmode vm.code $24
renOW vm.code \temp\vm.code
