compPAS fdisk
chmode fdisk.code $44
renOW fdisk.code \system\otherfs\fdisk.code

copyOW \sources\i80386\boot\osid.inc fdisk.asm
compASMn fdisk
del fdisk.asm
chmode fdisk.code $01
renOW fdisk.code \system\otherfs\fdisk.data
