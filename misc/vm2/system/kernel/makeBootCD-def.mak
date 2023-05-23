copyOW \system\drivers\disks\ramdrive.code \makebootcdTemp1.code
exec \system\process\daemonRun.code \makebootcdTemp1.code 1440
copyOW \system\filesystem\filesystem.code \makebootcdTemp2.code
chmode \makebootcdTemp2.code $44
exec \system\process\keycodesrun.code $79 \system\filesystem\makefs.code makebootcdTemp1.code 0 256
del \makebootcdTemp1.code
exec \system\process\keycodesrun.code $79 \system\filesystem\installBootSector.code makebootcdTemp1.code 0
exec \system\process\daemonRun.code \makebootcdTemp2.code makebootcdTemp1.code 0 #
exec makeRomDrive.code makebootcd-def.lst
compASM makebootcd-def.asm
chmode makebootcd-def.code 1
del makebootcd-def.asm
del \makebootcdTemp2.code
chmode #:\bootImage.code 1
exec \system\process\keycodesrun.code $79 installkernel.code #:\bootImage.code kernel.code makebootcd-def.code
del makebootcd-def.code
exec \system\process\killprocess.code makebootcdTemp2.code
noerr del \bootCD.img
exec \system\filesystem\disksaver.code makebootcdTemp1.code 0 \bootCD.img
exec \system\process\killprocess.code makebootcdTemp1.code
noerr del \bootCD.pck
exec \utils\packer.code a \bootCD.pck \utils\shell.code
exec \utils\packer.code a \bootCD.pck \utils\packer.code
exec \utils\packer.code a \bootCD.pck \utils\bc-main.*
exec \utils\packer.code a \bootCD.pck \bc
exec \utils\packer.code a \bootCD.pck \system\drivers\disks\partition.code
exec \utils\packer.code a \bootCD.pck \system\filesystem\*
exec \utils\packer.code a \bootCD.pck \system\otherfs\fdisk.*
exec \utils\packer.code a \bootCD.pck \system\process\processmonitor.code
noerr del \bootCD.iso
exec \system\otherfs\isocreator.code makebootcd-def.txt \bootCD.iso
