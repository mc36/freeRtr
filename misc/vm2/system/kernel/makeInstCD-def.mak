exec \system\process\daemonRun.code \system\drivers\console\bignum.code
exec \system\process\daemonRun.code \system\drivers\console\crypto.code
exec \system\process\daemonRun.code \system\drivers\console\randomnumber.code
copyOW \system\drivers\disks\ramdrive.code \makeinstcdTemp1.code
exec \system\process\daemonRun.code \makeinstcdTemp1.code 1440
copyOW \system\filesystem\filesystem.code \makeinstcdTemp2.code
chmode \makeinstcdTemp2.code $44
exec \system\process\keycodesrun.code $79 \system\filesystem\makefs.code makeinstcdTemp1.code 0 256
del \makeinstcdTemp1.code
exec \system\process\keycodesrun.code $79 \system\filesystem\installBootSector.code makeinstcdTemp1.code 0
exec \system\process\daemonRun.code \makeinstcdTemp2.code makeinstcdTemp1.code 0 #
exec makeRomDrive.code makeinstcd-def.lst
compASM makeinstcd-def.asm
chmode makeinstcd-def.code 1
del makeinstcd-def.asm
del \makeinstcdTemp2.code
chmode #:\bootImage.code 1
exec \system\process\keycodesrun.code $79 installkernel.code #:\bootImage.code kernel.code makeinstcd-def.code
del makeinstcd-def.code
exec \system\process\killprocess.code makeinstcdTemp2.code
noerr del \instCD.img
exec \system\filesystem\disksaver.code makeinstcdTemp1.code 0 \instCD.img
exec \system\process\killprocess.code makeinstcdTemp1.code
noerr del \instCD.pck
exec \utils\packer.code a \instCD.pck \utils\shell.code
exec \utils\packer.code a \instCD.pck \utils\packer.code
exec \utils\packer.code a \instCD.pck \utils\bc-main.*
exec \utils\packer.code a \instCD.pck \bc
exec \utils\packer.code a \instCD.pck \system\drivers\disks\*
exec \utils\packer.code a \instCD.pck \system\filesystem\*
exec \utils\packer.code a \instCD.pck \system\otherfs\fdisk.*
exec \utils\packer.code a \instCD.pck \system\process\processmonitor.code
exec \utils\packer.code a \instCD.pck \system\installer\*
noerr del \instCD.iso
exec \system\otherfs\isocreator.code makeinstcd-def.txt \instCD.iso
exec \system\installer\autoUpdater.code generate \instCD.iso \!users\autoUpdater.privateKey
