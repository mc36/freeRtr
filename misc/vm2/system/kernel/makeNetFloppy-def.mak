copyOW \system\drivers\disks\ramdrive.code \makeNetFloppyTemp1.code
exec \system\process\daemonRun.code \makeNetFloppyTemp1.code 1440
copyOW \system\filesystem\filesystem.code \makeNetFloppyTemp2.code
chmode \makeNetFloppyTemp2.code $44
exec \system\process\keycodesrun.code $79 \system\filesystem\makefs.code makeNetFloppyTemp1.code 0 550
del \makeNetFloppyTemp1.code
exec \system\process\keycodesrun.code $79 \system\filesystem\installBootSector.code makeNetFloppyTemp1.code 0
exec \system\process\daemonRun.code \makeNetFloppyTemp2.code makeNetFloppyTemp1.code 0 #
noerr del \makeNetFloppySysDrv.pck
noerr del \makeNetFloppySysDrv.pc
exec \utils\packer.code r \makeNetFloppySysDrv.pck \system\drivers\
exec \utils\compressor.code c6 \makeNetFloppySysDrv.pck \makeNetFloppySysDrv.pc
del \makeNetFloppySysDrv.pck
noerr del \makeNetFloppyRamImg.pck
noerr del \makeNetFloppyRamImg.pc
exec \utils\packer.code a \makeNetFloppyRamImg.pck \internet\kernel\eth4.code \
exec \utils\packer.code a \makeNetFloppyRamImg.pck \internet\kernel\ip4.code \
exec \utils\packer.code a \makeNetFloppyRamImg.pck \internet\kernel\ip4iface.code \
exec \utils\packer.code a \makeNetFloppyRamImg.pck \internet\kernel\ip4conf.code \
exec \utils\packer.code a \makeNetFloppyRamImg.pck \internet\kernel\tcp.code \
exec \utils\packer.code a \makeNetFloppyRamImg.pck \internet\kernel\dns.code \
exec \utils\packer.code a \makeNetFloppyRamImg.pck \internet\client\http.code \
exec \utils\packer.code a \makeNetFloppyRamImg.pck \system\process\capturerun.code \
exec \utils\packer.code a \makeNetFloppyRamImg.pck \system\process\keycodesrun.code \
exec \utils\packer.code a \makeNetFloppyRamImg.pck \system\installer\networkinstall.code \
exec \utils\packer.code a \makeNetFloppyRamImg.pck \system\installer\networkconfig.code \
exec \utils\packer.code a \makeNetFloppyRamImg.pck \system\installer\networkconfig.list \
exec \utils\packer.code a \makeNetFloppyRamImg.pck \utils\shell.code \
exec \utils\packer.code a \makeNetFloppyRamImg.pck \system\drivers\disks\ramdrive.code \
exec \utils\packer.code a \makeNetFloppyRamImg.pck \system\filesystem\filesystem.code \
exec \utils\packer.code a \makeNetFloppyRamImg.pck \system\filesystem\makefs.code \
exec \utils\packer.code a \makeNetFloppyRamImg.pck \system\otherfs\iso9660.code \
exec \utils\packer.code a \makeNetFloppyRamImg.pck \system\otherfs\cdemu.code \
exec \utils\compressor.code c6 \makeNetFloppyRamImg.pck \makeNetFloppyRamImg.pc
del \makeNetFloppyRamImg.pck
noerr del \makeNetFloppyPacker.pc
exec \utils\compressor.code c6 \utils\packer.code \makeNetFloppyPacker.pc
exec makeRomDrive.code makenetfloppy-def.lst
compASM makenetfloppy-def.asm
chmode makenetfloppy-def.code 1
del \makeNetFloppySysDrv.pc
del \makeNetFloppyRamImg.pc
del \makeNetFloppyPacker.pc
del makenetfloppy-def.asm
del \makeNetFloppyTemp2.code
chmode #:\bootImage.code 1
exec \system\process\keycodesrun.code $79 installkernel.code #:\bootImage.code kernel.code makenetfloppy-def.code
del makenetfloppy-def.code
exec \system\process\killprocess.code makeNetFloppyTemp2.code
noerr del \net.img
exec \system\filesystem\disksaver.code makeNetFloppyTemp1.code 0 \net.img
exec \system\process\killprocess.code makeNetFloppyTemp1.code
