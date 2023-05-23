copyOW \system\drivers\disks\ramdrive.code \makeBootFloppyTemp1.code
exec \system\process\daemonRun.code \makeBootFloppyTemp1.code 1440
copyOW \system\filesystem\filesystem.code \makeBootFloppyTemp2.code
chmode \makeBootFloppyTemp2.code $44
exec \system\process\keycodesrun.code $79 \system\filesystem\makefs.code makeBootFloppyTemp1.code 0 140
del \makeBootFloppyTemp1.code
exec \system\process\keycodesrun.code $79 \system\filesystem\installBootSector.code makeBootFloppyTemp1.code 0
exec \system\process\daemonRun.code \makeBootFloppyTemp2.code makeBootFloppyTemp1.code 0 #
exec makeRomDrive.code makebootfloppy-def.lst
compASM makebootfloppy-def.asm
chmode makebootfloppy-def.code 1
del makebootfloppy-def.asm
del \makeBootFloppyTemp2.code
chmode #:\bootImage.code 1
exec \system\process\keycodesrun.code $79 installkernel.code #:\bootImage.code kernel.code makebootfloppy-def.code
del makebootfloppy-def.code
exec \utils\shell.code md #:\system
exec \utils\shell.code md #:\system\process
copyOW \system\process\processMonitor.code #:\system\process\processMonitor.code
copyOW \system\systemInfo.code #:\system\systemInfo.code
copyOW \system\systemTime.code #:\system\systemTime.code
exec \utils\shell.code md #:\utils
exec \utils\shell.code copy c:\utils\* #:\utils\
exec \utils\shell.code chmod #:\utils\*.code $07
del #:\utils\talker.code
del #:\utils\cdplayer.code
del #:\utils\filechecker.code
del #:\utils\tclsh.code
renOW #:\utils\bc-launcher-i80386.code #:\bc
exec \utils\shell.code md #:\system\drivers
exec \utils\shell.code rec-copy c:\system\drivers\* #:\system\drivers\
exec \utils\shell.code rec-chmod #:\system\*.code $44
exec \system\process\killprocess.code makeBootFloppyTemp2.code
noerr del \boot.img
exec \system\filesystem\disksaver.code makeBootFloppyTemp1.code 0 \boot.img
exec \system\process\killprocess.code makeBootFloppyTemp1.code
