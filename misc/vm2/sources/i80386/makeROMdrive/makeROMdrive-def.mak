exec makeRomDrive.code makeRomDrive-def.lst
compASM makeRomDrive-def.asm
chmode makeRomDrive-def.code 1
del makeRomDrive-def.asm
renOW makeRomDrive-def.code !romDrive.code
chmode \bootUp\bootImage.code 1
exec installkernel.code \bootUp\bootImage.code kernel.code !romDrive.code
del !romDrive.code
