exec makeRomDrive.code \bootup\bootOrder1.lst
compASM \bootup\bootOrder1.asm
chmode \bootup\bootOrder1.code 1
del \bootup\bootOrder1.asm
chmode \bootUp\bootImage.code 1
exec installkernel.code \bootUp\bootImage.code kernel.code \bootup\bootOrder1.code
del \bootup\bootOrder1.code
