exec \utils\developer\sasm\sasm-z80.code comx-firm.asm
exec \utils\developer\sasm\link4sasm.code comx-firm.obj /dob
renOW comx-firm.code \system\drivers\wancards\comx-firm.z80

copyOW \sources\i80386\driver-netcards\ethermain.obj ethermain.obj

exec \system\communication\etherComp.code comx-drv
renOW comx-drv.code \system\drivers\wancards\comx-drv.code

exec \system\communication\etherComp.code mixcom
renOW mixcom.code \system\drivers\wancards\mixcom.code

del ethermain.obj

compASM comx-init
chmode comx-init.code $44
renOW comx-init.code \system\drivers\wancards\comx-init.code

compASM comx-stop
chmode comx-stop.code $44
renOW comx-stop.code \system\drivers\wancards\comx-stop.code

compASM comx-term
chmode comx-term.code $44
renOW comx-term.code \system\drivers\wancards\comx-term.code
