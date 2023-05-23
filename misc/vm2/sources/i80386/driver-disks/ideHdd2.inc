;compact disk part...

;------------------------------- read data sector...
proc cDisk_readDataSec
;in:  esi-offset of drive table...
;     eax-sector number to read...
;out: carry-cleared if successful...
call dword cDisk_packerClear
mov byte def:[edi],28h
mov byte def:[edi+8],1
xchg al,ah
rol eax,16
xchg al,ah
mov def:[edi+2],eax
mov ecx,800h
call dword cDisk_packetCmd
retnd
endp
;-------------------------------

;------------------------------- read one toc entry...
proc cDisk_readTocEntry
;in:  esi-offset of drive table...
;     al-track number, 0 if just header needed...
;out: carry-cleared if successful...
;     bl-first track number...
;     bh-last track number...
;     al-track number...
;     ah-track type: 10h..13h=audio, 14h..17h=data...
;     ecx-track lba beginning...
;     edx-track lba ending...
call dword cDisk_packerClear
mov byte def:[edi],43h
mov def:[edi+6],al
mov byte def:[edi+1],2
mov byte def:[edi+8],20
mov ecx,20
call dword cDisk_packetCmd
jc byte cDisk_readTocEntry_j1
mov bl,def:[edi+9]
mov cl,def:[edi+10]
mov dl,def:[edi+11]
call dword cDisk_msf2lba
push eax
mov bl,def:[edi+17]
mov cl,def:[edi+18]
mov dl,def:[edi+19]
call dword cDisk_msf2lba
mov edx,eax
pop ecx
mov bl,def:[edi+2]
mov bh,def:[edi+3]
mov al,def:[edi+6]
mov ah,def:[edi+5]
clc
cDisk_readTocEntry_j1:
retnd
endp
;-------------------------------

;------------------------------- check disk readyness...
proc cDisk_unitReady
;in:  esi-offset of drive table...
;out: carry-cleared if successful...
;     al-unit ready: 0=no, 1=yes...
call dword cDisk_packerClear
sub ecx,ecx
call dword cDisk_packetCmd
jc byte cDisk_unitReady_j1
or ah,ah
setz al
clc
retnd
cDisk_unitReady_j1:
mov al,0
stc
retnd
endp
;-------------------------------

;------------------------------- read subchannel info...
proc cDisk_subChannel
;in:  esi-offset of drive table...
;out: carry-cleared if successful...
;     eax-track number (-1 if nothing)...
;     ecx-lba offset in track...
call dword cDisk_packerClear
mov byte def:[edi],42h
mov byte def:[edi+2],40h
mov byte def:[edi+3],1
mov byte def:[edi+8],16
mov ecx,16
call dword cDisk_packetCmd
jc byte cDisk_subChannel_j2
mov ecx,def:[edi+12]
xchg cl,ch
rol ecx,16
xchg cl,ch
mov eax,-1
mov dl,def:[edi+1]
cmp dl,11h
jne byte cDisk_subChannel_j1
movzx byte eax,def:[edi+6]
cDisk_subChannel_j1:
clc
cDisk_subChannel_j2:
retnd
endp
;-------------------------------

;------------------------------- open/close tray...
proc cDisk_openClose
;in:  esi-offset of drive table...
;     al-status: 0=close, 1=open...
;out: carry-cleared if successful...
call dword cDisk_packerClear
mov byte def:[edi],1bh
mov byte def:[edi+1],1
not al
and al,1
or al,2
mov def:[edi+4],al
sub ecx,ecx
call dword cDisk_packetCmd
retnd
endp
;-------------------------------

;------------------------------- stop playing...
proc cDisk_stopPlay
;in:  esi-offset of drive table...
;out: carry-cleared if successful...
call dword cDisk_packerClear
mov byte def:[edi],1bh
mov byte def:[edi+1],1
mov byte def:[edi+4],0
sub ecx,ecx
call dword cDisk_packetCmd
retnd
endp
;-------------------------------

;------------------------------- pause playing...
proc cDisk_pausePlay
;in:  esi-offset of drive table...
;     al-status: 0=resume, 1=pause...
;out: carry-cleared if successful...
call dword cDisk_packerClear
mov byte def:[edi],4bh
not al
and al,1
mov def:[edi+8],al
sub ecx,ecx
call dword cDisk_packetCmd
retnd
endp
;-------------------------------

;------------------------------- start playing...
proc cDisk_startPlay
;in:  esi-offset of drive table...
;     eax-starting sector number...
;     ecx-ending sector number...
;out: carry-cleared if successful...
call dword cDisk_packerClear
mov byte def:[edi],47h
push ecx
call dword lba2msf
mov def:[edi+3],bl
mov def:[edi+4],cl
mov def:[edi+5],dl
pop eax
call dword lba2msf
mov def:[edi+6],bl
mov def:[edi+7],cl
mov def:[edi+8],dl
sub ecx,ecx
call dword cDisk_packetCmd
retnd
endp
;-------------------------------

;------------------------------- open/close dirve....
proc cDisk_doorLock
;in:  esi-offset of drive table...
;     al-status: 0=free, 1=lock...
;out: carry-cleared if successful...
call dword cDisk_packerClear
mov byte def:[edi],1eh
and al,1
mov def:[edi+4],al
sub ecx,ecx
call dword cDisk_packetCmd
retnd
endp
;-------------------------------




;-------------------------------
proc cDisk_packerClear
push eax
push ecx
mov edi,dataSeg_freMem
add edi,8
push edi
mov ecx,16
sub eax,eax
rep
  stosd ptr32
pop edi
pop ecx
pop eax
retnd
endp
;-------------------------------

;------------------------------- do one packet command...
proc cDisk_packetCmd
;in:  esi-offset of drive table...
;     edi-offset of data (12 bytes command, x bytes result)...
;     ecx-bytes to receive...
;out: carry-cleared if successful...
;     ah-error register...
lea eax,def:[ecx+1]
shr eax,1
mov def:[dataSeg_temp01],eax
mov edx,def:[esi+driveData_port]
add edx,6
mov al,def:[esi+driveData_driv]
out dx,al
mov edx,def:[esi+driveData_port]
add edx,7
mov ecx,8800h                   ;bsy, drq...
call dword drive_waitForIt
;jc dword cDisk_packetCmd_err
mov edx,def:[esi+driveData_port]
inc edx
mov al,0
out dx,al
inc edx
out dx,al
inc edx
out dx,al
inc edx
mov al,cl
out dx,al
inc edx
mov al,ch
out dx,al
mov edx,def:[esi+driveData_port]
add edx,7
mov al,0a0h                     ;atapi command...
out dx,al
mov edx,def:[esi+driveData_port]
add edx,2
mov ecx,0301h                   ;COD, io...
call dword drive_waitForIt
jc dword cDisk_packetCmd_err
mov edx,def:[esi+driveData_port]
push esi
mov esi,edi
mov ecx,6
cDisk_packetCmd_j1:
push ecx
add edx,7
mov ecx,4848h                   ;DRDY,DRQ...
call dword drive_waitForIt
sub edx,7
lodsw ptr32
out dx,ax
pop ecx
loopd cDisk_packetCmd_j1
pop esi
mov edx,def:[esi+driveData_port]
add edx,7
mov ecx,8000h                   ;bsy...
call dword drive_waitForIt
jc dword cDisk_packetCmd_err
mov eax,def:[dataSeg_temp01]
or eax,eax
jz byte cDisk_packetCmd_j4
mov edx,def:[esi+driveData_port]
add edx,2
mov ecx,0302h                   ;cod, IO...
call dword drive_waitForIt
jc dword cDisk_packetCmd_err
push edi
cDisk_packetCmd_j2:
dec dword def:[dataSeg_temp01]
js byte cDisk_packetCmd_j3
mov edx,def:[esi+driveData_port]
add edx,7
mov ecx,4848h                   ;DRDY,DRQ...
call dword drive_waitForIt
sub edx,7
in ax,dx
stosw ptr32
jmp byte cDisk_packetCmd_j2
cDisk_packetCmd_j3:
pop edi
cDisk_packetCmd_j4:
mov edx,def:[esi+driveData_port]
add edx,2
mov ecx,0303h                   ;COD, IO...
call dword drive_waitForIt
jc dword cDisk_packetCmd_err
mov edx,def:[esi+driveData_port]
add edx,7
in al,dx
mov ah,0
test al,1
jz byte cDisk_packetCmd_ok
mov edx,def:[esi+driveData_port]
add edx,1
in al,dx
mov ah,al
cDisk_packetCmd_ok:
clc
cDisk_packetCmd_vege:
retnd
cDisk_packetCmd_err:
sub ecx,ecx
stc
jmp byte cDisk_packetCmd_vege
endp
;-------------------------------




;------------------------------- identify drive...
proc cDisk_identify
;in:  esi-offset of drive table...
;out: carry-cleared if successful...
sub eax,eax
mov def:[esi+driveData_type],eax
mov edx,def:[esi+driveData_port]
add edx,6
mov al,def:[esi+driveData_driv]
out dx,al
sub eax,eax
mov edx,def:[esi+driveData_port]
add edx,4
out dx,al
inc edx
out dx,al
mov edx,def:[esi+driveData_port]
add edx,7
mov ecx,4040h                   ;DRDY...
call dword drive_waitForIt
jc dword cDisk_identify_err
mov edx,def:[esi+driveData_port]
add edx,7
mov al,0a1h                     ;atapi identify...
out dx,al
mov ecx,1d18h                   ;SERV,DRQ,corr,check...
call dword drive_waitForIt
jc dword cDisk_identify_err
mov ecx,256
mov edi,dataSeg_freMem
cDisk_identify_j1:
mov edx,def:[esi+driveData_port]
in ax,dx
stosw ptr32
loopd cDisk_identify_j1
mov byte def:[esi+driveData_type],2
mov ebp,esi
mov esi,36h
mov edi,driveData_modl
mov cl,20
call dword drive_identify_j2
mov esi,14h
mov edi,driveData_serl
mov cl,10
call dword drive_identify_j2
mov esi,2eh
mov edi,driveData_firm
mov cl,4
call dword drive_identify_j2
mov esi,ebp
clc
cDisk_identify_vege:
retnd
cDisk_identify_err:
stc
jmp byte cDisk_identify_vege
endp
;-------------------------------




;------------------------------- convert msf to lba...
proc cDisk_msf2lba
;in:  bl-minute...
;     cl-second...
;     dl-frame...
;out: eax-lba address...
push esi
push edx
movzx eax,bl
mov esi,60
mul esi
movzx esi,cl
add eax,esi
mov esi,75
mul esi
pop edx
movzx esi,dl
add eax,esi
pop esi
sub eax,150
retnd
endp
;-------------------------------

;------------------------------- convert lba to msf...
proc lba2msf
;in:  eax-lba address...
;out: bl-minute...
;     cl-second...
;     dl-frame...
push eax
push ecx
push edx
add eax,150
mov ecx,4500
sub edx,edx
div ecx
mov bl,al
mov eax,edx
mov ecx,75
sub edx,edx
div ecx
mov ah,dl
pop edx
pop ecx
mov cl,al
mov dl,ah
pop eax
retnd
endp
;-------------------------------
