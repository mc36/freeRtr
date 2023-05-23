org 0h
use32
db 'exec'                       ;id
dd offset lastbyte              ;size
dd 8000h                        ;data
dd 4096                         ;stack
;-------------------------------

;-------------------------------
mov esi,offset text01
call dword writeCodeStr

mov al,1
clts                            ;switch io accessible mode...
dd 04h

mov eax,0d0000h
mov ecx,8000h
clts                            ;map system memory...
dd 03h
or ebx,ebx
jnz dword vege
mov def:[DataBlock_phyMap],eax
mov def:[DataBlock_logMap],edi

sub edi,edi
clts                            ;get process parameters...
dd 13h
mov al,def:[edi]
or al,al
setz al
movzx eax,al
mov def:[DataBlock_readOn],eax
mov esi,offset text26
call dword writeCodeStr
mov eax,def:[DataBlock_readOn]
mov esi,cs:[text28+eax*4]
call dword writeCodeStr
mov esi,offset text27
call dword writeCodeStr


mov esi,offset text02
call dword writeCodeStr
call dword i82365_detect
mov def:[DataBlock_crdNum],ebp
mov edx,ebp
call dword conv2dec
call dword writeDataStr
mov esi,offset text03
call dword writeCodeStr

mov eax,def:[DataBlock_crdNum]
or eax,eax
jz dword vege
sub edx,edx
main_j1:
mov ebp,DataBlock_sckDat
mov ds:[ebp],dl
push edx
call dword pcmcia_enableSck
call dword pcmcia_applyCfg
pop edx
inc edx
cmp edx,def:[DataBlock_crdNum]
jb byte main_j1

sub edx,edx
main_j2:
mov ebp,DataBlock_sckDat
mov ds:[ebp],dl
push edx
call dword i82365_getStat
call dword pcmcia_displaySck
pop edx
inc edx
cmp edx,def:[DataBlock_crdNum]
jb byte main_j2

vege:
sub eax,eax                     ;terminate process...
clts
dd 00h
;-------------------------------




;-------------------------------
proc conv2dec
;in:  edx-value to write...
;out: esi-where converted...
mov esi,offset conv2dec_d1
mov edi,DataBlock_wrtBuf
conv2dec_j3:
cmp esi,offset conv2dec_d2
jae byte conv2dec_j4
lodsd cs,ptr32
or eax,eax
jz byte conv2dec_j3
cmp edx,eax
jb byte conv2dec_j3
conv2dec_j4:
sub esi,4
conv2dec_j1:
lodsd cs,ptr32
or eax,eax
jnz byte conv2dec_j2
mov al,' '
stosb ptr32
jmp byte conv2dec_j1
conv2dec_j2:
mov ecx,eax
mov eax,edx
sub edx,edx
div ecx
add al,'0'
stosb ptr32
cmp esi,offset conv2dec_d2
jb byte conv2dec_j1
sub eax,eax
stosb ptr32
dec edi
mov esi,DataBlock_wrtBuf
retnd
conv2dec_d1:
dd 1000000000,100000000,10000000,1000000,100000,10000,1000,100,10,1
;dd 1000000000,0,100000000,10000000,1000000,0,100000,10000,1000,0,100,10,1
conv2dec_d2:
endp
;-------------------------------

;-------------------------------
proc conv2hex
;in:  edx-value to write...
;     cl-digits to convert...
;out: esi-where converted...
mov edi,DataBlock_wrtBuf
mov al,'$'
stosb ptr32
dec ecx
and ecx,7
inc ecx
push ecx
neg cl
add cl,8
shl cl,2
rol edx,cl
pop ecx
conv2hex_j1:
rol edx,4
movzx eax,dl
and al,0fh
mov ah,'0'
cmp al,10
jb byte conv2hex_j2
mov ah,'A'
sub al,10
conv2hex_j2:
add al,ah
stosb ptr32
loopd conv2hex_j1
sub eax,eax
stosd ptr32
mov esi,DataBlock_wrtBuf
retnd
endp
;-------------------------------

;-------------------------------
proc writeDataStr
;in: cs:esi-offset of text
push ecx
push eax
push esi
sub ecx,ecx
writeDataStr_j1:
inc ecx
lodsb ptr32
or al,al
jnz byte writeDataStr_j1
dec ecx
pop esi
clts                            ;write to console...
dd 20h
pop eax
pop ecx
retnd
endp
;-------------------------------

;-------------------------------
proc writeCodeStr
;in: cs:esi-offset of text
push esi
push edi
push ecx
push eax
sub ecx,ecx
mov edi,DataBlock_wrtBuf
writeCodeStr_j1:
inc ecx
lodsb cs,ptr32
stosb ptr32
or al,al
jnz byte writeCodeStr_j1
dec ecx
mov esi,DataBlock_wrtBuf
clts                            ;write to console...
dd 20h
pop eax
pop ecx
pop edi
pop esi
retnd
endp
;-------------------------------

;-------------------------------
proc delay
push edx
push ecx
push eax
clts                            ;get uptime info...
dd 2ah
shr edx,2
delay_j1:
clts                            ;give away the control...
dd 01h
clts                            ;get uptime info...
dd 2bh
sub eax,ecx
sub eax,edx
js byte delay_j1
pop eax
pop ecx
pop edx
retnd
endp
;-------------------------------

;-------------------------------
proc writeUserStr
;in: cs:esi-offset of text
;    ecx-size in bytes...
dec ecx
jns byte writeUserStr_j1
retnd
writeUserStr_j1:
inc ecx
push esi
push edi
push eax
mov edi,esi
writeUserStr_j2:
lodsb ptr32
or al,al
jz byte writeUserStr_j3
cmp al,255
je byte writeUserStr_j3
cmp al,13
je byte writeUserStr_j3
cmp al,10
je byte writeUserStr_j3
writeUserStr_j4:
stosb ptr32
loopd writeUserStr_j2
movzx byte eax,def:[esi-1]
cmp al,' '
sete al
sub esi,eax
mov def:[esi],ah
pop eax
pop edi
pop esi
jmp dword writeDataStr
writeUserStr_j3:
mov al,' '
jmp byte writeUserStr_j4
endp
;-------------------------------

;-------------------------------
proc getVarLenInt
;in:  esi-where from read...
;     ecx-bytes to read...
;out: edx-data readed...
mov edx,ecx
cmp edx,4
jbe byte getVarLenInt_j1
mov edx,4
getVarLenInt_j1:
mov edx,cs:[getVarLenInt_d1+edx*4]
and edx,def:[esi]
add esi,ecx
retnd
getVarLenInt_d1 dd 0,0ffh,0ffffh,0ffffffh,0ffffffffh
endp
;-------------------------------




;-------------------------------
proc pcmcia_displaySck
;in: ebp-offset of data...
mov esi,offset text04
call dword writeCodeStr
movzx byte edx,ds:[ebp+oneSocket_sock]
call dword conv2dec
call dword writeDataStr
mov esi,offset textCRLF
call dword writeCodeStr
mov al,ds:[ebp+oneSocket_dtct]
or al,al
jnz byte pcmcia_displaySck_j1
mov esi,offset text05
call dword writeCodeStr
retnd
pcmcia_displaySck_j1:
mov al,ds:[ebp+oneSocket_wrpr]
mov esi,offset text09
call dword pcmcia_displaySck_j2
mov al,ds:[ebp+oneSocket_redy]
mov esi,offset text10
call dword pcmcia_displaySck_j2
mov al,ds:[ebp+oneSocket_pwon]
mov esi,offset text11
call dword pcmcia_displaySck_j2
mov al,ds:[ebp+oneSocket_pwok]
mov esi,offset text12
call dword pcmcia_displaySck_j2
mov al,ds:[ebp+oneSocket_cdtc]
mov esi,offset text13
call dword pcmcia_displaySck_j2
mov al,ds:[ebp+oneSocket_crdy]
mov esi,offset text14
call dword pcmcia_displaySck_j2
mov esi,offset textCRLF
call dword writeCodeStr
mov al,ds:[ebp+oneSocket_cden]
mov esi,offset text15
call dword pcmcia_displaySck_j2
mov al,ds:[ebp+oneSocket_atpw]
mov esi,offset text16
call dword pcmcia_displaySck_j2
mov al,ds:[ebp+oneSocket_rset]
mov esi,offset text17
call dword pcmcia_displaySck_j2
mov al,ds:[ebp+oneSocket_iocd]
mov esi,offset text18
call dword pcmcia_displaySck_j2
mov al,ds:[ebp+oneSocket_vccp]
mov esi,offset text19
call dword pcmcia_displaySck_j3
mov al,ds:[ebp+oneSocket_vppp]
mov esi,offset text20
call dword pcmcia_displaySck_j3
mov esi,offset text21
call dword writeCodeStr
movzx byte edx,ds:[ebp+oneSocket_irqn]
call dword conv2dec
call dword writeDataStr
mov esi,offset textCRLF
call dword writeCodeStr
mov eax,oneSocket_iomp
mov esi,offset text22
mov ch,1
mov cl,4
call dword pcmcia_displaySck_j5
mov eax,oneSocket_mems
mov esi,offset text23
mov ch,4
mov cl,8
call dword pcmcia_displaySck_j5
retnd
pcmcia_displaySck_j2: ;al-status, esi-text...
push eax
call dword writeCodeStr
pop eax
movzx eax,al
and al,1
mov esi,cs:[text06+eax*4]
call dword writeCodeStr
retnd
pcmcia_displaySck_j3: ;al-0.1volts, esi-text...
push eax
call dword writeCodeStr
pop eax
movzx edx,al
call dword conv2dec
push esi
mov eax,def:[esi]
or ah,ah
jnz byte pcmcia_displaySck_j4
shl eax,8
mov al,30h
mov def:[esi],eax
pcmcia_displaySck_j4:
lodsb ptr32
or al,al
jnz byte pcmcia_displaySck_j4
dec esi
dec esi
mov eax,2eh
mov ah,def:[esi]
mov def:[esi],eax
pop esi
call dword writeDataStr
retnd
pcmcia_displaySck_j5: ;eax-begin, esi-text, cl-nibbles, ch-counter...
push ebp
add ebp,eax
pcmcia_displaySck_j6:
push esi
push ecx
mov al,ds:[ebp+oneMaping_actv]
and al,1
jz dword pcmcia_displaySck_j7
call dword writeCodeStr
mov edx,ds:[ebp+oneMaping_begH]
pop ecx
push ecx
call dword conv2hex
call dword writeDataStr
mov esi,offset text24
call dword writeCodeStr
mov edx,ds:[ebp+oneMaping_begH]
add edx,ds:[ebp+oneMaping_size]
dec edx
pop ecx
push ecx
call dword conv2hex
call dword writeDataStr
mov esi,offset text25
call dword writeCodeStr
mov edx,ds:[ebp+oneMaping_begC]
pop ecx
push ecx
call dword conv2hex
call dword writeDataStr
mov esi,offset text24
call dword writeCodeStr
mov edx,ds:[ebp+oneMaping_begC]
add edx,ds:[ebp+oneMaping_size]
dec edx
pop ecx
push ecx
call dword conv2hex
call dword writeDataStr
mov esi,offset textCRLF
call dword writeCodeStr
pcmcia_displaySck_j7:
add ebp,oneMaping__siz
pop ecx
pop esi
dec ch
jns dword pcmcia_displaySck_j6
pop ebp
retnd
retnd
endp
;-------------------------------



;-------------------------------
proc pcmcia_clrSockDat
;in: ebp-offset of data...
lea edi,def:[ebp+1]
mov ecx,oneSocket__siz
dec ecx
sub eax,eax
rep
  stosb ptr32
endp
retnd
;-------------------------------

;-------------------------------
proc pcmcia_clrSockMap
;in: ebp-offset of data...
sub eax,eax
lea edi,def:[ebp+oneSocket_iomp]
mov ecx,oneMaping__siz
shl ecx,3
rep
  stosb ptr32
lea edi,def:[ebp+oneSocket_mems]
mov ecx,oneMaping__siz
shl ecx,3
rep
  stosb ptr32
endp
retnd
;-------------------------------



;-------------------------------
proc pcmcia_accessCIS
;in: ebp-offset of data...
;    edi-host offset...
;    esi-card offset...
;    ecx-bytes to move...
;    al-operation: 0-read, 1-write...
and al,1
movzx eax,al
mov eax,cs:[pcmcia_accessCIS_d1+eax*4]
mov def:[DataBlock_acsMod],eax
mov def:[DataBlock_acsByt],ecx
mov def:[DataBlock_acsCrd],esi
mov def:[DataBlock_acsHst],edi
pcmcia_accessCIS_j3:
mov eax,def:[DataBlock_acsByt]
or eax,eax
jz dword pcmcia_accessCIS_j4
lea edx,def:[ebp+oneSocket_mems]
mov byte def:[edx+oneMaping_actv],1
mov byte def:[edx+oneMaping_bits],8
mov byte def:[edx+oneMaping_time],1
mov eax,def:[DataBlock_phyMap]
mov def:[edx+oneMaping_begH],eax
mov eax,def:[DataBlock_acsCrd]
add eax,eax
mov def:[edx+oneMaping_begC],eax
mov dword def:[edx+oneMaping_size],1000h
call dword i82365_putStat
call dword i82365_enableCIS
call dword i82365_getStat
lea edx,def:[ebp+oneSocket_mems]
mov esi,def:[DataBlock_acsCrd]
add esi,esi
sub esi,def:[edx+oneMaping_begC]
mov ecx,def:[edx+oneMaping_size]
sub ecx,esi
shr ecx,1
mov edx,def:[DataBlock_acsByt]
cmp ecx,edx
jb byte pcmcia_accessCIS_j5
mov ecx,edx
pcmcia_accessCIS_j5:
push ecx
add esi,def:[DataBlock_logMap]
mov edi,def:[DataBlock_acsHst]
mov eax,def:[DataBlock_acsMod]
call eax
pop eax
sub def:[DataBlock_acsByt],eax
add def:[DataBlock_acsCrd],eax
add def:[DataBlock_acsHst],eax
jmp dword pcmcia_accessCIS_j3
pcmcia_accessCIS_j4:
call dword pcmcia_clrSockMap
call dword i82365_putStat
call dword i82365_getStat
retnd
pcmcia_accessCIS_d1 dd offset pcmcia_accessCIS_j1,offset pcmcia_accessCIS_j2
pcmcia_accessCIS_j1: ;ecx-bytes, esi-cardOfs, edi-hostOfs...
mov al,def:[esi]
inc esi
inc esi
mov def:[edi],al
inc edi
loopd pcmcia_accessCIS_j1
retnd
pcmcia_accessCIS_j2: ;ecx-bytes, esi-cardOfs, edi-hostOfs...
mov al,def:[edi]
inc edi
mov def:[esi],al
inc esi
inc esi
loopd pcmcia_accessCIS_j2
retnd
endp
;-------------------------------

;-------------------------------
proc pcmcia_readCIStuple
;in:  ebp-offset of data...
;     edi-host offset...
;     esi-card offset...
;out: eax-type of tuple...
;     ecx-size of tuple...
;     esi-updated...
push esi
push edi
mov ecx,260
sub eax,eax
call dword pcmcia_accessCIS
pop edi
movzx byte eax,def:[edi+0]
movzx byte ecx,def:[edi+1]
push eax
push ecx
lea esi,def:[edi+2]
rep
  movsb ptr32
mov ecx,256
sub eax,eax
rep
  stosb ptr32
pop ecx
pop eax
pop esi
add esi,ecx
inc esi
inc esi
retnd
endp
;-------------------------------



;-------------------------------
proc pcmcia_enableSck
;in: ebp-offset of data...
mov al,def:[DataBlock_readOn]
or al,al
jz byte pcmcia_enableSck_j1
retnd
pcmcia_enableSck_j1:
mov esi,offset text31
call dword writeCodeStr
movzx byte edx,ds:[ebp+oneSocket_sock]
call dword conv2dec
call dword writeDataStr
mov esi,offset text32
call dword writeCodeStr
call dword pcmcia_clrSockDat
call dword i82365_putStat
call dword delay
call dword i82365_getStat
mov al,ds:[ebp+oneSocket_dtct]
or al,al
jnz byte pcmcia_enableSck_j2
mov esi,offset textCRLF
call dword writeCodeStr
mov esi,offset text05
call dword writeCodeStr
retnd
pcmcia_enableSck_j2:
mov esi,offset text33
call dword writeCodeStr
call dword pcmcia_clrSockDat
mov byte ds:[ebp+oneSocket_cden],1
mov byte ds:[ebp+oneSocket_atpw],1
mov byte ds:[ebp+oneSocket_vccp],50
mov byte ds:[ebp+oneSocket_iocd],1
call dword i82365_putStat
call dword delay
mov esi,offset text34
call dword writeCodeStr
mov byte ds:[ebp+oneSocket_rset],1
call dword i82365_putStat
call dword delay
mov esi,offset text35
call dword writeCodeStr
mov byte ds:[ebp+oneSocket_rset],0
call dword i82365_putStat
call dword delay
mov esi,offset text36
call dword writeCodeStr
sub eax,eax
mov def:[DataBlock_cisPos],eax
mov edi,DataBlock_curCfg
mov ecx,oneConfig__siz
rep
  stosb ptr32
mov edi,DataBlock_bakCfg
mov ecx,oneConfig__siz
rep
  stosb ptr32
pcmcia_enableSck_j3: ;read next tuple...
mov edi,DataBlock_freMem
mov esi,def:[DataBlock_cisPos]
call dword pcmcia_readCIStuple
mov def:[DataBlock_cisPos],esi
movzx edx,al
;find tuple handler...
mov esi,offset pcmcia_enableSck_d1
pcmcia_enableSck_j4:
lodsd cs,ptr32
mov ebx,eax
lodsd cs,ptr32
or eax,eax
jz byte pcmcia_enableSck_j5
cmp edx,ebx
jne byte pcmcia_enableSck_j4
mov esi,DataBlock_freMem
push ebp
call eax
pop ebp
jmp byte pcmcia_enableSck_j3
pcmcia_enableSck_j5: ;unknown tuple...
push ecx
mov esi,offset text37
call dword writeCodeStr
mov cl,2
call dword conv2hex
call dword writeDataStr
pop ecx
mov esi,DataBlock_freMem
pcmcia_enableSck_j6:
dec ecx
js byte pcmcia_enableSck_j7
lodsb ptr32
push esi
push ecx
movzx edx,al
mov esi,offset textSPC
call dword writeCodeStr
mov cl,2
call dword conv2hex
inc esi
call dword writeDataStr
pop ecx
pop esi
jmp byte pcmcia_enableSck_j6
pcmcia_enableSck_j7:
mov esi,offset textCRLF
call dword writeCodeStr
jmp dword pcmcia_enableSck_j3
pcmcia_enableSck_d1:
dd 0ffh,offset pcmcia_enableSck_j8
dd 000h,offset pcmcia_enableSck_j9
dd 001h,offset pcmcia_enableSck_j10
dd 017h,offset pcmcia_enableSck_j10
dd 020h,offset pcmcia_enableSck_j11
dd 021h,offset pcmcia_enableSck_j12
dd 015h,offset pcmcia_enableSck_j13
dd 01ah,offset pcmcia_enableSck_j14
dd 01bh,offset pcmcia_enableSck_j15
dd 040h,offset pcmcia_enableSck_j32
dd 0,0
pcmcia_enableSck_j8: ;end of tuples...
pop ebp
pop ebp
retnd
pcmcia_enableSck_j9: ;null tuple...
retnd
pcmcia_enableSck_j10: ;device tuple...
lodsb ptr32
push eax
mov esi,offset text38
call dword writeCodeStr
pop edx
movzx edx,dl
shr edx,4
and dl,0fh
call dword conv2dec
call dword writeDataStr
mov esi,offset textCRLF
call dword writeCodeStr
retnd
pcmcia_enableSck_j11: ;manufacturer tuple...
lodsd ptr32
mov edx,eax
mov esi,offset text39
call dword writeCodeStr
push edx
mov cl,4
call dword conv2hex
call dword writeDataStr
pop edx
shr edx,16
mov esi,offset text40
call dword writeCodeStr
mov cl,4
call dword conv2hex
call dword writeDataStr
mov esi,offset textCRLF
call dword writeCodeStr
retnd
pcmcia_enableSck_j12: ;function tuple...
lodsb ptr32
mov edi,DataBlock_curCfg
mov def:[edi+oneConfig_func],al
movzx edx,al
mov esi,offset text41
call dword writeCodeStr
call dword conv2dec
call dword writeDataStr
mov esi,offset textCRLF
call dword writeCodeStr
retnd
pcmcia_enableSck_j13: ;version tuple...
sub eax,eax
lodsb ptr32
push eax
lodsb ptr32
dec ecx
dec ecx
push eax
push esi
push ecx
mov esi,offset text42
call dword writeCodeStr
pop ecx
pop esi
call dword writeUserStr
mov esi,offset text43
call dword writeCodeStr
pop edx
call dword conv2dec
call dword writeDataStr
mov esi,offset text44
call dword writeCodeStr
pop edx
call dword conv2dec
call dword writeDataStr
mov esi,offset textCRLF
call dword writeCodeStr
retnd
pcmcia_enableSck_j14: ;config tuple...
mov edi,DataBlock_curCfg
lodsw ptr32
movzx ecx,ah
mov def:[edi+oneConfig_last],ecx
movzx ecx,al
push ecx
mov cl,al
and cl,3
inc ecx
call dword getVarLenInt
mov def:[edi+oneConfig_base],edx
pop ecx
shr ecx,2
inc ecx
call dword getVarLenInt
mov def:[edi+oneConfig_mask],edx
mov esi,offset text45
call dword writeCodeStr
mov edi,DataBlock_curCfg
mov edx,def:[edi+oneConfig_base]
mov cl,8
call dword conv2hex
call dword writeDataStr
mov esi,offset text46
call dword writeCodeStr
mov edi,DataBlock_curCfg
mov edx,def:[edi+oneConfig_mask]
mov cl,8
call dword conv2hex
call dword writeDataStr
mov esi,offset textCRLF
call dword writeCodeStr
retnd
pcmcia_enableSck_j15: ;cfTable tuple...
push esi
mov esi,DataBlock_curCfg
mov edi,DataBlock_bakCfg
mov ecx,oneConfig__siz
rep
  movsb ptr32
mov edi,DataBlock_curCfg
mov ecx,oneConfig_cfgN
add edi,ecx
neg ecx
add ecx,oneConfig__siz
sub eax,eax
rep
  stosb ptr32
pop esi
lodsb ptr32
mov ah,al
and ah,3fh
mov edi,DataBlock_curCfg
mov def:[edi+oneConfig_cfgN],ah
test al,40h
setnz ah
inc ah
mov edi,DataBlock_curCfg
mov def:[edi+oneConfig_dflt],ah
test al,80h
jz byte pcmcia_enableSck_j16
lodsb ptr32
and al,0fh
mov def:[edi+oneConfig_ifce],al
pcmcia_enableSck_j16:
lodsb ptr32
movzx eax,al
push eax
;process powers...
and al,03h
pcmcia_enableSck_j17:
dec eax
js byte pcmcia_enableSck_j18
push eax
lodsb ptr32
and al,7fh
pcmcia_enableSck_j19:
test al,1
jz byte pcmcia_enableSck_j20
mov ah,def:[esi]
inc esi
test ah,80h
jnz byte pcmcia_enableSck_j19
pcmcia_enableSck_j20:
shr al,1
or al,al
jnz byte pcmcia_enableSck_j19
pop eax
jmp byte pcmcia_enableSck_j17
pcmcia_enableSck_j18:
pop eax
push eax
;process timing...
test al,4
jz byte pcmcia_enableSck_j21
lodsb ptr32
movzx eax,al
push eax
and al,3
cmp al,3
setne al
add esi,eax
pop eax
shr eax,2
push eax
and al,7
cmp al,7
setne al
add esi,eax
pop eax
shr eax,3
and al,7
cmp al,7
setne al
add esi,eax
pcmcia_enableSck_j21:
pop eax
push eax
;process io....
test al,8
jz dword pcmcia_enableSck_j22
lodsb ptr32
mov ah,al
shr ah,5
and ah,3
mov edi,DataBlock_curCfg
mov def:[edi+oneConfig_ioBt],ah
test al,80h
jnz byte pcmcia_enableSck_j23
mov edi,DataBlock_curCfg
mov byte def:[edi+oneConfig_iosN],1
add edi,oneConfig_iosD
mov cl,al
and cl,1fh
sub eax,eax
stosd ptr32
stosd ptr32
inc eax
shl eax,cl
stosd ptr32
jmp dword pcmcia_enableSck_j22
pcmcia_enableSck_j23:
lodsb ptr32
movzx ecx,al
and cl,0fh
inc ecx
mov edi,DataBlock_curCfg
mov def:[edi+oneConfig_iosN],cl
shr al,4
mov ah,al
shr ah,2
and eax,0303h
cmp al,03h
sete dl
cmp ah,03h
sete dh
add eax,edx
movzx edx,al
mov def:[DataBlock_tempn1],edx ;size of base...
movzx edx,ah
mov def:[DataBlock_tempn2],edx ;size of length...
add edi,oneConfig_iosD
dec ecx
pcmcia_enableSck_j24:
push ecx
mov ecx,def:[DataBlock_tempn1]
call dword getVarLenInt
mov eax,edx
stosd ptr32
sub eax,eax
stosd ptr32
mov ecx,def:[DataBlock_tempn2]
call dword getVarLenInt
lea eax,def:[edx+1]
stosd ptr32
pop ecx
dec ecx
jns dword pcmcia_enableSck_j24
pcmcia_enableSck_j22:
pop eax
push eax
;process irq....
test al,10h
jz dword pcmcia_enableSck_j25
lodsb ptr32
mov ah,al
and al,0fh
mov edi,DataBlock_curCfg
mov def:[edi+oneConfig_irqN],al
test ah,10h
jz byte pcmcia_enableSck_j25
mov cl,al
lodsw ptr32
or cl,cl
jnz byte pcmcia_enableSck_j25
and eax,7ffch
bsf ecx,eax
mov def:[edi+oneConfig_irqN],cl
pcmcia_enableSck_j25:
pop eax
;process memory...
shr eax,5
and al,3
mov eax,cs:[pcmcia_enableSck_d2+eax*4]
mov edi,DataBlock_curCfg
jmp eax
pcmcia_enableSck_d2:
dd offset pcmcia_enableSck_j26,offset pcmcia_enableSck_j27
dd offset pcmcia_enableSck_j28,offset pcmcia_enableSck_j29
pcmcia_enableSck_j27:
mov byte def:[edi+oneConfig_memN],1
add edi,oneConfig_memD
sub eax,eax
stosd ptr32
stosd ptr32
lodsw ptr32
shl eax,8
stosd ptr32
jmp dword pcmcia_enableSck_j26
pcmcia_enableSck_j28:
mov byte def:[edi+oneConfig_memN],1
add edi,oneConfig_memD
sub eax,eax
lodsw ptr32
mov edx,eax
lodsw ptr32
shl eax,8
stosd ptr32
sub eax,eax
stosd ptr32
mov eax,edx
shl eax,8
stosd ptr32
jmp dword pcmcia_enableSck_j26
pcmcia_enableSck_j29:
lodsb ptr32
mov ah,al
and ah,7
inc ah
mov def:[edi+oneConfig_memN],ah
movzx ecx,al
shr cl,3
and cl,3
mov def:[DataBlock_tempn1],ecx ;length size...
mov cl,al
shr cl,5
and cl,3
mov def:[DataBlock_tempn2],ecx ;card address size...
test al,80h
setz al
dec eax
and cl,al
mov def:[DataBlock_tempn3],ecx ;host address size...
movzx byte ecx,def:[edi+oneConfig_memN]
add edi,oneConfig_memD
pcmcia_enableSck_j30:
push ecx
mov ecx,def:[DataBlock_tempn1]
call dword getVarLenInt
shl edx,8
mov def:[edi+8],edx
mov ecx,def:[DataBlock_tempn2]
call dword getVarLenInt
shl edx,8
mov def:[edi+0],edx
mov ecx,def:[DataBlock_tempn3]
call dword getVarLenInt
shl edx,8
mov def:[edi+4],edx
pop ecx
add edi,12
loopd pcmcia_enableSck_j30
jmp dword pcmcia_enableSck_j26
pcmcia_enableSck_j32: ;version tuple...
mov eax,9
add esi,eax
sub ecx,eax
push esi
push ecx
mov esi,offset text42
call dword writeCodeStr
pop ecx
pop esi
call dword writeUserStr
mov esi,offset textCRLF
call dword writeCodeStr
retnd
pcmcia_enableSck_j26:
;update if needed...
mov edi,DataBlock_curCfg
mov al,def:[edi+oneConfig_irqN]
or al,al
setnz al
shl al,4
mov ecx,def:[edi+oneConfig_iosD]
or ecx,ecx
setnz cl
shl cl,5
or al,cl
add edi,oneConfig_memD
mov ecx,def:[edi+0]
or ecx,ecx
setnz cl
shl cl,6
or al,cl
mov ecx,def:[edi+4]
or ecx,ecx
setnz cl
shl cl,7
or al,cl
mov edi,DataBlock_curCfg
or def:[edi+oneConfig_dflt],al
call dword pcmcia_printCfg
mov edi,DataBlock_curCfg
mov esi,DataBlock_bakCfg
mov al,def:[edi+oneConfig_dflt]
mov ah,def:[esi+oneConfig_dflt]
cmp al,ah
jae byte pcmcia_enableSck_j31
mov ecx,oneConfig__siz
rep
  movsb ptr32
pcmcia_enableSck_j31:
retnd
endp
;-------------------------------

;-------------------------------
proc pcmcia_printCfg
;print whole tuple...
mov esi,offset text47
call dword writeCodeStr
mov edi,DataBlock_curCfg
movzx byte edx,def:[edi+oneConfig_cfgN]
call dword conv2dec
call dword writeDataStr
mov esi,offset text48
call dword writeCodeStr
mov edi,DataBlock_curCfg
movzx byte edx,def:[edi+oneConfig_ifce]
call dword conv2dec
call dword writeDataStr
mov esi,offset text49
call dword writeCodeStr
mov edi,DataBlock_curCfg
movzx byte edx,def:[edi+oneConfig_dflt]
call dword conv2dec
call dword writeDataStr
mov esi,offset text50
call dword writeCodeStr
mov edi,DataBlock_curCfg
movzx byte edx,def:[edi+oneConfig_irqN]
call dword conv2dec
call dword writeDataStr
mov esi,offset textCRLF
call dword writeCodeStr
mov edi,DataBlock_curCfg
mov ch,def:[edi+oneConfig_iosN]
mov cl,4
add edi,oneConfig_iosD
mov esi,offset text51
call dword pcmcia_printCfg_j1
mov edi,DataBlock_curCfg
mov ch,def:[edi+oneConfig_memN]
mov cl,8
add edi,oneConfig_memD
mov esi,offset text52
call dword pcmcia_printCfg_j1
retnd
pcmcia_printCfg_j1: ;edi-database, esi-text, cl-digits, ch-number..
;print mappings...
mov def:[DataBlock_tempn1],esi ;text...
movzx eax,cl
mov def:[DataBlock_tempn2],eax ;digits...
movzx ecx,ch
mov esi,edi
pcmcia_printCfg_j2:
dec ecx
jns byte pcmcia_printCfg_j3
retnd
pcmcia_printCfg_j3:
push ecx
push esi
mov esi,def:[DataBlock_tempn1]
call dword writeCodeStr
pop esi
push esi
mov edx,def:[esi+4]
mov ecx,def:[DataBlock_tempn2]
call dword conv2hex
call dword writeDataStr
mov esi,offset text53
call dword writeCodeStr
pop esi
push esi
mov edx,def:[esi+4]
add edx,def:[esi+8]
dec edx
mov ecx,def:[DataBlock_tempn2]
call dword conv2hex
call dword writeDataStr
mov esi,offset text54
call dword writeCodeStr
pop esi
push esi
mov edx,def:[esi+0]
mov ecx,def:[DataBlock_tempn2]
call dword conv2hex
call dword writeDataStr
mov esi,offset text53
call dword writeCodeStr
pop esi
push esi
mov edx,def:[esi+0]
add edx,def:[esi+8]
dec edx
mov ecx,def:[DataBlock_tempn2]
call dword conv2hex
call dword writeDataStr
mov esi,offset textCRLF
call dword writeCodeStr
pop esi
pop ecx
add esi,12
jmp dword pcmcia_printCfg_j2
endp
;-------------------------------

;-------------------------------
proc pcmcia_applyCfg
;in: ebp-offset of data...
mov al,def:[DataBlock_readOn]
or al,al
jz byte pcmcia_applyCfg_j1
retnd
pcmcia_applyCfg_j1:
mov al,ds:[ebp+oneSocket_dtct]
or al,al
jnz byte pcmcia_applyCfg_j7
retnd
pcmcia_applyCfg_j7:
mov esi,offset text55
call dword writeCodeStr
call dword pcmcia_printCfg
mov edi,DataBlock_curCfg
movzx byte ecx,def:[edi+oneConfig_iosN]
add edi,oneConfig_iosD
mov ebx,100h
mov edx,400h
call dword pcmcia_applyCfg_j2
mov edi,DataBlock_curCfg
movzx byte ecx,def:[edi+oneConfig_memN]
add edi,oneConfig_memD
mov ebx,0c0000h
mov edx,0e0000h
call dword pcmcia_applyCfg_j2
;socket and copy register...
mov dl,3
mov al,0
mov cl,1
call dword pcmcia_applyCfg_j8
;configuration option register...
mov dl,0
mov edi,DataBlock_curCfg
mov al,def:[edi+oneConfig_cfgN]
mov cl,1
call dword pcmcia_applyCfg_j8
;card configuration and status register...
mov dl,1
mov al,48h
mov cl,1
call dword pcmcia_applyCfg_j8
;pin replacement register...
mov dl,2
mov al,00h
mov cl,1
call dword pcmcia_applyCfg_j8
;extended status register...
mov dl,4
mov al,20h
mov cl,1
call dword pcmcia_applyCfg_j8
;io base register...
mov dl,5
mov edi,DataBlock_curCfg
mov eax,def:[edi+oneConfig_iosD]
mov cl,4
call dword pcmcia_applyCfg_j8
;io size register...
mov dl,9
mov edi,DataBlock_curCfg
add edi,oneConfig_iosD
mov al,def:[edi+8]
mov cl,1
call dword pcmcia_applyCfg_j8
;compose config...
call dword i82365_getStat
call dword pcmcia_clrSockMap
call dword i82365_putStat
lea edi,def:[ebp+oneSocket_iomp]
mov esi,DataBlock_curCfg
add esi,oneConfig_iosD
mov ecx,8
call dword pcmcia_applyCfg_j9
lea edi,def:[ebp+oneSocket_mems]
mov esi,DataBlock_curCfg
add esi,oneConfig_memD
mov ecx,8
call dword pcmcia_applyCfg_j9
mov esi,DataBlock_curCfg
mov al,def:[esi+oneConfig_irqN]
mov ds:[ebp+oneSocket_irqn],al
call dword i82365_putStat
mov esi,offset text56
call dword writeCodeStr
call dword pcmcia_printCfg
retnd
pcmcia_applyCfg_j2: ;ebx-first, edx-last, edi-database, ecx-count...
dec ecx
jns byte pcmcia_applyCfg_j3
retnd
pcmcia_applyCfg_j3:
mov eax,def:[edi]
cmp eax,ebx
jae byte pcmcia_applyCfg_j4
mov eax,ebx
pcmcia_applyCfg_j4:
cmp eax,edx
jbe byte pcmcia_applyCfg_j5
mov eax,edx
pcmcia_applyCfg_j5:
mov def:[edi],eax
mov edx,def:[edi+4]
or edx,edx
jnz byte pcmcia_applyCfg_j6
mov edx,eax
pcmcia_applyCfg_j6:
mov def:[edi+4],edx
add edi,12
jmp byte pcmcia_applyCfg_j2
retnd
pcmcia_applyCfg_j8: ;dl-cisreg, eax-value, cl-count...
mov edi,DataBlock_tempn1
mov def:[edi],eax
mov esi,DataBlock_curCfg
mov esi,def:[esi+oneConfig_base]
shr esi,1
movzx edx,dl
add esi,edx
mov eax,1
movzx ecx,cl
call dword pcmcia_accessCIS
retnd
pcmcia_applyCfg_j9: ;esi-config, edi-socket, ecx-counter...
push ecx
push esi
push edi
mov eax,def:[esi+0]
or eax,eax
jz byte pcmcia_applyCfg_j10
mov def:[edi+oneMaping_begC],eax
mov eax,def:[esi+4]
mov def:[edi+oneMaping_begH],eax
mov eax,def:[esi+8]
mov def:[edi+oneMaping_size],eax
mov byte def:[edi+oneMaping_auto],1
mov byte def:[edi+oneMaping_actv],1
mov esi,DataBlock_curCfg
mov al,def:[esi+oneConfig_ioBt]
and al,2
setnz al
inc eax
shl eax,3
mov def:[edi+oneMaping_bits],al
pcmcia_applyCfg_j10:
pop edi
pop esi
pop ecx
add edi,oneMaping__siz
add esi,12
loopd pcmcia_applyCfg_j9
retnd
endp
;-------------------------------



;-------------------------------
oneSocket_sock equ 000h         ;db: w: socket number...
oneSocket_dtct equ 001h         ;db: r: card detected (0=no, 1=yes)...
oneSocket_wrpr equ 002h         ;db: r: write protected (0=no, 1=yes)...
oneSocket_redy equ 003h         ;db: r: card ready (0=no, 1=yes)...
oneSocket_pwon equ 004h         ;db: r: power turned on (0=no, 1=yes)...
oneSocket_pwok equ 005h         ;db: r: power good (0=no, 1=yes)...
oneSocket_cden equ 006h         ;db: rw: card outputs enabled (0=no, 1=yes)...
oneSocket_atpw equ 007h         ;db: rw: auto power on/off (0=no, 1=yes)...
oneSocket_vccp equ 008h         ;db: rw: vcc power (in 0.1 volts)...
oneSocket_vppp equ 009h         ;db: rw: vpp power (in 0.1 volts)...
oneSocket_rset equ 00ah         ;db: rw: reset the card (0=no, 1=yes)...
oneSocket_iocd equ 00bh         ;db: rw: io card interface (0=mem, 1=io)...
oneSocket_cdtc equ 00ch         ;db: r: card detect changed (0=no, 1=yes)...
oneSocket_crdy equ 00dh         ;db: r: card ready changed (0=no, 1=yes)...
oneSocket_irqn equ 00eh         ;db: rw: card irq number...
oneSocket_iomp equ 020h         ;8*: rw: io mapping...
oneSocket_mems equ 0c0h         ;8*: rw: memory mapping...
oneSocket__siz equ 160h         ;size of structure...
;-------------------------------

;-------------------------------
oneMaping_actv equ 00h          ;db: rw: active (0=no, 1=yes)...
oneMaping_bits equ 01h          ;db: rw: decode size (8, 16)...
oneMaping_time equ 02h          ;db: rw: timing mode (0, 1)...
oneMaping_auto equ 03h          ;db: rw: auto size (0=no, 1=yes)...
oneMaping_wrpr equ 04h          ;db: rw: write protected (0=no, 1=yes)...
oneMaping_begH equ 08h          ;dd: rw: host io begin...
oneMaping_begC equ 0ch          ;dd: rw: card io begin...
oneMaping_size equ 10h          ;dd: rw: size of region...
oneMaping__siz equ 14h          ;size of structure...
;-------------------------------

;-------------------------------
oneConfig_base equ 000h         ;dd: config regs base...
oneConfig_mask equ 004h         ;dd: config regs mask...
oneConfig_last equ 008h         ;dd: config regs last...
oneConfig_cfgN equ 00ch         ;db: config number...
oneConfig_dflt equ 00dh         ;db: default config (0=none, 1=normal, 2=default)...
oneConfig_ifce equ 00eh         ;db: interface number...
oneConfig_irqN equ 00fh         ;db: irq number...
oneConfig_iosN equ 010h         ;db: number of io regions...
oneConfig_memN equ 011h         ;db: number of mem regions...
oneConfig_ioBt equ 012h         ;db: io bit mask: b0-8, b1-16...
oneConfig_func equ 013h         ;db: function id...
oneConfig_iosD equ 020h         ;16*: card,host,len...
oneConfig_memD equ 0e0h         ;16*: card,host,len...
oneConfig__siz equ 200h         ;size of structure...
;-------------------------------

;-------------------------------
DataBlock_wrtBuf equ 0000h      ;256: write buffer...
DataBlock_crdNum equ 0104h      ;dd: number of cards...
DataBlock_readOn equ 0108h      ;dd: read only mode...
DataBlock_phyMap equ 010ch      ;dd: physical memory offset...
DataBlock_logMap equ 0110h      ;dd: logical memory offset...
DataBlock_acsMod equ 0114h      ;dd: accessCis - operator...
DataBlock_acsByt equ 0118h      ;dd: accessCis - bytes left...
DataBlock_acsCrd equ 011ch      ;dd: accessCis - card offset...
DataBlock_acsHst equ 0120h      ;dd: accessCis - host offset...
DataBlock_cisPos equ 0124h      ;dd: current cis position...
DataBlock_tempn1 equ 0128h      ;dd: temporary number for parsing...
DataBlock_tempn2 equ 012ch      ;dd: temporary number for parsing...
DataBlock_tempn3 equ 0130h      ;dd: temporary number for parsing...
DataBlock_sckDat equ 0140h      ;512: socket info record...
DataBlock_curCfg equ 0340h      ;512: current configuration...
DataBlock_bakCfg equ 0540h      ;512: backup configuration...
DataBlock_freMem equ 0740h      ;free memory...
;-------------------------------

;-------------------------------
include pcmcia_i82365.inc
;-------------------------------

;-------------------------------
textSPC db ' ',0
textCRLF db 13,10,0
text01 db 'pcmcia enumerator v1.0, done by Mc at ',%date,' ',%time,'.',13,10,0
text02 db 'detecting sockets... ',0
text03 db ' found!',13,10,0
text04 db 16 dup ('-'),13,10,'socket: ',0
text05 db 'no card detected!',13,10,0
text06 dd offset text07,offset text08
text07 db 'no',0
text08 db 'yes',0
text09 db 'writeProt:',0
text10 db ' ready:',0
text11 db ' powerOn:',0
text12 db ' powerOK:',0
text13 db ' cardChgd:',0
text14 db ' readyChgd:',0
text15 db 'enabled:',0
text16 db ' autoPwr:',0
text17 db ' reset:',0
text18 db ' ioCard:',0
text19 db ' vcc:',0
text20 db ' vpp:',0
text21 db ' irq:',0
text22 db 'io: ',0
text23 db 'memory: ',0
text24 db '..',0
text25 db '->',0
text26 db 'working in ',0
text27 db ' mode.',13,10,0
text28 dd offset text29,offset text30
text29 db 'configurator',0
text30 db 'read-only',0
text31 db '=',64 dup ('-'),'=',13,10,'socket=',0
text32 db 13,10,'disabling card...',0
text33 db 13,10,'powering card...',0
text34 db 13,10,'resetting card...',0
text35 db 13,10,'enabling card...',0
text36 db 13,10,'parsing tuples...',13,10,0
text37 db 'unknown tag:',0
text38 db 'devType=',0
text39 db 'manufacturer=',0
text40 db ' card=',0
text41 db 'function=',0
text42 db 'text=',0
text43 db ' v',0
text44 db '.',0
text45 db 'cisRegBase=',0
text46 db ' cisRegMask=',0
text47 db 'devCfg=',0
text48 db ' iface=',0
text49 db ' priority=',0
text50 db ' irq=',0
text51 db '  io=',0
text52 db '  mem=',0
text53 db '..',0
text54 db '->',0
text55 db 'selected= ',0
text56 db 'applying= ',0
;-------------------------------

lastbyte:
