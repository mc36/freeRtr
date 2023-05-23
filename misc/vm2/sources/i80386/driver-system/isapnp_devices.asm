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

sub edi,edi
clts                            ;get process parameters...
dd 13h
mov al,def:[edi]
or al,al
setz al
movzx eax,al
mov def:[DataBlock_readOn],eax
mov esi,offset text35
call dword writeCodeStr
mov eax,def:[DataBlock_readOn]
mov esi,cs:[text37+eax*4]
call dword writeCodeStr
mov esi,offset text36
call dword writeCodeStr


mov eax,def:[DataBlock_readOn]
and al,1
jnz byte main_j1
call dword isapnp_isolateCards
jmp byte main_j2
main_j1:
call dword isapnp_detectCards
main_j2:

mov eax,def:[DataBlock_readOn]
and al,1
jnz byte main_j4
sub ebp,ebp
main_j3:
inc ebp
cmp ebp,def:[DataBlock_crdNum]
ja byte main_j4
push ebp
call dword isapnp_showResources
pop ebp
jmp byte main_j3
main_j4:

sub ebp,ebp
main_j5:
inc ebp
cmp ebp,def:[DataBlock_crdNum]
ja byte main_j6
push ebp
call dword isapnp_showConfig
pop ebp
jmp byte main_j5
main_j6:

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
push ecx
mov ecx,32
delay_j1:
clts                            ;give away the control...
dd 01h
loopd delay_j1
pop ecx
retnd
endp
;-------------------------------









;-------------------------------
proc isapnp_readResRec
;in:  edi-where to write...
;out: bl-type of res rec...
;     ecx-bytes readed...
push edi
mov ecx,1
call dword isapnp_readResrc
pop edi
mov bl,def:[edi]
test bl,80h
jnz byte isapnp_readResRec_j1
movzx ecx,bl
and cl,7
shr bl,3
jmp byte isapnp_readResRec_j2
isapnp_readResRec_j1:
push edi
mov ecx,2
call dword isapnp_readResrc
pop edi
movzx word ecx,def:[edi]
cmp ecx,0ffffh
jne byte isapnp_readResRec_j2
sub ecx,ecx
mov bl,0fh
isapnp_readResRec_j2:
push ecx
call dword isapnp_readResrc
sub eax,eax
stosd ptr32
stosd ptr32
pop ecx
retnd
endp
;-------------------------------

;-------------------------------
proc isapnp_decodeVendor
;in: ebx-vendor id...
;    edi-where to write...
xchg bl,bh
test bh,80h
jz byte isapnp_decodeVendor_j1
mov al,'?'
stosb ptr32
isapnp_decodeVendor_j1:
shl bx,1
mov ecx,3
isapnp_decodeVendor_j2:
rol bx,5
mov al,bl
and al,1fh
add al,40h
stosb ptr32
loopd isapnp_decodeVendor_j2
mov al,'-'
stosb ptr32
mov edx,ebx
shr edx,16
xchg dl,dh
mov cl,4
push edi
call dword conv2hex
pop edi
inc esi
isapnp_decodeVendor_j3:
lodsb ptr32
stosb ptr32
or al,al
jnz byte isapnp_decodeVendor_j3
retnd
endp
;-------------------------------

;-------------------------------
proc isapnp_readResrc
;in: edi-where to read...
;    ecx-number of bytes to read...
jecxz byte isapnp_readResrc_j2
isapnp_readResrc_j1:
call dword delay
call dword isapnp_getStat
and al,1
jz byte isapnp_readResrc_j1
call dword isapnp_getResrc
stosb ptr32
loopd isapnp_readResrc_j1
isapnp_readResrc_j2:
retnd
endp
;-------------------------------

;-------------------------------
proc isapnp_getResrc
;out: al-resource byte...
mov al,4                        ;resource register...
call dword isapnp_read
retnd
endp
;-------------------------------

;-------------------------------
proc isapnp_getStat
;out: al-status byte...
mov al,5                        ;resource register...
call dword isapnp_read
retnd
endp
;-------------------------------

;-------------------------------
proc isapnp_setCardCSN
;in: ah-card new address...
mov al,6                        ;card csn register...
call dword isapnp_write
call dword delay
retnd
endp
;-------------------------------

;-------------------------------
proc isapnp_selectCard
;in: ah-card number...
mov al,3                        ;card selector...
call dword isapnp_write
call dword delay
retnd
endp
;-------------------------------

;-------------------------------
proc isapnp_selectLogDev
;in:  ah-device number...
;out: carry cleared if successful...
push eax
mov al,7                        ;logical device selector...
call dword isapnp_write
call dword delay
pop eax
mov al,7                        ;logical device selector...
call dword isapnp_read
cmp al,ah
jne byte isapnp_selectLogDev_err
clc
retnd
isapnp_selectLogDev_err:
stc
retnd
endp
;-------------------------------

;-------------------------------
proc isapnp_setReadPort
;in: ax-io address of port...
shr ax,2
mov ah,al
mov al,0                        ;set read base port...
call dword isapnp_write
call dword delay
retnd
endp
;-------------------------------

;-------------------------------
proc isapnp_calcSum
;in:  esi-data to sum...
;     ecx-number of bytes...
;out: al-checksum...
mov bh,6ah
isapnp_calcSum_j1:
lodsb ptr32
mov bl,al
push ecx
mov ecx,8
isapnp_calcSum_j2:
mov al,bh
shr al,1
xor al,bl
xor al,bh
shl al,7
shr bh,1
or bh,al
shr bl,1
loopd isapnp_calcSum_j2
pop ecx
loopd isapnp_calcSum_j1
mov al,bh
retnd
endp
;-------------------------------

;-------------------------------
proc isapnp_readSerial
;in: edi-where to read...
;    ecx-number of bytes to read...
sub eax,eax                     ;select this card...
call dword isapnp_selectCard
mov eax,isapnpPort_read         ;set reader port...
call dword isapnp_setReadPort
mov edx,isapnpPort_addr         ;start reading...
mov al,1
out dx,al
call dword delay
mov edx,isapnpPort_read
isapnp_readSerial_j3:
call dword isapnp_readSerial_j1
stosb ptr32
loopd isapnp_readSerial_j3
retnd
isapnp_readSerial_j1:   ;read one byte to al,..
push ecx
push ebx
mov ecx,8
isapnp_readSerial_j2:
call dword delay
in al,dx
mov ah,al
call dword delay
in al,dx
cmp ax,55aah
sete bh
shr ebx,1
loopd isapnp_readSerial_j2
mov al,bl
pop ebx
pop ecx
retnd
endp
;-------------------------------

;-------------------------------
proc isapnp_sendIsoKey
call dword delay
mov eax,0202h                   ;magic write...
call dword isapnp_write
call dword delay
sub eax,eax                     ;send 2 zeros to wake card up...
mov edx,isapnpPort_addr
out dx,al
out dx,al
call dword delay
mov esi,offset isapnpKey_beg
mov ecx,offset isapnpKey_end
sub ecx,esi
isapnp_sendIsoKey_j1:
lodsb cs,ptr32
mov edx,isapnpPort_addr
out dx,al
loopd isapnp_sendIsoKey_j1
call dword delay
retnd
endp
;-------------------------------

;-------------------------------
proc isapnp_resetCards
mov ax,0702h                    ;reset cards...
call dword isapnp_write
call dword delay
retnd
endp
;-------------------------------

;-------------------------------
proc isapnp_write
;in: al-address to write...
;    ah-byte to write...
mov edx,isapnpPort_addr
out dx,al
mov al,ah
mov edx,isapnpPort_wrte
out dx,al
retnd
endp
;-------------------------------

;-------------------------------
proc isapnp_read
;in:  al-address to write...
;out: al-byte to write...
mov edx,isapnpPort_addr
out dx,al
mov edx,isapnpPort_read
in al,dx
retnd
endp
;-------------------------------

;-------------------------------
isapnpPort_addr equ 0279h       ;address port...
isapnpPort_wrte equ 0a79h       ;write port...
isapnpPort_read equ 0273h       ;read port...
isapnpKey_beg:                  ;isolation key...
db 06ah,0b5h,0dah,0edh,0f6h,0fbh,07dh,0beh
db 0dfh,06fh,037h,01bh,00dh,086h,0c3h,061h
db 0b0h,058h,02ch,016h,08bh,045h,0a2h,0d1h
db 0e8h,074h,03ah,09dh,0ceh,0e7h,073h,039h
isapnpKey_end:
;-------------------------------

;-------------------------------
proc configLogDev
;in: ebp-offset of config record...
mov eax,def:[DataBlock_readOn]
and al,1
jz byte configLogDev_j1
retnd
configLogDev_j1:
mov esi,offset text23
call dword writeCodeStr
mov edx,ds:[ebp+oneConfig_devn]
push dword ds:[ebp+oneConfig_cfgn]
call dword conv2dec
call dword writeDataStr
mov esi,offset text24
call dword writeCodeStr
pop edx
call dword conv2dec
call dword writeDataStr
mov esi,offset text25
call dword writeCodeStr
mov esi,DataBlock_bstCfg
mov ah,ds:[ebp+oneConfig_devn]
call dword isapnp_selectLogDev
jnc byte configLogDev_j2
mov esi,offset text26
call dword writeCodeStr
retnd
configLogDev_j2:
;setup io lines...
lea esi,def:[ebp+oneConfig_prtD]
mov ecx,8
mov bl,60h
configLogDev_j3:
mov ah,def:[esi+1]
mov al,bl
call dword isapnp_write
inc ebx
mov ah,def:[esi+0]
mov al,bl
call dword isapnp_write
inc ebx
add esi,8
loopd configLogDev_j3
;setup irq lines...
lea esi,def:[ebp+oneConfig_irqD]
mov ecx,2
mov bl,70h
configLogDev_j4:
mov ah,def:[esi+0]
mov al,bl
call dword isapnp_write
inc ebx
mov ah,2
mov al,bl
call dword isapnp_write
inc ebx
add esi,4
loopd configLogDev_j4
;setup dma lines...
lea esi,def:[ebp+oneConfig_dmaD]
mov ecx,2
mov bl,74h
configLogDev_j5:
mov ah,def:[esi+0]
mov al,bl
call dword isapnp_write
inc ebx
add esi,4
loopd configLogDev_j5
;setup mem lines...
lea esi,def:[ebp+oneConfig_memD]
mov ecx,4
mov bl,40h
configLogDev_j6:
mov ah,def:[esi+2]
mov al,bl
call dword isapnp_write
inc ebx
mov ah,def:[esi+1]
mov al,bl
call dword isapnp_write
inc ebx
call dword isapnp_read
inc ebx
mov edi,def:[esi+4]
and al,1
jz byte configLogDev_j7
add edi,def:[esi+0]
shr edi,8
configLogDev_j7:
mov eax,edi
mov al,bl
call dword isapnp_write
inc ebx
mov eax,edi
shl eax,8
mov al,bl
call dword isapnp_write
add ebx,4
add esi,8
loopd configLogDev_j6
;setup mem32 lines...
lea esi,def:[ebp+oneConfig_mewD]
mov ecx,4
mov bl,76h
configLogDev_j8:
mov ah,def:[esi+3]
mov al,bl
call dword isapnp_write
inc ebx
mov ah,def:[esi+2]
mov al,bl
call dword isapnp_write
inc ebx
mov ah,def:[esi+1]
mov al,bl
call dword isapnp_write
inc ebx
mov ah,def:[esi+0]
mov al,bl
call dword isapnp_write
inc ebx
call dword isapnp_read
inc ebx
mov edi,def:[esi+4]
and al,1
jz byte configLogDev_j9
add edi,def:[esi+0]
configLogDev_j9:
mov eax,edi
shr eax,16
mov al,bl
call dword isapnp_write
inc ebx
mov eax,edi
shr eax,8
mov al,bl
call dword isapnp_write
inc ebx
mov eax,edi
mov al,bl
call dword isapnp_write
inc ebx
mov eax,edi
shl eax,8
mov al,bl
call dword isapnp_write
add ebx,8
add esi,8
dec ecx
jnz dword configLogDev_j8
;enable device...
mov eax,0031h                   ;no checking...
call dword isapnp_write
mov eax,0130h                   ;activated...
call dword isapnp_write
mov esi,offset textCRLF
call dword writeCodeStr
retnd
endp
;-------------------------------




;-------------------------------
proc isapnp_isolateCards
mov esi,offset text02
call dword writeCodeStr
sub eax,eax
mov def:[DataBlock_crdNum],eax
call dword isapnp_sendIsoKey
call dword isapnp_resetCards
call dword isapnp_sendIsoKey
isapnp_isolateCards_j1:
mov ecx,9
mov edi,DataBlock_freMem
call dword isapnp_readSerial
mov ecx,8
mov esi,DataBlock_freMem
call dword isapnp_calcSum
mov esi,DataBlock_freMem
add esi,8
cmp al,def:[esi]
jne byte isapnp_isolateCards_j2
inc dword def:[DataBlock_crdNum]
mov ah,def:[DataBlock_crdNum]
call dword isapnp_setCardCSN
jmp byte isapnp_isolateCards_j1
isapnp_isolateCards_j2:
mov edx,def:[DataBlock_crdNum]
call dword conv2dec
call dword writeDataStr
mov esi,offset text03
call dword writeCodeStr
retnd
endp
;-------------------------------

;-------------------------------
proc isapnp_detectCards
mov esi,offset text33
call dword writeCodeStr
sub eax,eax
mov def:[DataBlock_crdNum],eax
isapnp_detectCards_j1:
inc dword def:[DataBlock_crdNum]
mov ah,def:[DataBlock_crdNum]
or ah,ah
jz byte isapnp_detectCards_j2
call dword isapnp_selectCard
mov eax,isapnpPort_read         ;set reader port...
call dword isapnp_setReadPort
mov ecx,9
mov edi,DataBlock_freMem
call dword isapnp_readResrc
mov ecx,8
mov esi,DataBlock_freMem
call dword isapnp_calcSum
mov esi,DataBlock_freMem
add esi,8
cmp al,def:[esi]
je byte isapnp_detectCards_j1
isapnp_detectCards_j2:
dec dword def:[DataBlock_crdNum]
mov edx,def:[DataBlock_crdNum]
call dword conv2dec
call dword writeDataStr
mov esi,offset text34
call dword writeCodeStr
retnd
endp
;-------------------------------



;-------------------------------
proc isapnp_showResources
;in: ebp-card number...
;    carry-cleared if succeeded...
mov eax,ebp
mov ah,al
call dword isapnp_selectCard
mov ecx,9
mov edi,DataBlock_freMem
call dword isapnp_readResrc
mov ecx,8
mov esi,DataBlock_freMem
call dword isapnp_calcSum
mov esi,DataBlock_freMem
add esi,8
cmp al,def:[esi]
je byte isapnp_displayCard_j1
stc
retnd
isapnp_displayCard_j1:
mov esi,offset text04
call dword writeCodeStr
mov edx,ebp
call dword conv2dec
call dword writeDataStr
mov esi,offset text05
call dword writeCodeStr
mov edi,DataBlock_freMem
mov ebx,def:[edi]
push dword def:[edi+4]
call dword isapnp_decodeVendor
mov esi,DataBlock_freMem
call dword writeDataStr
mov esi,offset text06
call dword writeCodeStr
pop edx
mov cl,8
call dword conv2hex
inc esi
call dword writeDataStr
mov esi,offset textCRLF
call dword writeCodeStr
;parse tags...
sub eax,eax
mov def:[DataBlock_devNum],eax
mov def:[DataBlock_cfgNum],eax
mov edi,DataBlock_curCfg
mov ecx,oneConfig__siz
rep
  stosb ptr32
mov edi,DataBlock_bstCfg
mov ecx,oneConfig__siz
rep
  stosb ptr32
dec ax
mov def:[DataBlock_bstCfg],eax
dec eax
mov def:[DataBlock_curCfg],eax
isapnp_displayCard_j2:
mov edi,DataBlock_freMem
call dword isapnp_readResRec
mov esi,offset isapnp_displayCard_d1
isapnp_displayCard_j3:
lodsd cs,ptr32
or eax,eax
jz byte isapnp_displayCard_j4
mov edx,eax
lodsd cs,ptr32
cmp dl,bl
jne byte isapnp_displayCard_j3
mov esi,DataBlock_freMem
mov edi,esi
mov ebp,esi
jmp eax
isapnp_displayCard_j4: ;unknown tag...
push ecx
mov esi,offset text07
call dword writeCodeStr
mov dl,bl
mov cl,2
call dword conv2hex
call dword writeDataStr
pop ecx
mov esi,DataBlock_freMem
isapnp_displayCard_j5:
dec ecx
js byte isapnp_displayCard_j6
lodsb ptr32
push ecx
push esi
mov dl,al
mov cl,2
call dword conv2hex
mov byte def:[esi],32
call dword writeDataStr
pop esi
pop ecx
jmp byte isapnp_displayCard_j5
isapnp_displayCard_j6:
mov esi,offset textCRLF
call dword writeCodeStr
jmp dword isapnp_displayCard_j2
isapnp_displayCard_j7: ;tag: end...
call dword isapnp_displayCard_j30
call dword isapnp_displayCard_j32
clc
retnd
isapnp_displayCard_j8: ;tag: pnp ver...
mov edx,def:[esi]
mov esi,offset text08
call dword writeCodeStr
push edx
mov cl,2
call dword conv2hex
mov al,def:[esi+1]
mov ah,'.'
mov def:[esi],ax
call dword writeDataStr
mov esi,offset text09
call dword writeCodeStr
pop edx
shr edx,8
mov cl,2
call dword conv2hex
mov al,def:[esi+1]
mov ah,'.'
mov def:[esi],ax
call dword writeDataStr
mov esi,offset textCRLF
call dword writeCodeStr
jmp dword isapnp_displayCard_j2
isapnp_displayCard_j9: ;tag: id string...
push esi
or cl,1
isapnp_displayCard_j10:
lodsb ptr32
cmp al,13
je byte isapnp_displayCard_j12
cmp al,10
je byte isapnp_displayCard_j12
cmp al,0
je byte isapnp_displayCard_j12
isapnp_displayCard_j11:
stosb ptr32
loopd isapnp_displayCard_j10
sub eax,eax
stosd ptr32
mov esi,offset text10
call dword writeCodeStr
pop esi
call dword writeDataStr
mov esi,offset textCRLF
call dword writeCodeStr
jmp dword isapnp_displayCard_j2
isapnp_displayCard_j12:
mov al,32
jmp byte isapnp_displayCard_j11
isapnp_displayCard_j13: ;tag: unicode id string...
shr ecx,1
push ecx
push esi
or cl,1
isapnp_displayCard_j14:
lodsw ptr32
stosb ptr32
loopd isapnp_displayCard_j13
pop esi
pop ecx
jmp byte isapnp_displayCard_j9
isapnp_displayCard_j15: ;tag: logical device...
push dword def:[esi]
call dword isapnp_displayCard_j30
call dword isapnp_displayCard_j32
mov esi,offset text11
call dword writeCodeStr
mov edx,def:[DataBlock_devNum]
call dword conv2dec
call dword writeDataStr
mov esi,offset text12
call dword writeCodeStr
pop ebx
mov edi,DataBlock_freMem
call dword isapnp_decodeVendor
mov esi,DataBlock_freMem
call dword writeDataStr
mov esi,offset textCRLF
call dword writeCodeStr
jmp dword isapnp_displayCard_j2
isapnp_displayCard_j16: ;tag: compatible device...
mov ebx,def:[esi]
mov esi,offset text13
call dword writeCodeStr
mov edi,DataBlock_freMem
call dword isapnp_decodeVendor
mov esi,DataBlock_freMem
call dword writeDataStr
mov esi,offset textCRLF
call dword writeCodeStr
jmp dword isapnp_displayCard_j2
isapnp_displayCard_j17: ;tag: new configuration...
movzx byte ebp,def:[esi]
push ebp
call dword isapnp_displayCard_j30
pop ebp
mov esi,offset text14
call dword writeCodeStr
mov eax,DataBlock_curCfg
mov edx,def:[DataBlock_cfgNum]
call dword conv2dec
call dword writeDataStr
mov esi,offset text15
call dword writeCodeStr
mov edx,ebp
mov eax,DataBlock_curCfg
mov def:[eax+oneConfig_prio],edx
call dword conv2dec
call dword writeDataStr
mov esi,offset textCRLF
call dword writeCodeStr
jmp dword isapnp_displayCard_j2
isapnp_displayCard_j18: ;tag: irq resource...
movzx word ebp,def:[esi]
mov esi,offset text16
call dword writeCodeStr
bsf eax,ebp
jz byte isapnp_displayCard_j19
mov edi,DataBlock_curCfg
mov ecx,def:[edi+oneConfig_irqN]
inc dword def:[edi+oneConfig_irqN]
mov def:[edi+oneConfig_irqD+ecx*4],eax
isapnp_displayCard_j19:
bsf edx,ebp
jz byte isapnp_displayCard_j20
push edx
mov esi,offset textSPC
call dword writeCodeStr
call dword conv2dec
call dword writeDataStr
pop ecx
mov eax,1
shl eax,cl
not eax
and ebp,eax
jmp byte isapnp_displayCard_j19
isapnp_displayCard_j20:
mov esi,offset textCRLF
call dword writeCodeStr
jmp dword isapnp_displayCard_j2
isapnp_displayCard_j21: ;tag: dma resource...
movzx byte ebp,def:[esi]
mov esi,offset text17
call dword writeCodeStr
bsf eax,ebp
jz byte isapnp_displayCard_j22
mov edi,DataBlock_curCfg
mov ecx,def:[edi+oneConfig_dmaN]
inc dword def:[edi+oneConfig_dmaN]
mov def:[edi+oneConfig_dmaD+ecx*4],eax
isapnp_displayCard_j22:
bsf edx,ebp
jz byte isapnp_displayCard_j23
push edx
mov esi,offset textSPC
call dword writeCodeStr
call dword conv2dec
call dword writeDataStr
pop ecx
mov eax,1
shl eax,cl
not eax
and ebp,eax
jmp byte isapnp_displayCard_j22
isapnp_displayCard_j23:
mov esi,offset textCRLF
call dword writeCodeStr
jmp dword isapnp_displayCard_j2
isapnp_displayCard_j24: ;tag: end of logical device...
call dword isapnp_displayCard_j30
jmp dword isapnp_displayCard_j2
isapnp_displayCard_j25: ;tag: io resources...
movzx word eax,def:[esi+1]
movzx byte edx,def:[edi+6]
or eax,eax
setz cl
or ah,cl
mov edi,DataBlock_curCfg
mov ecx,def:[edi+oneConfig_prtN]
inc dword def:[edi+oneConfig_prtN]
add edi,oneConfig_prtD
mov def:[edi+ecx*8],eax
mov def:[edi+ecx*8+4],edx
mov esi,offset text18
call dword writeCodeStr
mov edx,ds:[ebp+1]
mov cl,4
call dword conv2hex
call dword writeDataStr
mov esi,offset text19
call dword writeCodeStr
mov edx,ds:[ebp+3]
mov cl,4
call dword conv2hex
call dword writeDataStr
mov esi,offset text20
call dword writeCodeStr
movzx byte edx,ds:[ebp+5]
call dword conv2dec
call dword writeDataStr
mov esi,offset text21
call dword writeCodeStr
movzx byte edx,ds:[ebp+6]
call dword conv2dec
call dword writeDataStr
mov esi,offset textCRLF
call dword writeCodeStr
jmp dword isapnp_displayCard_j2
isapnp_displayCard_j26: ;tag: fixed io resources...
movzx word eax,def:[esi+0]
movzx byte edx,def:[edi+2]
or eax,eax
setz cl
or ah,cl
mov edi,DataBlock_curCfg
mov ecx,def:[edi+oneConfig_prtN]
inc dword def:[edi+oneConfig_prtN]
add edi,oneConfig_prtD
mov def:[edi+ecx*8],eax
mov def:[edi+ecx*8+4],edx
mov esi,offset text18
call dword writeCodeStr
mov edx,ds:[ebp+0]
mov cl,4
call dword conv2hex
call dword writeDataStr
mov esi,offset text21
call dword writeCodeStr
movzx byte edx,ds:[ebp+2]
call dword conv2dec
call dword writeDataStr
mov esi,offset textCRLF
call dword writeCodeStr
jmp dword isapnp_displayCard_j2
isapnp_displayCard_j27: ;tag: mem resources...
movzx word eax,def:[esi+1]
movzx byte edx,def:[edi+7]
or eax,eax
setnz cl
dec cl
mov ch,0dh
and cl,ch
or ah,cl
shl eax,8
shl edx,8
mov edi,DataBlock_curCfg
mov ecx,def:[edi+oneConfig_memN]
inc dword def:[edi+oneConfig_memN]
add edi,oneConfig_memD
mov def:[edi+ecx*8],eax
mov def:[edi+ecx*8+4],edx
mov esi,offset text22
call dword writeCodeStr
movzx word edx,ds:[ebp+1]
shl edx,8
mov cl,8
call dword conv2hex
call dword writeDataStr
mov esi,offset text19
call dword writeCodeStr
movzx word edx,ds:[ebp+3]
shl edx,8
mov cl,8
call dword conv2hex
call dword writeDataStr
mov esi,offset text20
call dword writeCodeStr
movzx word edx,ds:[ebp+5]
call dword conv2dec
call dword writeDataStr
mov esi,offset text21
call dword writeCodeStr
movzx word edx,ds:[ebp+7]
shl edx,8
call dword conv2dec
call dword writeDataStr
mov esi,offset textCRLF
call dword writeCodeStr
jmp dword isapnp_displayCard_j2
isapnp_displayCard_j28: ;tag: mem32 resources...
mov eax,def:[esi+1]
mov edx,def:[edi+13]
or edx,80000000h
or eax,eax
setnz cl
dec cl
mov ch,0dh
and cl,ch
movzx ecx,cl
shl ecx,16
or eax,ecx
mov edi,DataBlock_curCfg
mov ecx,def:[edi+oneConfig_mewN]
inc dword def:[edi+oneConfig_mewN]
add edi,oneConfig_mewD
mov def:[edi+ecx*8],eax
mov def:[edi+ecx*8+4],edx
mov esi,offset text22
call dword writeCodeStr
mov edx,ds:[ebp+1]
mov cl,8
call dword conv2hex
call dword writeDataStr
mov esi,offset text19
call dword writeCodeStr
mov edx,ds:[ebp+5]
mov cl,8
call dword conv2hex
call dword writeDataStr
mov esi,offset text20
call dword writeCodeStr
mov edx,ds:[ebp+9]
call dword conv2dec
call dword writeDataStr
mov esi,offset text21
call dword writeCodeStr
mov edx,ds:[ebp+13]
call dword conv2dec
call dword writeDataStr
mov esi,offset textCRLF
call dword writeCodeStr
jmp dword isapnp_displayCard_j2
isapnp_displayCard_j29: ;tag: fixed mem32 resources...
mov eax,def:[esi+1]
mov edx,def:[edi+5]
or edx,80000000h
or eax,eax
setnz cl
dec cl
mov ch,0dh
and cl,ch
movzx ecx,cl
shl ecx,16
or eax,ecx
mov edi,DataBlock_curCfg
mov ecx,def:[edi+oneConfig_mewN]
inc dword def:[edi+oneConfig_mewN]
add edi,oneConfig_mewD
mov def:[edi+ecx*8],eax
mov def:[edi+ecx*8+4],edx
mov esi,offset text22
call dword writeCodeStr
mov edx,ds:[ebp+1]
mov cl,8
call dword conv2hex
call dword writeDataStr
mov esi,offset text21
call dword writeCodeStr
mov edx,ds:[ebp+5]
call dword conv2dec
call dword writeDataStr
mov esi,offset textCRLF
call dword writeCodeStr
jmp dword isapnp_displayCard_j2
isapnp_displayCard_d1:
dd 01h,offset isapnp_displayCard_j8
dd 02h,offset isapnp_displayCard_j15
dd 03h,offset isapnp_displayCard_j16
dd 04h,offset isapnp_displayCard_j18
dd 05h,offset isapnp_displayCard_j21
dd 06h,offset isapnp_displayCard_j17
dd 07h,offset isapnp_displayCard_j24
dd 08h,offset isapnp_displayCard_j25
dd 09h,offset isapnp_displayCard_j26
dd 0fh,offset isapnp_displayCard_j7
dd 81h,offset isapnp_displayCard_j27
dd 82h,offset isapnp_displayCard_j9
dd 83h,offset isapnp_displayCard_j13
dd 85h,offset isapnp_displayCard_j28
dd 86h,offset isapnp_displayCard_j29
dd 0
isapnp_displayCard_j30: ;purge current config...
mov esi,DataBlock_curCfg
mov eax,def:[esi+oneConfig_memN]
add eax,def:[esi+oneConfig_mewN]
add eax,def:[esi+oneConfig_prtN]
add eax,def:[esi+oneConfig_irqN]
add eax,def:[esi+oneConfig_dmaN]
or eax,eax
jz byte isapnp_displayCard_j31
inc dword def:[DataBlock_cfgNum]
mov eax,def:[esi]
cmp eax,def:[DataBlock_bstCfg]
jae byte isapnp_displayCard_j31
mov edi,DataBlock_bstCfg
mov esi,DataBlock_curCfg
mov ecx,oneConfig__siz
rep
  movsb ptr32
isapnp_displayCard_j31:
mov edi,DataBlock_curCfg
sub eax,eax
mov ecx,oneConfig__siz
rep
  stosb ptr32
dec ax
dec eax
mov edi,DataBlock_curCfg
mov def:[edi],eax
mov eax,def:[DataBlock_cfgNum]
mov def:[edi+oneConfig_cfgn],eax
retnd
isapnp_displayCard_j32:
mov esi,DataBlock_bstCfg
mov eax,def:[esi+oneConfig_memN]
add eax,def:[esi+oneConfig_mewN]
add eax,def:[esi+oneConfig_prtN]
add eax,def:[esi+oneConfig_irqN]
add eax,def:[esi+oneConfig_dmaN]
or eax,eax
jz byte isapnp_displayCard_j33
mov edx,def:[DataBlock_devNum]
mov def:[esi+oneConfig_devn],edx
mov ebp,esi
call dword configLogDev
inc dword def:[DataBlock_devNum]
isapnp_displayCard_j33:
sub eax,eax
mov def:[DataBlock_cfgNum],eax
mov edi,DataBlock_curCfg
mov ecx,oneConfig__siz
rep
  stosb ptr32
mov edi,DataBlock_bstCfg
mov ecx,oneConfig__siz
rep
  stosb ptr32
dec ax
mov def:[DataBlock_bstCfg],eax
dec eax
mov def:[DataBlock_curCfg],eax
retnd
endp
;-------------------------------

;-------------------------------
proc isapnp_showConfig
;in: ebp-card number...
mov eax,ebp
mov ah,al
call dword isapnp_selectCard
mov esi,offset text27
call dword writeCodeStr
mov edx,ebp
call dword conv2dec
call dword writeDataStr
mov esi,offset textCRLF
call dword writeCodeStr
sub ebp,ebp
isapnp_showConfig_j1:
mov eax,ebp
mov ah,al
call dword isapnp_selectLogDev
jnc byte isapnp_showConfig_j2
retnd
isapnp_showConfig_j2:
push ebp
mov al,30h                      ;activation register...
call dword isapnp_read
and al,1
jz dword isapnp_showConfig_j3
mov esi,offset text28
call dword writeCodeStr
mov edx,ebp
call dword conv2dec
call dword writeDataStr
mov esi,offset text29
call dword writeCodeStr
;write memory...
mov bl,40h
mov ecx,4
isapnp_showConfig_j4:
mov al,bl
call dword isapnp_read
push eax
lea ax,def:[ebx+1]
call dword isapnp_read
pop edx
movzx eax,al
mov ah,dl
shl eax,8
or eax,eax
jz byte isapnp_showConfig_j5
push ecx
push ebx
mov edx,eax
mov esi,offset textSPC
call dword writeCodeStr
mov cl,8
call dword conv2hex
call dword writeDataStr
pop ebx
pop ecx
isapnp_showConfig_j5:
add bl,8
loopd isapnp_showConfig_j4
;write mem32...
mov bl,76h
mov ecx,4
isapnp_showConfig_j6:
lea ax,def:[ebx+3]
call dword isapnp_read
push eax
lea ax,def:[ebx+2]
call dword isapnp_read
push eax
lea ax,def:[ebx+1]
call dword isapnp_read
push eax
lea ax,def:[ebx+0]
call dword isapnp_read
shl eax,8
pop edx
or al,dl
shl eax,8
pop edx
or al,dl
shl eax,8
pop edx
or al,dl
or eax,eax
jz byte isapnp_showConfig_j7
push ecx
push ebx
mov edx,eax
mov esi,offset textSPC
call dword writeCodeStr
mov cl,8
call dword conv2hex
call dword writeDataStr
pop ebx
pop ecx
isapnp_showConfig_j7:
add bl,16
loopd isapnp_showConfig_j6
;write io...
mov esi,offset text30
call dword writeCodeStr
mov bl,60h
mov ecx,8
isapnp_showConfig_j8:
mov al,bl
call dword isapnp_read
inc ebx
push eax
mov al,bl
call dword isapnp_read
inc ebx
pop edx
movzx eax,al
mov ah,dl
movzx edx,ax
or edx,edx
jz byte isapnp_showConfig_j9
push ecx
push ebx
mov esi,offset textSPC
call dword writeCodeStr
mov cl,4
call dword conv2hex
call dword writeDataStr
pop ebx
pop ecx
isapnp_showConfig_j9:
loopd isapnp_showConfig_j8
;write irq...
mov esi,offset text31
call dword writeCodeStr
mov bl,70h
mov ecx,2
isapnp_showConfig_j10:
mov al,bl
call dword isapnp_read
inc ebx
inc ebx
and al,0fh
movzx edx,al
or edx,edx
jz byte isapnp_showConfig_j11
push ecx
push ebx
mov esi,offset textSPC
call dword writeCodeStr
call dword conv2dec
call dword writeDataStr
pop ebx
pop ecx
isapnp_showConfig_j11:
loopd isapnp_showConfig_j10
;write dma...
mov esi,offset text32
call dword writeCodeStr
mov bl,74h
mov ecx,2
isapnp_showConfig_j12:
mov al,bl
call dword isapnp_read
inc ebx
and al,07h
movzx edx,al
test dl,03h
jz byte isapnp_showConfig_j13
push ecx
push ebx
mov esi,offset textSPC
call dword writeCodeStr
call dword conv2dec
call dword writeDataStr
pop ebx
pop ecx
isapnp_showConfig_j13:
loopd isapnp_showConfig_j12
mov esi,offset textCRLF
call dword writeCodeStr
isapnp_showConfig_j3:
pop ebp
inc ebp
cmp ebp,256
jb dword isapnp_showConfig_j1
retnd
endp
;-------------------------------




;-------------------------------
oneConfig_prio equ 000h         ;dd: priority...
oneConfig_devn equ 004h         ;dd: number of configuration...
oneConfig_cfgn equ 008h         ;dd: number of configuration...
oneConfig_memN equ 00ch         ;dd: number of memorys...
oneConfig_mewN equ 010h         ;dd: number of memory32s...
oneConfig_prtN equ 014h         ;dd: number of ports...
oneConfig_irqN equ 018h         ;dd: number of irqs...
oneConfig_dmaN equ 01ch         ;dd: number of dmas...
oneConfig_memD equ 020h         ;16*: dd:beg,size
oneConfig_mewD equ 0a0h         ;16*: dd:beg,size
oneConfig_prtD equ 120h         ;16*: dd:beg,size
oneConfig_irqD equ 1a0h         ;16*: dd:irq
oneConfig_dmaD equ 1e0h         ;16*: dd:dma
oneConfig__siz equ 220h         ;size...
;-------------------------------

;-------------------------------
DataBlock_wrtBuf equ 0000h      ;256: write buffer...
DataBlock_crdNum equ 0100h      ;dd: number of cards...
DataBlock_devNum equ 0104h      ;dd: number of logical devices...
DataBlock_cfgNum equ 0108h      ;dd: number of configuration...
DataBlock_readOn equ 010ch      ;dd: read only mode...
DataBlock_curCfg equ 0110h      ;dd: current configuration...
DataBlock_bstCfg equ 0510h      ;dd: best configuration...
DataBlock_freMem equ 0710h      ;free memory...
;-------------------------------

;-------------------------------
textSPC db ' ',0
textCRLF db 13,10,0
text01 db 'isa plug''n''play enumerator v1.0, done by Mc at ',%date,' ',%time,'.',13,10,0
text02 db 'isolating cards... ',0
text03 db ' found.',13,10,0
text04 db '=',64 dup ('-'),'=',13,10,'cardNum=',0
text05 db ' vendor=',0
text06 db ' serial=',0
text07 db 'unknown tag:',0
text08 db 'pnpVer=',0
text09 db ' vendorVer=',0
text10 db 'string=',0
text11 db 'devNum=',0
text12 db ' device=',0
text13 db '  compatible=',0
text14 db '  cfgNum=',0
text15 db ' priority=',0
text16 db '    irq=',0
text17 db '    dma=',0
text18 db '    io=',0
text19 db '..',0
text20 db ' step=',0
text21 db ' size=',0
text22 db '    mem=',0
text23 db '  configuring device #',0
text24 db ' to config #',0
text25 db '...',0
text26 db ' failed!',13,10,0
text27 db 16 dup ('-'),13,10,'card: ',0
text28 db 'device: ',0
text29 db 13,10,'memory:',0
text30 db 13,10,'io:',0
text31 db 13,10,'irq:',0
text32 db 13,10,'dma:',0
text33 db 'detecting cards... ',0
text34 db ' found.',13,10,0
text35 db 'working in ',0
text36 db ' mode.',13,10,0
text37 dd offset text38,offset text39
text38 db 'configurator',0
text39 db 'read-only',0
;-------------------------------

lastbyte:
