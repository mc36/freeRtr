org 0h
use32
db 'exec'                       ;id
dd offset lastbyte              ;size
dd 4096                         ;data
dd 2048                         ;stack
;-------------------------------


;-------------------------------
mov esi,offset text01
call dword writeCodeStr

clts                            ;get uptime info...
dd 2ah
shl edx,2
mov def:[dataSeg_ChkInt],edx

mov al,1
clts                            ;switch io accessible mode...
dd 04h

sub eax,eax
mov def:[dataSeg_keySiz],eax
mov def:[dataSeg_shifts],eax
mov def:[dataSeg_tmpSiz],eax
mov def:[dataSeg_tmpReq],eax
mov def:[dataSeg_ackDat],eax
mov def:[dataSeg_numBuf],eax
mov def:[dataSeg_pipeln],eax

mov edi,dataSeg_wrtBuf
clts                            ;get process parameters...
dd 13h
mov esi,dataSeg_wrtBuf
lodsb ptr32
or al,al
jnz byte init_j1
call dword keyboardFlush
call dword keyboardReset
call dword keyboardFlush
call dword keyboardSpeedy
init_j1:
call dword keyboardFlush

mov al,1
mov esi,offset irqHandler
lea edi,def:[esp-512]
clts                            ;hook irq line...
dd 05h
or ebx,ebx
jnz dword vege


clts                            ;start listening...
dd 14h
or ebx,ebx
jnz dword vege

mov edi,dataSeg_wrtBuf
sub eax,eax
stosd ptr32
mov esi,dataSeg_wrtBuf
mov ecx,4
clts                            ;write to console...
dd 20h

main_j1:
mov eax,def:[dataSeg_pipeln]
clts                            ;close pipeline side...
dd 18h
main_j2:
call dword keyboardTest
clts                            ;give away the control...
dd 01h
clts                            ;get next incoming pipeline...
dd 16h
or ebx,ebx
jnz byte main_j2
mov def:[dataSeg_pipeln],eax
clts                            ;get pipeline info...
dd 19h
or ebx,ebx
jnz byte main_j1
mov def:[dataSeg_proces],eax
mov edi,dataSeg_freMem
clts                            ;get other process name...
dd 0bh
test dl,40h
jz byte main_j1
main_j3:
call dword keyboardTest
mov eax,def:[dataSeg_pipeln]
clts                            ;get pipeline info...
dd 19h
or eax,eax
jz byte main_j1
clts                            ;give away the control...
dd 01h
mov eax,def:[dataSeg_keySiz]
or eax,eax
jz byte main_j3
mov eax,def:[dataSeg_pipeln]
mov ecx,def:[dataSeg_keySiz]
mov esi,dataSeg_keyBuf
clts                            ;nonblocking send through pipeline...
dd 1ah
or ebx,ebx
jnz byte main_j3
mov dword def:[dataSeg_keySiz],0
mov eax,def:[dataSeg_proces]
clts                            ;give away the control...
dd 02h
jmp byte main_j3

vege:
sub eax,eax
clts                            ;terminate process...
dd 00h
;-------------------------------


;-------------------------------
proc keyboardTest
clts                            ;get uptime info...
dd 2bh
mov ecx,eax
sub eax,def:[dataSeg_lastTm]
cmp eax,def:[dataSeg_ChkInt]
jb byte keyboardTest_j1
mov def:[dataSeg_lastTm],ecx
in al,64h
and al,11b
jz byte keyboardTest_j1
call dword keyboardFlush
keyboardTest_j1:
retnd
endp
;-------------------------------

;-------------------------------
proc keyboardRead
;out: readed value...
push ecx
mov ecx,1024
keyboardRead_j1:
in al,64h
and al,1
jnz byte keyboardRead_j2
loopd keyboardRead_j1
keyboardRead_j2:
pop ecx
in al,60h
retnd
endp
;-------------------------------

;-------------------------------
proc keyboardFlush
mov ecx,1024
keyboardFlush_j1:
in al,64h
in al,60h
loopd keyboardFlush_j1
retnd
endp
;-------------------------------

;-------------------------------
proc keyboardReset
clts                            ;get uptime info...
dd 2ah
lea ebp,def:[ecx+edx*4]
mov al,0ffh
out 60h,al
keyboardReset_j1:
clts                            ;get uptime info...
dd 2bh
sub eax,ebp
jns byte keyboardReset_j2
call dword keyboardRead
cmp al,0aah
je byte keyboardReset_j2
jmp byte keyboardReset_j1
keyboardReset_j2:
retnd
endp
;-------------------------------

;-------------------------------
proc keyboardSpeedy
sub eax,eax
mov def:[dataSeg_ackDat],eax
mov al,0f3h
out 60h,al
retnd
endp
;-------------------------------

;-------------------------------
proc keyboardSendLeds
mov al,0edh
out 60h,al
mov eax,def:[dataSeg_shifts]
shr eax,8
mov def:[dataSeg_ackDat],eax
retnd
endp
;-------------------------------




;-------------------------------
proc PutOneChar
;in: ax-char code...
or ax,ax
jz byte PutOneChar_vege
mov ecx,def:[dataSeg_shifts]
shl ecx,4
shr cl,4
or cl,ch
and cl,7h
or ah,cl
test ah,80h
jnz byte PutOneChar_j1
mov cl,al
or cl,20h
cmp cl,'a'
jb byte PutOneChar_j1
cmp cl,'z'
ja byte PutOneChar_j1
mov ecx,def:[dataSeg_shifts]
test ecx,400h
setnz cl
shl cl,5
xor al,cl
test ah,6
jz byte PutOneChar_j1
or al,20h
PutOneChar_j1:
mov ecx,def:[dataSeg_keySiz]
cmp ecx,1024
ja byte PutOneChar_vege
mov def:[dataSeg_keyBuf+ecx],ax
add dword def:[dataSeg_keySiz],2
PutOneChar_vege:
sub eax,eax
mov def:[dataSeg_tmpSiz],eax
mov def:[dataSeg_tmpReq],eax
retnd
endp
;-------------------------------

;-------------------------------
proc irqHandler
mov ecx,1
irqHandler_j1:
dec ecx
js byte irqHandler_j2
push ecx
call dword irqCore
clts                            ;get number of missed irqs...
dd 4ch
pop ecx
add ecx,eax
jmp byte irqHandler_j1
irqHandler_j2:
clts                            ;get uptime info...
dd 2bh
mov def:[dataSeg_lastTm],eax
clts                            ;terminate irq handler...
dd 07h
endp
;-------------------------------



;-------------------------------
proc irqCore
call dword keyboardRead
mov def:[dataSeg_lastCd],al
mov def:[dataSeg_savdSP],esp

mov ecx,def:[dataSeg_tmpReq]
or ecx,ecx
jz byte irqCore_j1
mov edx,def:[dataSeg_tmpSiz]
mov def:[dataSeg_tmpBuf+edx],al
inc edx
mov def:[dataSeg_tmpSiz],edx
cmp edx,ecx
jb dword irqCore_vege
mov eax,def:[dataSeg_tmpOfs]
jmp eax
irqCore_j1:
mov byte def:[dataSeg_tmpSiz],1
mov def:[dataSeg_tmpBuf],al
cmp al,224
je dword irqCore_j2
cmp al,225
je dword irqCore_j3
mov cl,10h                      ;left shift...
cmp al,42
je dword irqCore_j6
cmp al,170
je dword irqCore_j7
mov cl,20h                      ;left ctrl...
cmp al,29
je dword irqCore_j6
cmp al,157
je dword irqCore_j7
mov cl,40h                      ;left alt...
cmp al,56
je dword irqCore_j6
cmp al,184
je dword irqCore_j14
mov cl,01h                      ;right shift...
cmp al,54
je dword irqCore_j6
cmp al,182
je dword irqCore_j7
cmp al,250                      ;ack...
je dword irqCore_j10
cmp al,254                      ;done...
je dword irqCore_vege
mov cl,1
cmp al,70                       ;scroll lock...
je dword irqCore_j11
cmp al,198
je dword irqCore_vege
mov cl,2
cmp al,69                       ;num lock...
je dword irqCore_j11
cmp al,197
je dword irqCore_vege
mov cl,4
cmp al,58                       ;caps lock...
je dword irqCore_j11
cmp al,186
je dword irqCore_vege
mov ecx,8002h                   ;tabulator...
cmp al,15
je dword irqCore_put
cmp al,143
je dword irqCore_vege
mov ecx,8003h                   ;backspace...
cmp al,14
je dword irqCore_put
cmp al,142
je dword irqCore_vege
mov ecx,8004h                   ;enter...
cmp al,28
je dword irqCore_put
cmp al,156
je dword irqCore_vege
mov ecx,8005h                   ;escape...
cmp al,1
je dword irqCore_put
cmp al,129
je dword irqCore_vege
mov ecx,801eh                   ;f11...
cmp al,87
je dword irqCore_put
cmp al,215
je dword irqCore_vege
mov ecx,801fh                   ;f12...
cmp al,88
je dword irqCore_put
cmp al,216
je dword irqCore_vege
mov ecx,'*'                     ;gray *...
cmp al,55
je dword irqCore_put
cmp al,183
je dword irqCore_vege
mov ecx,' '                     ;space...
cmp al,57
je dword irqCore_put
cmp al,185
je dword irqCore_vege
mov ecx,def:[dataSeg_shifts]
and cl,44h
jz byte irqCore_j15
cmp al,71
jb byte irqCore_j15
cmp al,82
ja byte irqCore_j15
movzx ebx,al
sub bl,71
movzx byte ebx,cs:[data07+ebx]
cmp bl,80h
ja byte irqCore_j15
mov eax,10
mul dword def:[dataSeg_numBuf]
add eax,ebx
mov def:[dataSeg_numBuf],eax
jmp byte irqCore_vege
irqCore_j15:

mov esi,offset data01           ;gray numbers...
call dword irqCore_table
mov esi,offset data02           ;123...
call dword irqCore_table
mov esi,offset data03           ;qwerty...
call dword irqCore_table
mov esi,offset data04           ;asdf...
call dword irqCore_table
mov esi,offset data05           ;zxcv...
call dword irqCore_table
mov esi,offset data06           ;f1-f10...
call dword irqCore_table

jmp dword irqCore_bad
irqCore_vege:
mov eax,def:[dataSeg_savdSP]
mov esp,eax
retnd
irqCore_put:
movzx eax,cx
call dword PutOneChar
jmp byte irqCore_vege
irqCore_table:
cmp al,cs:[esi+0]
jb byte irqCore_j12
cmp al,cs:[esi+1]
ja byte irqCore_j12
sub al,cs:[esi+0]
mov ebx,def:[dataSeg_shifts]
and bx,cs:[esi+2]
setnz bl
movzx ebx,bl
mov esi,cs:[esi+4+ebx*4]
movzx ebx,al
movzx word ecx,cs:[esi+ebx*2]
jmp byte irqCore_put
irqCore_j12:
mov ah,al
and ah,7fh
cmp ah,cs:[esi+0]
jb byte irqCore_j13
cmp ah,cs:[esi+1]
ja byte irqCore_j13
jmp byte irqCore_vege
irqCore_j13:
retnd
irqCore_j2:                     ;224...
mov def:[dataSeg_tmpBuf],al
mov byte def:[dataSeg_tmpReq],2
mov byte def:[dataSeg_tmpSiz],1
mov dword def:[dataSeg_tmpOfs],offset irqCore_j4
jmp dword irqCore_vege
irqCore_j3:                     ;225...
mov def:[dataSeg_tmpBuf],al
mov byte def:[dataSeg_tmpReq],3
mov byte def:[dataSeg_tmpSiz],1
mov dword def:[dataSeg_tmpOfs],offset irqCore_j5
jmp dword irqCore_vege
irqCore_j4:                     ;224...
mov byte def:[dataSeg_tmpReq],0
mov esi,dataSeg_tmpBuf
mov al,def:[esi+1]
mov cl,02h                      ;right ctrl...
cmp al,29
je dword irqCore_j6
cmp al,157
je dword irqCore_j7
mov cl,04h                      ;right alt...
cmp al,56
je dword irqCore_j6
cmp al,184
je dword irqCore_j14
mov ecx,8004h                   ;enter...
cmp al,28
je dword irqCore_put
cmp al,156
je dword irqCore_vege
mov ecx,8006h                   ;insert...
cmp al,82
je dword irqCore_put
cmp al,210
je dword irqCore_vege
mov ecx,8007h                   ;delete...
cmp al,83
je dword irqCore_put
cmp al,211
je dword irqCore_vege
mov ecx,8008h                   ;home...
cmp al,71
je dword irqCore_put
cmp al,199
je dword irqCore_vege
mov ecx,8009h                   ;home...
cmp al,79
je dword irqCore_put
cmp al,207
je dword irqCore_vege
mov ecx,800ah                   ;pgup...
cmp al,73
je dword irqCore_put
cmp al,201
je dword irqCore_vege
mov ecx,800bh                   ;pgdn...
cmp al,81
je dword irqCore_put
cmp al,209
je dword irqCore_vege
mov ecx,800ch                   ;up...
cmp al,72
je dword irqCore_put
cmp al,200
je dword irqCore_vege
mov ecx,800dh                   ;down...
cmp al,80
je dword irqCore_put
cmp al,208
je dword irqCore_vege
mov ecx,800eh                   ;left...
cmp al,75
je dword irqCore_put
cmp al,203
je dword irqCore_vege
mov ecx,800fh                   ;right...
cmp al,77
je dword irqCore_put
cmp al,205
je dword irqCore_vege
mov ecx,8010h                   ;printscreen...
cmp al,55
je dword irqCore_put
cmp al,183
je dword irqCore_vege
cmp al,42
je dword irqCore_vege
cmp al,170
je dword irqCore_vege
mov ecx,8012h                   ;start-left...
cmp al,91
je dword irqCore_put
cmp al,219
je dword irqCore_vege
mov ecx,8013h                   ;menu...
cmp al,93
je dword irqCore_put
cmp al,221
je dword irqCore_vege
mov ecx,8032h                   ;start-right...
cmp al,92
je dword irqCore_put
cmp al,220
je dword irqCore_vege
mov ecx,8033h                   ;wake-up...
cmp al,99
je dword irqCore_put
cmp al,227
je dword irqCore_vege
mov ecx,8034h                   ;sleep...
cmp al,95
je dword irqCore_put
cmp al,223
je dword irqCore_vege
mov ecx,8035h                   ;power...
cmp al,94
je dword irqCore_put
cmp al,222
je dword irqCore_vege
mov ecx,'/'                     ;gray /...
cmp al,53
je dword irqCore_put
cmp al,181
je dword irqCore_vege
cmp al,54                       ;right shift+gray /...
je dword irqCore_vege
cmp al,182
je dword irqCore_vege


jmp byte irqCore_bad
irqCore_j5:                     ;225...
mov byte def:[dataSeg_tmpReq],0
mov esi,dataSeg_tmpBuf
mov ax,def:[esi+1]
mov ecx,8011h                   ;break...
cmp ax,0451dh
je dword irqCore_put
cmp ax,0c59dh
je dword irqCore_vege


jmp dword irqCore_bad
irqCore_j6:                     ;set shift status...
or def:[dataSeg_shifts],cl
jmp dword irqCore_vege
irqCore_j7:                     ;clear shift status...
not cl
and def:[dataSeg_shifts],cl
jmp dword irqCore_vege
irqCore_j14:
push ecx
push dword def:[dataSeg_shifts]
mov dword def:[dataSeg_shifts],0
movzx byte eax,def:[dataSeg_numBuf]
call dword PutOneChar
sub eax,eax
mov def:[dataSeg_numBuf],eax
pop dword def:[dataSeg_shifts]
pop ecx
jmp byte irqCore_j7
irqCore_bad:
mov esi,offset text03
call dword writeCodeStr
mov ecx,def:[dataSeg_tmpSiz]
mov esi,dataSeg_tmpBuf
irqCore_j9:
lodsb ptr32
push esi
push ecx
movzx edx,al
call dword conv2dec
call dword writeDataStr
mov esi,offset text02
call dword writeCodeStr
pop ecx
pop esi
loopd irqCore_j9
mov esi,offset textCRLF
call dword writeCodeStr
jmp dword irqCore_vege
irqCore_j10:
sub eax,eax
xchg eax,def:[dataSeg_ackDat]
out 60h,al
jmp dword irqCore_vege
irqCore_j11:
movzx eax,cl
shl eax,8
xor def:[dataSeg_shifts],eax
call dword keyboardSendLeds
jmp dword irqCore_vege
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
mov edi,dataSeg_wrtBuf
writeCodeStr_j1:
inc ecx
lodsb cs,ptr32
stosb ptr32
or al,al
jnz byte writeCodeStr_j1
dec ecx
mov esi,dataSeg_wrtBuf
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
proc conv2dec
;in:  edx-value to write...
;out: esi-where converted...
mov esi,offset conv2dec_d1
mov edi,dataSeg_wrtBuf
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
mov esi,dataSeg_wrtBuf
retnd
conv2dec_d1:
dd 1000000000,0,100000000,10000000,1000000,0,100000,10000,1000,0,100,10,1
conv2dec_d2:
endp
;-------------------------------

;-------------------------------
data01:                         ;gray (numpad) numbers/arrows...
db 71,83                        ;lower/upper limit...
dw 200h                         ;depends on numlock...
dd offset data01_d0,offset data01_d1
data01_d0 dw 8008h,800ch,800ah,'-',800eh,0,800fh,'+'
          dw 8009h,800dh,800bh,8006h,8007h
data01_d1 dw '789-456+1230.'

data02:                         ;123...
db 2,13                         ;lower/upper limit...
dw 11h                          ;depends on shift...
dd offset data02_d0,offset data02_d1
data02_d0 dw '1234567890-='
data02_d1 dw '!@#$%^&*()_+'

data03:                         ;qwerty...
db 16,27                        ;lower/upper limit...
dw 11h                          ;depends on shift...
dd offset data03_d0,offset data03_d1
data03_d0 dw 'qwertyuiop[]'
data03_d1 dw 'QWERTYUIOP{}'

data04:                         ;asdf...
db 30,41                        ;lower/upper limit...
dw 11h                          ;depends on shift...
dd offset data04_d0,offset data04_d1
data04_d0 dw 'asdfghjkl;',39,96
data04_d1 dw 'ASDFGHJKL:"~'

data05:                         ;zxcv...
db 43,53                        ;lower/upper limit...
dw 11h                          ;depends on shift...
dd offset data05_d0,offset data05_d1
data05_d0 dw '\zxcvbnm,./'
data05_d1 dw '|ZXCVBNM<>?'

data06:                         ;f1-f10...
db 59,68                        ;lower/upper limit...
dw 00h                          ;depends on nothing...
dd offset data06_d0,offset data06_d0
data06_d0 dw 8014h,8015h,8016h,8017h,8018h,8019h,801ah,801bh,801ch,801dh

data07 db 7,8,9,255,4,5,6,255,1,2,3,0
;-------------------------------

;-------------------------------
textCRLF db 13,10,0
text01 db 'keyboard driver v1.0, done by Mc at ',%date,' ',%time,'.',13,10,0
text02 db ' ',0
text03 db 'unknown scancode: ',0
;-------------------------------

;-------------------------------
dataSeg_wrtBuf equ 000h         ;256: write buffer...
dataSeg_keyBuf equ 100h         ;1k: keyboard buffer...
dataSeg_keySiz equ 500h         ;dd: bytes in key buffer...
dataSeg_shifts equ 504h         ;dd: shift status: left:x0, right:0x; 1=shift, 2=ctrl, 4=alt, 100h=scroll, 200h=num, 400=caps...
dataSeg_tmpBuf equ 508h         ;16: incoming buffer #1 char...
dataSeg_tmpSiz equ 518h         ;dd: bytes in incoming buffer...
dataSeg_tmpReq equ 51ch         ;dd: bytes in incoming buffer...
dataSeg_tmpOfs equ 520h         ;dd: bytes in incoming buffer...
dataSeg_ackDat equ 524h         ;dd: send after an ack...
dataSeg_numBuf equ 528h         ;dd: number pad buffer...
dataSeg_pipeln equ 52ch         ;dd: pipeline number...
dataSeg_proces equ 530h         ;dd: pipeline number...
dataSeg_savdSP equ 534h         ;dd: saved esp value...
dataSeg_lastCd equ 538h         ;db: last keyboard data...
dataSeg_lastTm equ 53ch         ;dd: time of last keyboard irq...
dataSeg_ChkInt equ 540h         ;dd: ticks between tests..
dataSeg_freMem equ 544h         ;free memory...
;-------------------------------

lastbyte:
