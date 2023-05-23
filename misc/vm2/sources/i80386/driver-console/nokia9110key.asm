org 0h
use32
db 'exec'                       ;id
dd offset lastbyte              ;size
dd 2048                         ;data
dd 2048                         ;stack
;-------------------------------


;-------------------------------
mov eax,1
clts                            ;switch io accessible mode...
dd 04h

sub eax,eax
mov def:[dataSeg_keySiz],eax
mov def:[dataSeg_shifts],eax
mov def:[dataSeg_pipeln],eax

clts                            ;start listening...
dd 14h
or ebx,ebx
jnz dword vege

mov edi,dataSeg_freMem
sub eax,eax
stosd ptr32
mov esi,dataSeg_freMem
mov ecx,4
clts                            ;write to console...
dd 20h

main_j1:
mov eax,def:[dataSeg_pipeln]
clts                            ;close pipeline side...
dd 18h
main_j2:
call dword releq2keyboard
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
call dword releq2keyboard
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
proc releq2keyboard
sub eax,eax
in al,64h
and al,1
jnz byte releq2keyboard_j1
retnd
releq2keyboard_j1:
in al,60h
mov def:[dataSeg_lastCd],al
sub ecx,ecx
cmp al,73
je dword releq2keyboard_light
inc ecx
cmp al,81
je dword releq2keyboard_light
mov cl,1
cmp al,42
je dword releq2keyboard_press
mov cl,2
cmp al,29
je dword releq2keyboard_press
mov cl,4
cmp al,56
je dword releq2keyboard_press
mov cl,1
cmp al,170
je dword releq2keyboard_reles
mov cl,2
cmp al,157
je dword releq2keyboard_reles
mov cl,4
cmp al,184
je dword releq2keyboard_reles

mov esi,offset releq2keyboard_ctrl
call dword releq2keyboard_tabl

mov esi,offset releq2keyboard_num1
mov edi,offset releq2keyboard_num2
mov cl,2
mov ch,11
call dword releq2keyboard_sequ

mov esi,offset releq2keyboard_qwe1
mov edi,offset releq2keyboard_qwe2
mov cl,16
mov ch,25
call dword releq2keyboard_sequ

mov esi,offset releq2keyboard_asd1
mov edi,offset releq2keyboard_asd2
mov cl,30
mov ch,38
call dword releq2keyboard_sequ

mov esi,offset releq2keyboard_zxc1
mov edi,offset releq2keyboard_zxc2
mov cl,44
mov ch,50
call dword releq2keyboard_sequ

mov esi,offset releq2keyboard_spc1
mov dl,def:[dataSeg_shifts]
and dl,1
jz byte releq2keyboard_j2
mov esi,offset releq2keyboard_spc2
releq2keyboard_j2:
call dword releq2keyboard_tabl

jmp dword releq2keyboard
;------------------------------- apply shift to dx...
releq2keyboard_shift:
or dh,def:[dataSeg_shifts]
releq2keyboard_store:           ;just store dx...
mov eax,def:[dataSeg_keySiz]
and eax,03ffh
mov def:[eax],dx
inc eax
inc eax
mov def:[dataSeg_keySiz],eax
jmp dword releq2keyboard
;------------------------------- switch light...
releq2keyboard_light:
mov al,0a8h
out 22h,al
in al,23h
and al,0feh
and cl,1
or al,cl
out 23h,al
jmp dword releq2keyboard
;------------------------------- press shift+ctrl+alt...
releq2keyboard_press:
and cl,7
or def:[dataSeg_shifts],cl
jmp dword releq2keyboard
;------------------------------- release shift+ctrl+alt...
releq2keyboard_reles:
and cl,7
mov ch,def:[dataSeg_shifts]
or ch,cl
xor ch,cl
mov def:[dataSeg_shifts],ch
jmp dword releq2keyboard
;-------------------------------
releq2keyboard_ctrl:
dw 57,32                        ;space
dw 1,8005h                      ;escape
dw 89,8005h                     ;escape
dw 15,8002h                     ;tab
dw 58,8002h                     ;tab
dw 13,8003h                     ;backspace
dw 28,8004h                     ;enter
dw 72,800ch                     ;up
dw 80,800dh                     ;down
dw 75,800eh                     ;left
dw 77,800fh                     ;right
dw 63,8014h                     ;f1
dw 64,8015h                     ;f2
dw 65,8016h                     ;f3
dw 66,8017h                     ;f4
dw 67,8018h                     ;f5
dw 68,8019h                     ;f6
dw 87,801ah                     ;f7
dw 88,801bh                     ;f8
dw 62,801ch                     ;f9
dw 61,801dh                     ;f10
dw 60,801eh                     ;f11
dw 59,801fh                     ;f12
dw 0,0
;-------------------------------
releq2keyboard_spc1:
dw 14,'/'
dw 39,'\'
dw 51,';'
dw 52,39
dw 41,','
dw 53,'.'
dw 0,0
;-------------------------------
releq2keyboard_spc2:
dw 14,'?'
dw 39,'|'
dw 51,':'
dw 52,'"'
dw 41,'<'
dw 53,'>'
dw 0,0
;-------------------------------
releq2keyboard_num1 db '1234567890'
releq2keyboard_num2 db '!@#$%^&*()'
releq2keyboard_qwe1 db 'qwertyuiop'
releq2keyboard_qwe2 db 'QWERTYUIOP'
releq2keyboard_asd1 db 'asdfghjkl'
releq2keyboard_asd2 db 'ASDFGHJKL'
releq2keyboard_zxc1 db 'zxcvbnm'
releq2keyboard_zxc2 db 'ZXCVBNM'
;------------------------------- lookup in table located at esi...
releq2keyboard_tabl:
movzx word ecx,cs:[esi]
movzx word edx,cs:[esi+2]
add esi,4
or edx,edx
jz byte releq2keyboard_tabl1
cmp ecx,eax
jne byte releq2keyboard_tabl
pop eax
jmp dword releq2keyboard_shift
releq2keyboard_tabl1:
retnd
;------------------------------- lookup in sequence at esi, from cl to ch...
releq2keyboard_sequ:
cmp al,cl
jb byte releq2keyboard_sequ1
cmp al,ch
ja byte releq2keyboard_sequ1
movzx edx,al
sub dl,cl
add esi,edx
add edi,edx
mov dl,def:[dataSeg_shifts]
and dl,1
jz byte releq2keyboard_sequ2
mov esi,edi
releq2keyboard_sequ2:
movzx byte edx,cs:[esi]
jmp dword releq2keyboard_shift
releq2keyboard_sequ1:
retnd
;-------------------------------
endp
;-------------------------------





;-------------------------------
dataSeg_keyBuf equ 000h         ;1k: keyboard buffer...
dataSeg_keySiz equ 400h         ;dd: bytes in key buffer...
dataSeg_shifts equ 404h         ;db: shift status: 1=shift, 2=ctrl, 4=alt...
dataSeg_lastCd equ 405h         ;db: last keyboard data...
dataSeg_pipeln equ 408h         ;dd: pipeline number...
dataSeg_proces equ 40ch         ;dd: pipeline number...
dataSeg_freMem equ 410h         ;free memory...
;-------------------------------

lastbyte:
