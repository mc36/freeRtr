;-------------------------------
protocol_name db 'remote program loader',0
;-------------------------------
dataSeg_perMac equ 1000h        ;16: peer mac address...
dataSeg_retrys equ 1010h        ;dd: retry number...
dataSeg_positi equ 1014h        ;dd: position in stream...
dataSeg_timBeg equ 1018h        ;dd: timer began...
;-------------------------------

;-------------------------------
proc doRPL
mov si,offset doRPL_txt01
call writeCode
mov ax,32
mov def:[dataSeg_retrys],ax
doRPL_j1:
dec word def:[dataSeg_retrys]
js word doRPL_err
mov si,offset doRPL_txt02
call writeCode
mov di,dataSeg_freMem
mov si,dataSeg_parBrd
mov cx,nic_addrSize
rep
  movsb
mov si,offset doRPL_dat1beg
mov cx,offset doRPL_dat1end
call doRPL_j2
mov si,dataSeg_parAdr
mov cx,nic_addrSize
rep
  movsb
mov si,offset doRPL_dat2beg
mov cx,offset doRPL_dat2end
call doRPL_j2
call device_send
call dword timer_start
mov def:[dataSeg_timBeg],ebp
doRPL_j3:
mov ebp,def:[dataSeg_timBeg]
call dword timer_past
sub eax,5
jns byte doRPL_j1
call device_recv
or cx,cx
jz byte doRPL_j3
add si,nic_addrSize             ;skip address...
lodsw                           ;read size...
xchg al,ah
cmp ax,cx
ja byte doRPL_j1
mov cx,ax
lodsw                           ;read sap type...
cmp ax,0fcfch
jne byte doRPL_j1
lodsb                           ;read info type...
cmp al,03h
jne byte doRPL_j1
mov ax,def:[si+2]               ;read rpl type...
cmp ax,0200h
jne byte doRPL_j1
mov di,dataSeg_perMac
mov si,dataSeg_freMem
mov cx,nic_addrSize
rep
  movsb
mov si,offset doRPL_txt04
call writeCode
mov si,dataSeg_perMac
call writeMac
mov si,offset main_txtCRLF
call writeCode

mov ax,16
mov def:[dataSeg_retrys],ax
sub eax,eax
mov def:[dataSeg_positi],eax

doRPL_j4:
mov si,offset doRPL_txt05
call writeCode
mov eax,def:[dataSeg_positi]
call conv2dec
call writeCode
mov si,offset doRPL_txt06
call writeCode
mov di,dataSeg_freMem
mov si,dataSeg_perMac
mov cx,nic_addrSize
rep
  movsb
mov si,offset doRPL_dat3beg
mov cx,offset doRPL_dat3end
call doRPL_j2
mov eax,def:[dataSeg_positi]
xchg al,ah
rol eax,16
xchg al,ah
stosd
mov si,offset doRPL_dat4beg
mov cx,offset doRPL_dat4end
call doRPL_j2
mov si,dataSeg_parAdr
mov cx,nic_addrSize
rep
  movsb
mov si,offset doRPL_dat5beg
mov cx,offset doRPL_dat5end
call doRPL_j2
call device_send
call dword timer_start
mov def:[dataSeg_timBeg],ebp

doRPL_j5:
mov ebp,def:[dataSeg_timBeg]
call dword timer_past
sub eax,5
jns byte doRPL_j4
call device_recv
or cx,cx
jz byte doRPL_j5
mov di,dataSeg_perMac
mov bp,cx
mov cx,nic_addrSize             ;source mac address...
doRPL_j6:
lodsb
cmp al,def:[di]
jne byte doRPL_j5
inc di
loop doRPL_j6
lodsw                           ;length...
xchg al,ah
cmp ax,bp
ja byte doRPL_j5
mov cx,ax
lodsw
cmp ax,0fcfch                   ;sap id...
jne byte doRPL_j5
lodsb
cmp al,03h                      ;info type...
jne byte doRPL_j5
mov bp,si
lodsw                           ;length...
xchg al,ah
cmp ax,cx
ja byte doRPL_j5
add bp,ax
lodsw
cmp ax,2000h                    ;file data...
jne byte doRPL_j5
sub eax,eax
mov def:[dataSeg_tmpNm4],eax    ;flags address...
mov def:[dataSeg_tmpNm6],eax    ;size in bytes...
dec eax
mov def:[dataSeg_tmpNm1],eax    ;sequence number...
mov def:[dataSeg_tmpNm2],eax    ;target address...
mov def:[dataSeg_tmpNm3],eax    ;starter address...
mov def:[dataSeg_tmpNm5],eax    ;source address...
doRPL_j7:
cmp si,bp
jae byte doRPL_j8
lodsw                           ;read length...
xchg al,ah
mov dx,ax
sub dx,4
and dx,07ffh
lodsw                           ;read type...
mov di,si
add si,dx
cmp ax,1140h                    ;sequence header...
je byte doRPL_j9
cmp ax,14c0h                    ;loader header...
je byte doRPL_j10
cmp ax,1840h                    ;data header...
je byte doRPL_j11

jmp byte doRPL_j7
doRPL_j9:
mov eax,def:[di]
xchg al,ah
rol eax,16
xchg al,ah
mov def:[dataSeg_tmpNm1],eax    ;sequence number...
jmp byte doRPL_j7
doRPL_j10:
mov eax,def:[di]
xchg al,ah
rol eax,16
xchg al,ah
mov def:[dataSeg_tmpNm2],eax    ;target address...
mov eax,def:[di+4]
xchg al,ah
rol eax,16
xchg al,ah
mov def:[dataSeg_tmpNm3],eax    ;starter address...
movzx byte eax,def:[di+8]
mov def:[dataSeg_tmpNm4],eax    ;flags address...
jmp byte doRPL_j7
doRPL_j11:
mov def:[dataSeg_tmpNm5],di     ;source address...
mov def:[dataSeg_tmpNm6],dx     ;size in bytes...
jmp byte doRPL_j7
doRPL_j8:
mov eax,def:[dataSeg_tmpNm1]
cmp eax,def:[dataSeg_positi]
jne word doRPL_j5
call dword timer_start
mov def:[dataSeg_timBeg],ebp
inc dword def:[dataSeg_positi]
mov eax,def:[dataSeg_positi]
call conv2dec
call writeCode
mov si,offset doRPL_txt07
call writeCode
push es
mov eax,def:[dataSeg_tmpNm2]
mov di,ax
and di,0fh
shr eax,4
mov es,ax
mov si,def:[dataSeg_tmpNm5]
mov cx,def:[dataSeg_tmpNm6]
rep
  movsb
pop es
mov al,def:[dataSeg_tmpNm4]
and al,20h
jnz word doRPL_j5
mov eax,def:[dataSeg_positi]
call conv2dec
call writeCode
mov si,offset doRPL_txt08
call writeCode

mov eax,def:[dataSeg_tmpNm3]
mov def:[main_startOfs],ax
sub ax,ax
shr eax,4
mov def:[main_startSeg],eax
clc
ret
doRPL_err:
mov si,offset doRPL_txt03
call writeCode
stc
ret
doRPL_j2:
sub cx,si
rep
  movsb
ret
doRPL_dat1beg:
db 000h,032h,0fch,0fch,003h,000h,02fh,000h
db 001h,000h,008h,040h,003h,000h,000h,000h
db 000h,000h,010h,000h,008h,000h,006h,040h
db 009h,005h,059h,000h,006h,040h,00ah,000h
db 001h,000h,00ah,040h,006h
doRPL_dat1end:
doRPL_dat2beg:
db 000h,005h,040h,007h,0fch,000h,004h,000h
db 004h
doRPL_dat2end:
doRPL_dat3beg:
db 000h,032h,0fch,0fch,003h,000h,02fh,000h
db 010h,000h,008h,040h,011h
doRPL_dat3end:
doRPL_dat4beg:
db 000h,010h,000h,008h,000h,006h,040h,009h
db 005h,050h,000h,006h,040h,00ah,000h,001h
db 000h,00ah,040h,006h
doRPL_dat4end:
doRPL_dat5beg:
db 000h,005h,040h,007h,0fch,000h,004h,000h
db 004h
doRPL_dat5end:
doRPL_txt01 db 'finding rpl server..',0
doRPL_txt02 db '.',0
doRPL_txt03 db ' failed!',13,10,0
doRPL_txt04 db ' mac=',0
doRPL_txt05 db 'requesting packet #',0
doRPL_txt06 db '...',13,10,0
doRPL_txt07 db 13,0
doRPL_txt08 db ' packets received.',13,10,0
endp
;-------------------------------
