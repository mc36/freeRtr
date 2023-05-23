
mov ax,cs
mov ds,ax
mov es,ax

mov si,offset txt01
call write

mov eax,0
cpuid
mov di,offset lastbyte
mov eax,ebx
stosd
mov eax,edx
stosd
mov eax,ecx
stosd
sub eax,eax
stosd

mov si,offset txt02
call write
mov si,offset lastbyte
call write

mov eax,1
cpuid
mov def:[data1],eax
mov def:[data2],ecx
mov def:[data3],edx

mov si,offset txt03
call write
mov eax,def:[data1]
shr eax,8
and eax,0fh
imul eax,100
add eax,86
call conv2dec
call write
mov si,offset txt04
call write
mov eax,def:[data1]
shr eax,4
and eax,0fh
call conv2dec
call write
mov si,offset txt04
call write
mov eax,def:[data1]
and eax,0fh
call conv2dec
call write
mov si,offset txt06
call write
mov eax,def:[data1]
shr eax,12
and eax,03h
call conv2dec
call write
mov si,offset txt05
call write
mov eax,def:[data1]
shr eax,20
and eax,0ffh
imul eax,100
add eax,86
call conv2dec
call write
mov si,offset txt04
call write
mov eax,def:[data1]
shr eax,16
and eax,0fh
call conv2dec
call write
mov si,offset txt04
call write
mov eax,def:[data1]
and eax,0fh
call conv2dec
call write
mov si,offset txt07
call write
mov si,offset txtCRLF
call write

mov di,offset lastbyte
mov eax,80000002h
cpuid
stosd
mov eax,ebx
stosd
mov eax,ecx
stosd
mov eax,edx
stosd
mov eax,80000003h
cpuid
stosd
mov eax,ebx
stosd
mov eax,ecx
stosd
mov eax,edx
stosd
mov eax,80000004h
cpuid
stosd
mov eax,ebx
stosd
mov eax,ecx
stosd
mov eax,edx
stosd
sub eax,eax
stosd
mov si,offset txt12
call write
mov si,offset lastbyte
call write
mov si,offset txtCRLF
call write

mov si,offset txtCRLF
call write

mov ebx,def:[data3]
mov si,offset txt08
call wrtBits

mov ebx,def:[data2]
mov si,offset txt09
call wrtBits


sub eax,eax
clts
dw 00h
;----------------------------


;----------------------------
txtCRLF db 13,10,0
txt01 db 'cpuid v1.0, done by Mc at ',%date,' ',%time,'.',13,10,13,10,0
txt02 db 'cpu: ',0
txt03 db ' ',0
txt04 db '.',0
txt05 db ' (',0
txt06 db '-',0
txt07 db ')',0
txt08:
      db 'feature information bits: ',0
      db 31,'PBE ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ ³³³³³³³³³ ³³³³³³³³³ ³³³³³³³³³³',0
      db 29,'TM ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ³³³³³³³³ ³³³³³³³³³ ³³³³³³³³³³',0
      db 28,'HTT ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ³³³³³³³ ³³³³³³³³³ ³³³³³³³³³³',0
      db 27,'SS ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ³³³³³³ ³³³³³³³³³ ³³³³³³³³³³',0
      db 26,'SSE2 ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ³³³³³ ³³³³³³³³³ ³³³³³³³³³³',0
      db 25,'SSE ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ³³³³ ³³³³³³³³³ ³³³³³³³³³³',0
      db 24,'FXSR ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ³³³ ³³³³³³³³³ ³³³³³³³³³³',0
      db 23,'MMX ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ³³ ³³³³³³³³³ ³³³³³³³³³³',0
      db 22,'ACPI ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ³ ³³³³³³³³³ ³³³³³³³³³³',0
      db 21,'DS ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ ³³³³³³³³³ ³³³³³³³³³³',0
      db 19,'CFLSH ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ³³³³³³³³ ³³³³³³³³³³',0
      db 18,'PSN ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ³³³³³³³ ³³³³³³³³³³',0
      db 17,'PSE ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ³³³³³³ ³³³³³³³³³³',0
      db 16,'PAT ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ³³³³³ ³³³³³³³³³³',0
      db 15,'CMOV ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ³³³³ ³³³³³³³³³³',0
      db 14,'MCA ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ³³³ ³³³³³³³³³³',0
      db 13,'PGE ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ³³ ³³³³³³³³³³',0
      db 12,'MTRR ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ³ ³³³³³³³³³³',0
      db 11,'SEP ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ ³³³³³³³³³³',0
      db  9,'APIC ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ³³³³³³³³³',0
      db  8,'CX8 ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ³³³³³³³³',0
      db  7,'MCE ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ³³³³³³³',0
      db  6,'PAE ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ³³³³³³',0
      db  5,'MSR ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ³³³³³',0
      db  4,'TSC ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ³³³³',0
      db  3,'PSE ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ³³³',0
      db  2,'DE ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ³³',0
      db  1,'VME ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ³',0
      db  0,'FPU ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ',0
      db 255
txt09:
      db 'extended feature bits: ',0
      db 10,'CNTXID ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ ³³  ³³  ³',0
      db  8,'TM2 ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ³  ³³  ³',0
      db  7,'EST ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ  ³³  ³',0
      db  4,'DSCPL ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ³  ³',0
      db  3,'MONITOR ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ  ³',0
      db  0,'SSE3 ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ',0
      db 255
txt10 db '-û'
txt11 db '  ',0
txt12 db 'txt: ',0
;----------------------------



;----------------------------
proc wrtBits
;in: cs:si-offset of data block...
;    ebx-bit mapped value...
call write
call skip
mov eax,ebx
push si
call conv2bin
call write
mov si,offset txtCRLF
call write
pop si
wrtBits_j1:
lodsb
cmp al,32
jae byte wrtBits_j2
mov cl,al
mov eax,1
shl eax,cl
and eax,ebx
setnz al
push si
movzx si,al
mov al,def:[txt10+si]
mov si,offset txt11
mov def:[si],al
call write
pop si
call write
call skip
push si
mov si,offset txtCRLF
call write
pop si
jmp byte wrtBits_j1
wrtBits_j2:
mov si,offset txtCRLF
call write
ret
endp
;----------------------------


;----------------------------
proc skip
;in: cs:si-what to write...
skip_j1:
lodsb
or al,al
jnz byte skip_j1
ret
endp
;----------------------------


;----------------------------
proc conv2bin
;in:  eax-number to write...
;out: cs:si-text where asciiz is..
push ecx
push dx
push di
mov di,offset buffer
mov ecx,80000000h
conv2bin_j1:
test eax,ecx
setnz dl
add dl,'0'
mov def:[di],dl
inc di
shr ecx,1
or ecx,ecx
jnz byte conv2bin_j1
sub ax,ax
stosw
pop di
pop dx
pop ecx
mov si,offset buffer
ret
endp
;----------------------------

include vesautils.inc

align 10h
;----------------------------
data1 dd ?
data2 dd ?
data3 dd ?
data4 dd ?
buffer db 128 dup (?)
;----------------------------

lastbyte:
