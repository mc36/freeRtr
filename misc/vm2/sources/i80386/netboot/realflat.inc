;-----------------------------------
proc realflat
mov bp,offset realflat_mem48
mov byte cs:[bp],16
mov ax,cs
shl eax,4
mov bx,offset realflat_gdt
movzx ebx,bx
add eax,ebx
mov cs:[bp+2],eax
lgdt word cs:[bp]              ;load global descriptor table address
mov bx,08h                     ;load bx to point to GDT entry 1
push ds
cli                            ;disable interrupts
mov eax,cr0                    ;switch to protected mode
or eax,1
mov cr0,eax
db 0ebh,00h                    ;clear executionpipe
mov gs,bx                      ;load segment shadow-registers
mov fs,bx                      ;with GDT entry 1 (4GB segment limit)
mov es,bx
mov ds,bx
and al,0FEh                    ;switch back to real-mode without
mov cr0,eax                    ;resetting the CPU
dt 0ebh,00h                    ;clear executionpipe
sti                            ;enable interrupts
pop ds
ret
realflat_mem48 db 0,0,0,0,0,0
realflat_gdt db 0,0,0,0,0,0,0,0                  ;GDT ntr0 (null seg)
             db 0ffh,0ffh,0,0,0,092h,0cfh,0ffh   ;GDT ntr1 (seg 0,lim 4gb)
endp
;-----------------------------------



;-----------------------------------
proc enablea20                 ;hardware enable gate A20
cli                            ;disable interrupts
mov bp,1000
xor ax,ax                      ;set A20 test segments 0 and 0ffffh
mov fs,ax
dec ax
mov gs,ax
enablea20_j1:
call enablea20_test            ;is A20 already enabled?
jz byte enablea20_done         ;if yes, done
mov dx,92h                     ;PS/2 A20 enable
in al,dx
or al,2
db 0ebh,00h
db 0ebh,00h
db 0ebh,00h
out dx,al
dec bp
jns byte enablea20_j1
enablea20_done:
sti                            ;enable interrupts
ret
enablea20_test:                ;test for enabled A20
mov al,fs:[0]                  ;get byte from 0:0
mov ah,al                      ;preserve old byte
not al                         ;modify byte
xchg al,gs:[10h]               ;put modified byte to 0ffffh:10h
cmp ah,fs:[0]                  ;set zero if byte at 0:0 not modified
mov gs:[10h],al                ;put back old byte at 0ffffh:10h
ret                            ;return, zero if A20 enabled
endp
;-----------------------------------
