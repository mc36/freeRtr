;-------------------------------
proc exception_int
;in: al-interrupt number...
jmp dword opcode_int_j1
endp
;-------------------------------



;-------------------------------
proc exception13
call dword v86moni_codeReaderBegin

exception13_j1:
call dword v86moni_getCodeB     ;get the opcode...

cmp al,0cdh                     ;int opcode?
je dword opcode_int
cmp al,0cfh                     ;iret opcode?
je dword opcode_iret

cmp al,0fah                     ;cli opcode?
je dword opcode_cli
cmp al,0fbh                     ;sti opcode?
je dword opcode_sti
cmp al,09ch                     ;pushf opcode?
je dword opcode_pushf
cmp al,09dh                     ;popf opcode?
je dword opcode_popf
cmp al,0f4h                     ;hlt opcode?
je dword opcode_hlt
cmp al,0cch                     ;int3 opcode?
je dword opcode_int3
cmp al,0ceh                     ;into opcode?
je dword opcode_into

cmp al,0e4h                     ;in al,nn opcode?
je dword opcode_inalnn
cmp al,0e5h                     ;in ax,nn opcode?
je dword opcode_inaxnn
cmp al,0ech                     ;in al,dx opcode?
je dword opcode_inaldx
cmp al,0edh                     ;in ax,dx opcode?
je dword opcode_inaxdx

cmp al,0e6h                     ;out nn,al opcode?
je dword opcode_outnnal
cmp al,0e7h                     ;out nn,ax opcode?
je dword opcode_outnnax
cmp al,0eeh                     ;out dx,al opcode?
je dword opcode_outdxal
cmp al,0efh                     ;out dx,ax opcode?
je dword opcode_outdxax

cmp al,06ch                     ;insb opcode?
je dword opcode_insb
cmp al,06dh                     ;insw opcode?
je dword opcode_insw
cmp al,06eh                     ;outsb opcode?
je dword opcode_outsb
cmp al,06fh                     ;outsw opcode?
je dword opcode_outsw

cmp al,00fh                     ;opcode group 0f?
je dword opcode_grp0f

cmp al,9bh                      ;wait opcode?
je dword opcode_wait

call dword v86moni_tryPrefix    ;prefix bytes?
jnc dword exception13_j1

jmp dword v86moni_codeReaderError
endp
;-------------------------------



;-------------------------------
proc exception07
call dword v86moni_codeReaderBegin

exception07_j1:
call dword v86moni_getCodeB     ;get the opcode...
call dword v86moni_tryPrefix    ;prefix bytes?
jnc byte exception07_j1

cmp al,9bh                      ;wait opcode?
je dword opcode_wait

cmp al,0cdh                     ;int opcode?
je byte exception07_num8

and al,11111000b                ;test for valid FPU instruction...
cmp al,11011000b
jne dword v86moni_codeReaderError

call dword v86moni_getCodeB     ;get the opcode...
mov ah,al

shr al,6                        ;mod bits...
cmp al,11b                      ;11b-st(x)
je byte exception07_done
cmp al,01b                      ;01b-reg+mem8
je byte exception07_num8
cmp al,10b                      ;10b-reg+num16
je byte exception07_num16
;cmp al,00b                      ;00b-num16/reg
mov al,ah
and al,111b                     ;r/m bits...
cmp al,110b                     ;is this an num16?
je byte exception07_num16

exception07_done:
jmp dword run_j1

exception07_num8:
mov al,1                        ;skip bytes...
call dword v86moni_skipCodeB
jmp byte exception07_done

exception07_num16:
mov cl,def:[dataSeg_sawADR]
and cl,1
mov eax,2
shl eax,cl
call dword v86moni_skipCodeB
jmp byte exception07_done
endp
;-------------------------------
