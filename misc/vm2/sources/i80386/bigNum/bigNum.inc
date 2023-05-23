;-------------------------------
CommandList_beg:
dd offset command00,offset command01,offset command02,offset command03
dd offset command04,offset command05,offset command06,offset command07
dd offset command08,offset command09,offset command10,offset command11
dd offset command12,offset command13,offset command14,offset command15
dd offset command16,offset command17,offset command18,offset command19
dd offset command20,offset command21,offset command22,offset command23
dd offset command24,offset command25,offset command26
CommandList_end:
;-------------------------------

;-------------------------------
proc command00
mov eax,BigNumberSize
stosd ptr32
retnd
endp
;-------------------------------

;-------------------------------
proc command01
lodsd ptr32
mov edi,dataBlock_num01
call bigNum_load
mov edi,dataBlock_num01
call bigNum_trunc
mov edi,DataBlock_freMem
mov esi,dataBlock_num01
call dword command01_j1
retnd
command01_j1:
mov ecx,BigNumberSize
shr ecx,2
rep
  movsd ptr32
retnd
command01_j2:
mov eax,ebp
sub eax,esi
stosw
jmp byte command01_j1
endp
;-------------------------------

;-------------------------------
proc command02
mov edi,dataBlock_num01
call dword command01_j1
mov edi,dataBlock_num01
call bigNum_abs
mov edi,DataBlock_freMem
mov esi,dataBlock_num01
call dword command01_j1
retnd
endp
;-------------------------------

;-------------------------------
proc command03
mov edi,dataBlock_num01
call dword command01_j1
mov edi,dataBlock_num01
call bigNum_neg
mov edi,DataBlock_freMem
mov esi,dataBlock_num01
call dword command01_j1
retnd
endp
;-------------------------------

;-------------------------------
proc command04
mov edi,dataBlock_num01
call dword command01_j1
mov edi,dataBlock_num02
call dword command01_j1
mov esi,dataBlock_num01
mov edi,dataBlock_num02
call bigNum_comp
mov edi,DataBlock_freMem
movzx eax,al
stosd ptr32
retnd
endp
;-------------------------------

;-------------------------------
proc command05
lodsd ptr32
push eax
mov edi,dataBlock_num01
call dword command01_j1
pop ecx
mov esi,dataBlock_num01
mov edi,dataBlock_num02
call bigNum_shl
mov edi,dataBlock_num02
call bigNum_trunc
mov edi,DataBlock_freMem
mov esi,dataBlock_num02
call dword command01_j1
retnd
endp
;-------------------------------

;-------------------------------
proc command06
lodsd ptr32
push eax
mov edi,dataBlock_num01
call dword command01_j1
pop ecx
mov esi,dataBlock_num01
mov edi,dataBlock_num02
call bigNum_shr
mov edi,dataBlock_num02
call bigNum_trunc
mov edi,DataBlock_freMem
mov esi,dataBlock_num02
call dword command01_j1
retnd
endp
;-------------------------------

;-------------------------------
proc command07
mov edi,dataBlock_num01
call dword command01_j1
mov edi,dataBlock_num01
call bigNum_not
mov edi,DataBlock_freMem
mov esi,dataBlock_num01
call dword command01_j1
retnd
endp
;-------------------------------

;-------------------------------
proc command08
mov edi,dataBlock_num01
call dword command01_j1
mov edi,dataBlock_num02
call dword command01_j1
mov esi,dataBlock_num01
mov edi,dataBlock_num02
mov ebp,dataBlock_num03
call bigNum_or
mov edi,dataBlock_num03
call bigNum_trunc
mov edi,DataBlock_freMem
mov esi,dataBlock_num03
call dword command01_j1
retnd
endp
;-------------------------------

;-------------------------------
proc command09
mov edi,dataBlock_num01
call dword command01_j1
mov edi,dataBlock_num02
call dword command01_j1
mov esi,dataBlock_num01
mov edi,dataBlock_num02
mov ebp,dataBlock_num03
call bigNum_xor
mov edi,dataBlock_num03
call bigNum_trunc
mov edi,DataBlock_freMem
mov esi,dataBlock_num03
call dword command01_j1
retnd
endp
;-------------------------------

;-------------------------------
proc command10
mov edi,dataBlock_num01
call dword command01_j1
mov edi,dataBlock_num02
call dword command01_j1
mov esi,dataBlock_num01
mov edi,dataBlock_num02
mov ebp,dataBlock_num03
call bigNum_and
mov edi,dataBlock_num03
call bigNum_trunc
mov edi,DataBlock_freMem
mov esi,dataBlock_num03
call dword command01_j1
retnd
endp
;-------------------------------

;-------------------------------
proc command11
mov edi,dataBlock_num01
call dword command01_j1
mov edi,dataBlock_num02
call dword command01_j1
mov esi,dataBlock_num01
mov edi,dataBlock_num02
mov ebp,dataBlock_num03
call bigNum_add
mov edi,dataBlock_num03
call bigNum_trunc
mov edi,DataBlock_freMem
mov esi,dataBlock_num03
call dword command01_j1
retnd
endp
;-------------------------------

;-------------------------------
proc command12
mov edi,dataBlock_num01
call dword command01_j1
mov edi,dataBlock_num02
call dword command01_j1
mov esi,dataBlock_num01
mov edi,dataBlock_num02
mov ebp,dataBlock_num03
call bigNum_sub
mov edi,dataBlock_num03
call bigNum_trunc
mov edi,DataBlock_freMem
mov esi,dataBlock_num03
call dword command01_j1
retnd
endp
;-------------------------------

;-------------------------------
proc command13
mov edi,dataBlock_num01
call dword command01_j1
mov edi,dataBlock_num02
call dword command01_j1
mov esi,dataBlock_num01
mov edi,dataBlock_num02
mov ebp,dataBlock_num03
call bigNum_mul
mov edi,dataBlock_num03
call bigNum_trunc
mov edi,DataBlock_freMem
mov esi,dataBlock_num03
call dword command01_j1
retnd
endp
;-------------------------------

;-------------------------------
proc command14
mov edi,dataBlock_num01
call dword command01_j1
mov edi,dataBlock_num02
call dword command01_j1
mov esi,dataBlock_num01
mov edi,dataBlock_num02
mov ebp,dataBlock_num03
mov ebx,dataBlock_num04
mov ecx,dataBlock_num05
call bigNum_div
mov edi,dataBlock_num03
call bigNum_trunc
mov edi,dataBlock_num04
call bigNum_trunc
mov edi,DataBlock_freMem
mov esi,dataBlock_num03
call dword command01_j1
mov esi,dataBlock_num04
call dword command01_j1
retnd
endp
;-------------------------------

;-------------------------------
proc command15
mov edi,dataBlock_num01
call dword command01_j1
mov edi,dataBlock_num02
call dword command01_j1
mov esi,dataBlock_num01
mov edi,dataBlock_num02
mov ebp,dataBlock_num03
mov eax,dataBlock_num04
mov ebx,dataBlock_num05
call bigNum_power
mov edi,DataBlock_freMem
mov esi,dataBlock_num03
call dword command01_j1
retnd
endp
;-------------------------------

;-------------------------------
proc command16
mov edi,dataBlock_num01
call dword command01_j1
mov edi,dataBlock_num02
call dword command01_j1
mov esi,dataBlock_num01
mov edi,dataBlock_num02
mov ebp,dataBlock_num03
mov eax,dataBlock_num04
mov ebx,dataBlock_num05
mov ecx,dataBlock_num06
call bigNum_root
mov edi,DataBlock_freMem
mov esi,dataBlock_num03
call dword command01_j1
retnd
endp
;-------------------------------

;-------------------------------
proc command17
mov edi,dataBlock_num01
call dword command01_j1
mov edi,dataBlock_num02
call dword command01_j1
mov edi,dataBlock_num03
call dword command01_j1
mov esi,dataBlock_num01
mov edi,dataBlock_num02
mov edx,dataBlock_num03
mov ebp,dataBlock_num04
mov eax,dataBlock_num05
mov ebx,dataBlock_num06
mov ecx,dataBlock_num07
call bigNum_powMod
mov edi,DataBlock_freMem
mov esi,dataBlock_num04
call dword command01_j1
retnd
endp
;-------------------------------

;-------------------------------
proc command18
mov edi,dataBlock_num01
call dword command01_j1
mov edi,dataBlock_num02
call dword command01_j1
mov esi,dataBlock_num01
mov edi,dataBlock_num02
mov ebp,dataBlock_num03
mov eax,dataBlock_num04
mov ebx,dataBlock_num05
mov ecx,dataBlock_num06
mov edx,dataBlock_num07
call bigNum_invMod
mov edi,DataBlock_freMem
mov esi,dataBlock_num03
call dword command01_j1
retnd
endp
;-------------------------------

;-------------------------------
proc command19
mov edi,dataBlock_num01
call dword command01_j1
mov esi,dataBlock_num01
mov ebp,dataBlock_num02
mov eax,dataBlock_num03
mov ebx,dataBlock_num04
call bigNum_writeLSB
mov edi,DataBlock_freMem
mov esi,dataBlock_num02
lodsw ptr32
movzx ecx,ax
rep
  movsb ptr32
retnd
endp
;-------------------------------

;-------------------------------
proc command20
mov edi,dataBlock_num01
call dword command01_j1
mov esi,dataBlock_num01
mov ebp,dataBlock_num02
mov eax,dataBlock_num03
mov ebx,dataBlock_num04
call bigNum_writeMSB
mov edi,DataBlock_freMem
mov esi,dataBlock_num02
lodsw ptr32
movzx ecx,ax
rep
  movsb ptr32
retnd
endp
;-------------------------------

;-------------------------------
proc command21
mov edi,dataBlock_num01
call dword command01_j2
mov esi,dataBlock_num01
mov ebp,dataBlock_num02
mov eax,dataBlock_num03
mov ebx,dataBlock_num04
call bigNum_readLSB
mov edi,DataBlock_freMem
mov esi,dataBlock_num02
call dword command01_j1
retnd
endp
;-------------------------------

;-------------------------------
proc command22
mov edi,dataBlock_num01
call dword command01_j2
mov esi,dataBlock_num01
mov ebp,dataBlock_num02
mov eax,dataBlock_num03
mov ebx,dataBlock_num04
call bigNum_readMSB
mov edi,DataBlock_freMem
mov esi,dataBlock_num02
call dword command01_j1
retnd
endp
;-------------------------------

;-------------------------------
proc command23
mov edi,dataBlock_num01
call dword command01_j1
mov edi,dataBlock_num01
call bigNum_bitSize
mov edi,DataBlock_freMem
stosd ptr32
retnd
endp
;-------------------------------

;-------------------------------
proc command24
mov edi,dataBlock_num01
call dword command01_j1
mov edi,dataBlock_num01
call bigNum_bitUsed
mov edi,DataBlock_freMem
stosd ptr32
retnd
endp
;-------------------------------

;-------------------------------
proc command25
mov edi,dataBlock_num01
call dword command01_j1
mov edi,dataBlock_num01
mov cx,BigNumberSize
shr cx,2
call bigNum_padup
mov edi,dataBlock_num01
call bigNum_trunc
mov edi,DataBlock_freMem
mov esi,dataBlock_num01
call dword command01_j1
retnd
endp
;-------------------------------

;-------------------------------
proc command26
mov edi,dataBlock_num01
call dword command01_j1
mov edi,dataBlock_num02
call dword command01_j1
mov esi,dataBlock_num01
mov edi,dataBlock_num02
mov ebp,dataBlock_num03
mov eax,dataBlock_num04
mov ebx,dataBlock_num05
mov ecx,dataBlock_num06
mov edx,dataBlock_num07
call bigNum_gcd
mov edi,DataBlock_freMem
mov esi,dataBlock_num03
call dword command01_j1
retnd
endp
;-------------------------------
