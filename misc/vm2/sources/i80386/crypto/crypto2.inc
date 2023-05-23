;------------------------------- hash starter...
proc doHasher_init
;in: eax-offset of handler...
mov def:[DataBlock_Handle],eax
mov ebp,DataBlock_contx
call eax
retnd
endp
;-------------------------------

;------------------------------- hash updater...
proc doHasher_add
;in: eax-offset of handler...
mov def:[DataBlock_Handle],eax
sub eax,eax
mov def:[DataBlock_bufPos],eax
doHasher_add_j1:
mov eax,def:[DataBlock_bufPos]
cmp eax,def:[DataBlock_bufSiz]
jae byte doHasher_add_j2
mov al,def:[DataBlock_bufDat+eax]
mov ebp,DataBlock_contx
mov ecx,def:[DataBlock_Handle]
call ecx
inc dword def:[DataBlock_bufPos]
jmp byte doHasher_add_j1
doHasher_add_j2:
retnd
endp
;-------------------------------

;------------------------------- hash finisher...
proc doHasher_finish
;in: eax-offset of handler...
mov def:[DataBlock_Handle],eax
mov ebp,DataBlock_contx
call eax
retnd
endp
;-------------------------------
