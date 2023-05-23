use32                           ;not a question...
;-------------------------------

;------------------------------- set ownership of a page...
proc memmap_alloc
;in: cr3-linear mapping...
;    ds,es-4gb data...
;    eax-physical offset...
;    ebx-process number...
push eax
shr eax,10                      ;one entry is 4 bytes long...
and al,0fch
add eax,cs:[MemoryUsageMap]
mov def:[eax],ebx
pop eax
retnd
endp
;-------------------------------

;------------------------------- count process memory usage...
proc memmap_count
;in:  cr3-linear mapping...
;     ds,es-4gb data...
;     ebx-process number...
;out: edx-pages allocated...
push ecx
push esi
mov esi,cs:[MemoryUsageMap]
mov ecx,cs:[MemorySizePages]
sub edx,edx
memmap_count_j1:
lodsd ptr32
sub eax,ebx
jne byte memmap_count_j2
inc edx
memmap_count_j2:
loopd memmap_count_j1
pop esi
pop ecx
retnd
endp
;-------------------------------

;------------------------------- find memory address...
proc memmap_find
;in:  cr3-linear mapping...
;     ds,es-4gb data...
;out: carry-cleared if successful...
;     eax-free memory offset, 0=error...
push esi
push ecx
mov eax,cs:[MemoryUsageMap]
mov ecx,cs:[MemorySizePages]
lea esi,def:[eax+ecx*4]
memmap_find_j1:
sub esi,4
mov eax,def:[esi]
or eax,eax
jz byte memmap_find_j3
loopd memmap_find_j1
sub eax,eax
stc
memmap_find_j2:
pop ecx
pop esi
retnd
memmap_find_j3:
mov eax,esi
sub eax,cs:[MemoryUsageMap]
shl eax,10
clc
jmp byte memmap_find_j2
endp
;-------------------------------

;------------------------------- kill one process's all memory...
proc memmap_kill
;in: cr3-linear mapping...
;    ds,es-4gb data...
;    ebx-process number...
push esi
push ecx
push eax
mov esi,cs:[MemoryUsageMap]
mov ecx,cs:[MemorySizePages]
memmap_kill_j1:
lodsd ptr32
sub eax,ebx
jnz byte memmap_kill_j2
mov def:[esi-4],eax
memmap_kill_j2:
loopd memmap_kill_j1
pop eax
pop ecx
pop esi
retnd
endp
;-------------------------------

;------------------------------- find next continuous pages...
proc memmap_fndcnt
;in:  cr3-linear mapping...
;     ds,es-4gb data...
;     eax-offset where begin...
;     ecx-block size in pages...
;out: carry-cleared if successful...
;     eax-free block offset, 0=error...
push esi
push edi
push ebx
push edx
mov edx,eax
mov esi,eax
shr esi,10
and esi,0fffffffch
mov edi,cs:[MemoryUsageMap]
mov ebx,cs:[MemorySizePages]
add esi,edi
lea edi,def:[edi+ebx*4]
memmap_fndcnt_j1:
sub ebx,ebx
memmap_fndcnt_j2:
cmp esi,edi
jae byte memmap_fndcnt_err
lodsd ptr32
or eax,eax
jnz byte memmap_fndcnt_j1
inc ebx
cmp ebx,ecx
jb byte memmap_fndcnt_j2
sub esi,cs:[MemoryUsageMap]
shr esi,2
sub esi,ecx
shl esi,12
mov edx,esi
clc
memmap_fndcnt_vege:
mov eax,edx
pop edx
pop ebx
pop edi
pop esi
retnd
memmap_fndcnt_err:
stc
jmp byte memmap_fndcnt_vege
endp
;-------------------------------



;------------------------------- write page table entry...
proc pagtab_write
;in:  cr3-linear mapping...
;     ds,es-4gb data...
;     edx-page table offset...
;     esi-physical offset...
;     edi-logical offset...
;     ebx-process number...
;out: carry-cleared if successful...
push eax
push edx
mov eax,edi
shr eax,20
and eax,0ffch
add edx,eax
mov eax,def:[edx]
or eax,eax
jnz byte pagtab_write_j1
call dword memmap_find
jc byte pagtab_write_err
call dword memmap_alloc
push eax
push ecx
push edi
mov edi,eax
sub eax,eax
mov ecx,1024
rep
  stosd ptr32
pop edi
pop ecx
pop eax
mov al,7
mov def:[edx],eax
pagtab_write_j1:
and ax,0f000h
mov edx,edi
shr edx,10
and edx,0ffch
add edx,eax
mov eax,esi
and ah,0f0h
mov al,7
mov def:[edx],eax
clc
pagtab_write_vege:
pop edx
pop eax
retnd
pagtab_write_err:
stc
jmp byte pagtab_write_vege
endp
;-------------------------------

;------------------------------- erase page table entry...
proc pagtab_erase
;in:  cr3-linear mapping...
;     ds,es-4gb data...
;     edx-page table offset...
;     edi-logical offset...
;out: carry-cleared if successful...
;     esi-physical offset...
push eax
push edx
push ebx
mov ebx,edi
shr ebx,20
and ebx,0ffch
add ebx,edx
mov eax,def:[ebx]
and eax,0fffff000h
jz byte pagtab_erase_err
mov edx,edi
shr edx,10
and edx,0ffch
add edx,eax
mov esi,def:[edx]
test esi,1
jz byte pagtab_erase_err
and esi,0fffff000h
mov dword def:[edx],0
push edi
push ecx
push eax
mov edi,eax
mov ecx,1024
sub eax,eax
repe
  scasd ptr32
pop eax
pop ecx
pop edi
jnz byte pagtab_erase_ok
mov dword def:[ebx],0
sub ebx,ebx
call dword memmap_alloc
pagtab_erase_ok:
clc
pagtab_erase_vege:
pop ebx
pop edx
pop eax
retnd
pagtab_erase_err:
sub esi,esi
stc
jmp byte pagtab_erase_vege
endp
;-------------------------------

;------------------------------- write more page table entries...
proc pagtab_writeMore
;in:  cr3-linear mapping...
;     ds,es-4gb data...
;     edx-page table offset...
;     esi-physical offset...
;     edi-logical offset...
;     ebx-process number...
;     ecx-pages to write...
;out: carry-cleared if successful...
push ecx
push esi
push edi
or ecx,ecx
jz byte pagtab_writeMore_ok
pagtab_writeMore_j1:
call dword pagtab_write
jc byte pagtab_writeMore_vege
add esi,4096
add edi,4096
loopd pagtab_writeMore_j1
pagtab_writeMore_ok:
clc
pagtab_writeMore_vege:
pop edi
pop esi
pop ecx
retnd
endp
;-------------------------------

;------------------------------- create new page table...
proc pagtab_create
;in:  cr3-linear mapping...
;     ds,es-4gb data...
;     ebx-process number...
;out: carry-cleared if successful...
;     edx-page table offset...
push eax
push ecx
push edi
sub edx,edx
call dword memmap_find
jc byte pagtab_create_vege
call dword memmap_alloc
mov edi,eax
mov edx,eax
mov ecx,1023
sub eax,eax
rep
  stosd ptr32
mov ecx,cs:[LinearPagingTab]
mov eax,def:[ecx+4092]
stosd ptr32
clc
pagtab_create_vege:
pop edi
pop ecx
pop eax
retnd
endp
;-------------------------------



;------------------------------- append one entry to a list...
proc listing_append
;in:  cr3-linear mapping...
;     ds,es-4gb data...
;     ebp-offset of list...
;out: carry-cleared if successful...
;     edi-physical offset of new entry...
;     cr3-list mapping...
push eax
push ecx
push edx
push ebx
push esi
mov eax,cs:[LinearPagingTab]
mov cr3,eax
mov ecx,cs:[ebp+0]
mov eax,cs:[ebp+4]
imul eax,ecx
cmp eax,ProcessDataSiz
ja byte listing_append_err2
add eax,0fffh
mov edi,eax
add ecx,eax
shr eax,12
shr ecx,12
cmp eax,ecx
je byte listing_append_j1
call dword memmap_find
jc byte listing_append_err2
mov esi,eax
mov ebx,KernelProcNum
call dword memmap_alloc
mov edx,cs:[ebp+8]
call dword pagtab_write
jc byte listing_append_err1
listing_append_j1:
mov eax,sel_KernelRW
mov es,ax
inc dword es:[ebp+4]
mov eax,sel_raw4gbMem
mov es,ax
mov eax,cs:[ebp+8]
mov cr3,eax
mov edi,cs:[ebp+4]
dec edi
mov ecx,cs:[ebp+0]
imul edi,ecx
mov esi,edi
sub eax,eax
rep
  stosb ptr32
mov edi,esi
clc
listing_append_vege:
pop esi
pop ebx
pop edx
pop ecx
pop eax
retnd
listing_append_err1:
mov eax,esi
sub ebx,ebx
call dword memmap_alloc
listing_append_err2:
stc
jmp byte listing_append_vege
endp
;-------------------------------

;------------------------------- delete one entry from a list...
proc listing_delete
;in:  cr3-linear mapping...
;     ds,es-4gb data...
;     ebp-offset of list...
;     eax-entry to delete (0..max-1)...
;out: cr3-linear mapping...
push eax
push ecx
push edx
push ebx
push esi
push edi
mov esi,cs:[ebp+4]
dec esi
cmp eax,esi
ja byte listing_delete_vege
mov ecx,sel_KernelRW
mov es,cx
dec dword es:[ebp+4]
mov ecx,sel_raw4gbMem
mov es,cx
mov ecx,cs:[ebp+0]
imul eax,ecx
imul esi,ecx
mov edi,eax
mov eax,cs:[ebp+8]
mov cr3,eax
mov edx,esi
rep
  movsb ptr32
mov eax,cs:[LinearPagingTab]
mov cr3,eax
mov eax,esi
add edx,0fffh
add esi,0fffh
shr edx,12
shr esi,12
cmp edx,esi
je byte listing_delete_vege
mov edi,eax
mov edx,cs:[ebp+8]
call dword pagtab_erase
jc byte listing_delete_vege
mov eax,esi
sub ebx,ebx
call dword memmap_alloc
listing_delete_vege:
pop edi
pop esi
pop ebx
pop edx
pop ecx
pop eax
retnd
endp
;-------------------------------
