;------------------------------- find one pipeline id...
proc pipeline_findID
;in:  t0-id of pipeline...
;out: t0-offset of descriptor, 0=error...
;     t1,t2-destroyed...
lui t1,offset FirstPipeOfs
ori t1,t1,offset FirstPipeOfs
j listing_findID
noop
endp
;-------------------------------

;------------------------------- count process pipeline usage...
proc pipeline_count
;in:  t0-id of process...
;out: t1-number of pipelines...
;     t2,t3-destroyed...
lui t1,offset FirstPipeOfs
ori t1,t1,offset FirstPipeOfs
lw t2,0,t1
or t1,r0,r0
pipeline_count_j1:
beq t2,r0,pipeline_count_j4
noop
lw t3,pipeLst_pidC,t2
beq t0,t3,pipeline_count_j3
noop
lw t3,pipeLst_pidA,t2
beq t0,t3,pipeline_count_j3
noop
pipeline_count_j2:
lw t2,pipeLst_next,t2
j pipeline_count_j1
noop
pipeline_count_j3:
addiu t1,t1,1
j pipeline_count_j2
noop
pipeline_count_j4:
jr ra
noop
endp
;-------------------------------

;------------------------------- count system pipeline usage...
proc pipeline_total
;out: t0-number of pipelines...
;     t1-destroyed...
lui t1,offset FirstPipeOfs
ori t1,t1,offset FirstPipeOfs
j listing_count
noop
endp
;-------------------------------

;------------------------------- info about a pipeline...
proc pipeline_info
;in:  t0-id of pipe...
;     t4-process who wants info...
;out: t0-process id of remote...
;     t1-bytes free in tx buffer...
;     t2-bytes waiting in rx buffer...
;     t3-status: 0=ok, 1=error...
;     t4,t5-destroyed...
or t5,r0,ra
jal pipeline_findID
noop
beq t0,r0,pipeline_info_err
noop
lw t1,pipeLst_pidC,t0
beq t1,t4,pipeline_info_j1
noop
lw t1,pipeLst_pidA,t0
beq t1,t4,pipeline_info_j2
noop
pipeline_info_err:
or t0,r0,r0
or t1,r0,r0
or t2,r0,r0
ori t3,r0,1
jr t5
noop
pipeline_info_j1:
lw t1,pipeLst_sizS,t0
lw t2,pipeLst_posS,t0
subu t1,t1,t2
lw t2,pipeLst_posR,t0
lw t0,pipeLst_pidA,t0
or t3,r0,r0
jr t5
noop
pipeline_info_j2:
lw t1,pipeLst_sizR,t0
lw t2,pipeLst_posR,t0
subu t1,t1,t2
lw t2,pipeLst_posS,t0
lw t0,pipeLst_pidC,t0
or t3,r0,r0
jr t5
noop
endp
;-------------------------------

;------------------------------- create new pipeline...
proc pipeline_create
;in:  t6-caller pid...
;     t7-answerer pid...
;     t8-size in bytes...
;     t9-block mode pipe...
;out: t0-pipeline id, 0=error...
;     t9-pipeline descriptor, 0=error...
;     tX,s0-destroyed...
or s0,r0,ra
ori t1,r0,pipeLst__siz
subu t0,t8,t1
blez t0,pipeline_create_j1
noop
or t8,r0,t1
pipeline_create_j1:
bne t8,r0,pipeline_create_j2
noop
ori t8,r0,1
pipeline_create_j2:
addiu t8,t8,1fffh
srl t8,t8,13
sll t8,t8,13
or t0,r0,t8
jal memmap_fndcnt
noop
beq t1,r0,pipeline_create_err
noop
andi t9,t9,1
sb t9,pipeLst_blck,t1
or t9,r0,t1
srl t1,t8,13
sh t1,pipeLst_size,t9
sw t0,pipeLst_phys,t9
sw t6,pipeLst_pidC,t9
sw t7,pipeLst_pidA,t9
or t7,r0,t0
sw r0,pipeLst_posS,t9
sw r0,pipeLst_posR,t9
ori t0,r0,pipeLst__hed
subu t8,t8,t0
srl t8,t8,1
sw t8,pipeLst_sizS,t9
sw t8,pipeLst_sizR,t9
sw t0,pipeLst_begS,t9
addu t0,t0,t8
sw t0,pipeLst_begR,t9
lui t1,offset FirstPipeOfs
ori t1,t1,offset FirstPipeOfs
jal listing_makeID
noop
sw t0,pipeLst_plid,t9
lhu t8,pipeLst_size,t9
pipeline_create_j3:
or t0,r0,t7
lui t1,KernelProcNum
ori t1,t1,KernelProcNum
jal memmap_alloc
noop
addiu t7,t7,2000h
addiu t8,t8,-1
bgtz t8,pipeline_create_j3
noop
or t0,r0,t9
lui t1,offset FirstPipeOfs
ori t1,t1,offset FirstPipeOfs
jal listing_append
noop
lw t0,pipeLst_plid,t9
jr s0
noop
pipeline_create_err:
or t0,r0,r0
or t9,r0,r0
jr ra
noop
endp
;-------------------------------

;------------------------------- close one side of pipeline...
proc pipeline_close
;in:  t0-id of pipe...
;     t4-process who closes...
;out: t0-status: 0=ok, 1=error...
;     t0,t1,t2,t3,t4,t5-destroyed...
or t5,r0,ra
jal pipeline_findID
noop
beq t0,r0,pipeline_close_err
noop
lw t1,pipeLst_pidC,t0
beq t1,t4,pipeline_close_j1
noop
lw t1,pipeLst_pidA,t0
beq t1,t4,pipeline_close_j2
noop
pipeline_close_err:
ori t0,r0,1
jr t5
noop
pipeline_close_done:
lw t1,pipeLst_pidC,t0
lw t2,pipeLst_pidA,t0
or t1,t1,t2
bne t1,r0,pipeline_close_ok
noop
or t4,r0,t0
lui t1,offset FirstPipeOfs
ori t1,t1,offset FirstPipeOfs
jal listing_remove
noop
lw t3,pipeLst_phys,t4
lhu t4,pipeLst_size,t4
pipeline_close_j3:
blez t4,pipeline_close_ok
noop
or t0,r0,t3
or t1,r0,r0
jal memmap_alloc
noop
addiu t3,t3,2000h
addiu t4,t4,-1
j pipeline_close_j3
noop
pipeline_close_ok:
or t0,r0,r0
jr t5
noop
pipeline_close_j1:
sw r0,pipeLst_pidC,t0
sw r0,pipeLst_begR,t0
sw r0,pipeLst_sizR,t0
sw r0,pipeLst_posR,t0
j pipeline_close_done
noop
pipeline_close_j2:
sw r0,pipeLst_pidA,t0
sw r0,pipeLst_begS,t0
sw r0,pipeLst_sizS,t0
sw r0,pipeLst_posS,t0
j pipeline_close_done
noop
endp
;-------------------------------

;------------------------------- kill process's all pipelines...
proc pipeline_kill
;in:  t9-process id...
;out: tX-destroyed...
or t8,r0,ra
pipeline_kill_j1:
lui t1,offset FirstPipeOfs
ori t1,t1,offset FirstPipeOfs
lw t1,0,t1
pipeline_kill_j2:
beq t1,r0,pipeline_kill_j4
noop
lw t0,pipeLst_pidC,t1
beq t0,t9,pipeline_kill_j3
noop
lw t0,pipeLst_pidA,t1
beq t0,t9,pipeline_kill_j3
noop
lw t1,pipeLst_next,t1
j pipeline_kill_j2
noop
pipeline_kill_j3:
lw t0,pipeLst_plid,t1
or t4,r0,t9
jal pipeline_close
noop
j pipeline_kill_j1
noop
pipeline_kill_j4:
jr t8
noop
endp
;-------------------------------

;------------------------------- send through pipeline...
proc pipeline_send
;in:  t0-id of pipe...
;     t4-process who wants send...
;     kernelBuffer-dw:size, db:data...
;out: t0-result: 0=ok, 1=error...
;     tX-destroyed...
or t9,r0,ra
jal pipeline_findID
noop
beq t0,r0,pipeline_send_err
noop
or t8,r0,t0
or t7,r0,r0
lw t0,pipeLst_pidC,t8
beq t0,t4,pipeline_send_j1
noop
ori t7,r0,4
lw t0,pipeLst_pidA,t8
beq t0,t4,pipeline_send_j1
noop
pipeline_send_err:
ori t0,r0,1
jr t9
noop
pipeline_send_j1:
addu t7,t7,t8
lui t6,KernelBufOfs
ori t6,t6,KernelBufOfs
lhu t5,0,t6
beq t5,r0,pipeline_send_ok
noop
addiu t6,t6,2
lbu t0,pipeLst_blck,t8
sll t0,t0,1
subu t6,t6,t0
addu t5,t5,t0
lw t0,pipeLst_sizS,t7
lw t1,pipeLst_posS,t7
subu t0,t0,t1
subu t0,t0,t5
bltz t0,pipeline_send_err
noop
or t0,r0,t6
lw t1,pipeLst_posS,t7
lw t2,pipeLst_begS,t7
addu t1,t1,t2
addu t1,t1,t8
or t2,r0,t5
jal copyBinary
noop
lw t0,pipeLst_posS,t7
addu t0,t0,t5
sw t0,pipeLst_posS,t7
pipeline_send_ok:
or t0,r0,r0
jr t9
noop
endp
;-------------------------------

;------------------------------- receive through pipeline...
proc pipeline_recv
;in:  t0-id of pipe...
;     t4-process who wants receive...
;     t6-max bytes to receive...
;out: t0-result: 0=ok, 1=error...
;     t1-buffer offset...
;     t2-size value...
;     kernelBuffer-dw:size, db:data...
;     tX-destroyed...
or t9,r0,ra
jal pipeline_findID
noop
beq t0,r0,pipeline_recv_err
noop
or t8,r0,t0
ori t7,r0,4
lw t0,pipeLst_pidC,t8
beq t0,t4,pipeline_recv_j1
noop
or t7,r0,r0
lw t0,pipeLst_pidA,t8
beq t0,t4,pipeline_recv_j1
noop
pipeline_recv_err:
lui t1,KernelBufOfs
ori t1,t1,KernelBufOfs
or t2,r0,r0
sw r0,0,t1
ori t0,r0,1
jr t9
noop
pipeline_recv_j1:
addu t7,t7,t8
lw t4,pipeLst_posS,t7
beq t4,r0,pipeline_recv_err
noop
blez t6,pipeline_recv_err
noop
lbu t0,pipeLst_blck,t8
beq t0,r0,pipeline_recv_j2
noop
;block mode...
lw t0,pipeLst_begS,t7
addu t0,t0,t8
lbu t1,0,t0
lbu t2,1,t0
sll t1,t1,8
or t5,t1,t2
subu t0,t6,t5
bltz t0,pipeline_recv_err
noop
or t6,r0,t5
j pipeline_recv_j3
noop
pipeline_recv_j2:
;char mode...
subu t0,t6,t4
blez t0,pipeline_recv_j3
noop
or t6,r0,t4
pipeline_recv_j3:
;copy data...
lui t1,KernelBufOfs
ori t1,t1,KernelBufOfs
sh t6,0,t1
addiu t1,t1,2
lw t0,pipeLst_begS,t7
addu t0,t0,t8
lbu t2,pipeLst_blck,t8
sll t2,t2,1
addu t0,t0,t2
or t2,r0,t6
jal copyBinary
noop
;delete bytes...
lw t2,pipeLst_posS,t7
lbu t0,pipeLst_blck,t8
sll t0,t0,1
addu t3,t0,t6
subu t2,t2,t3
sw t2,pipeLst_posS,t7
lw t1,pipeLst_begS,t7
addu t1,t1,t8
addu t0,t1,t3
jal copyBinary
noop
lui t1,KernelBufOfs
ori t1,t1,KernelBufOfs
lhu t2,0,t1
or t0,r0,r0
jr t9
noop
endp
;-------------------------------
