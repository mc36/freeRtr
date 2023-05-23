;-------------------------------
devCls_head:
dd 1,offset devCls_store0,offset devCls_store1
dd 2,offset devCls_netwk0,offset devCls_netwk1
dd 3,offset devCls_dsply0,offset devCls_dsply1
dd 4,offset devCls_media0,offset devCls_media1
dd 5,offset devCls_memry0,offset devCls_memry1
dd 6,offset devCls_bridg0,offset devCls_bridg1
dd 7,offset devCls_commt0,offset devCls_commt1
dd 8,offset devCls_systm0,offset devCls_systm1
dd 9,offset devCls_input0,offset devCls_input1
dd 10,offset devCls_dockn0,offset devCls_dockn1
dd 11,offset devCls_prcsr0,offset devCls_prcsr1
dd 12,offset devCls_serbs0,offset devCls_serbs1
devCls_none dd -1
devCls_other db 'other',0
;-------------------------------
devCls_store0 db 'storage',0
devCls_store1:
dd 0,offset devCls_store2,1,offset devCls_store3,2,offset devCls_store4
dd 3,offset devCls_store5,4,offset devCls_store6
dd 80h,offset devCls_other,-1
devCls_store2 db 'scsi',0
devCls_store3 db 'ide',0
devCls_store4 db 'floppy',0
devCls_store5 db 'ipi',0
devCls_store6 db 'raid',0
;-------------------------------
devCls_netwk0 db 'network',0
devCls_netwk1:
dd 0,offset devCls_netwk2,1,offset devCls_netwk3,2,offset devCls_netwk4
dd 3,offset devCls_netwk5
dd 80h,offset devCls_other,-1
devCls_netwk2 db 'ethernet',0
devCls_netwk3 db 'tokenring',0
devCls_netwk4 db 'fddi',0
devCls_netwk5 db 'atm',0
;-------------------------------
devCls_dsply0 db 'display',0
devCls_dsply1:
dd 0,offset devCls_dsply2,1,offset devCls_dsply3,2,offset devCls_dsply4
dd 80h,offset devCls_other,-1
devCls_dsply2 db 'vga',0
devCls_dsply3 db 'svga',0
devCls_dsply4 db 'xvga',0
;-------------------------------
devCls_media0 db 'multimedia',0
devCls_media1:
dd 0,offset devCls_media2,1,offset devCls_media3
dd 80h,offset devCls_other,-1
devCls_media2 db 'video',0
devCls_media3 db 'audio',0
;-------------------------------
devCls_memry0 db 'memory',0
devCls_memry1:
dd 0,offset devCls_memry2,1,offset devCls_memry3
dd 80h,offset devCls_other,-1
devCls_memry2 db 'ram',0
devCls_memry3 db 'flash',0
;-------------------------------
devCls_bridg0 db 'bridge',0
devCls_bridg1:
dd 0,offset devCls_bridg2,1,offset devCls_bridg3,2,offset devCls_bridg4
dd 3,offset devCls_bridg5,4,offset devCls_bridg6,5,offset devCls_bridg7
dd 6,offset devCls_bridg8,7,offset devCls_bridg9
dd 80h,offset devCls_other,-1
devCls_bridg2 db 'cpu',0
devCls_bridg3 db 'isa',0
devCls_bridg4 db 'eisa',0
devCls_bridg5 db 'microchannel',0
devCls_bridg6 db 'pci',0
devCls_bridg7 db 'pcmcia',0
devCls_bridg8 db 'nubus',0
devCls_bridg9 db 'cardbus',0
;-------------------------------
devCls_commt0 db 'communication',0
devCls_commt1:
dd 0,offset devCls_commt2,1,offset devCls_commt3
dd 80h,offset devCls_other,-1
devCls_commt2 db 'serial',0
devCls_commt3 db 'paralell',0
;-------------------------------
devCls_systm0 db 'system',0
devCls_systm1:
dd 0,offset devCls_systm2,1,offset devCls_systm3,2,offset devCls_systm4
dd 3,offset devCls_systm5
dd 80h,offset devCls_other,-1
devCls_systm2 db 'pic',0
devCls_systm3 db 'dma',0
devCls_systm4 db 'timer',0
devCls_systm5 db 'rtc',0
;-------------------------------
devCls_input0 db 'input',0
devCls_input1:
dd 0,offset devCls_input2,1,offset devCls_input3,2,offset devCls_input4
dd 80h,offset devCls_other,-1
devCls_input2 dd 'keyboard',0
devCls_input3 dd 'digitizer',0
devCls_input4 dd 'mouse',0
;-------------------------------
devCls_dockn0 db 'docking',0
devCls_dockn1:
dd 0,offset devCls_dockn2
dd 80h,offset devCls_other,-1
devCls_dockn2 db 'generic',0
;-------------------------------
devCls_prcsr0 db 'cpu',0
devCls_prcsr1:
dd 0,offset devCls_prcsr2,1,offset devCls_prcsr3,2,offset devCls_prcsr4
dd 3,offset devCls_prcsr5,10h,offset devCls_prcsr6,40h,offset devCls_prcsr7
dd 80h,offset devCls_other,-1
devCls_prcsr2 db 'intel-386',0
devCls_prcsr3 db 'intel-486',0
devCls_prcsr4 db 'intel-pentium',0
devCls_prcsr5 db 'intel-pentiumPro',0
devCls_prcsr6 db 'dec-alpha',0
devCls_prcsr7 db 'coprocessor',0
;-------------------------------
devCls_serbs0 db 'serialBus',0
devCls_serbs1:
dd 0,offset devCls_serbs2,1,offset devCls_serbs3,2,offset devCls_serbs4
dd 3,offset devCls_serbs5,4,offset devCls_serbs6
dd 80h,offset devCls_other,-1
devCls_serbs2 db 'firewire',0
devCls_serbs3 db 'ACCESS.bus',0
devCls_serbs4 db 'SSA',0
devCls_serbs5 db 'USB',0
devCls_serbs6 db 'fiberChannel',0
;-------------------------------
