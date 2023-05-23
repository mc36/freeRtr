;qemu-system-ppc -M mac99 -m 128 -kernel vm.elf

memorySize equ 67108864
restartOfs equ 0bfc00000h

uartRxData equ 0f20003f8h
uartRxStat equ 0f20003fdh
uartRxMask equ 01h
uartRxNeed equ 01h

uartTxData equ 0f20003f8h
uartTxStat equ 0f20003fdh
uartTxMask equ 20h
uartTxNeed equ 20h
