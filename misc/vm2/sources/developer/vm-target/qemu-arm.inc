;qemu-system-arm -M versatilepb -m 128 -kernel vm.elf

memorySize equ 67108864
restartOfs equ 0bfc00000h

uartRxData equ 101f1000h
uartRxStat equ 101f1018h
uartRxMask equ 10h
uartRxNeed equ 00h

uartTxData equ 101f1000h
uartTxStat equ 101f1018h
uartTxMask equ 20h
uartTxNeed equ 00h
