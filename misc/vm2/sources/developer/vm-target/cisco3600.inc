memorySize equ 67108864
restartOfs equ 0bfc00000h

uartRxData equ 0be840000h
uartRxStat equ 0be840028h
uartRxMask equ 01h
uartRxNeed equ 01h

uartTxData equ 0be840000h
uartTxStat equ 0be840028h
uartTxMask equ 20h
uartTxNeed equ 20h
