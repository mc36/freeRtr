compASMn bootSec-disk
chmode bootSec-disk.code 1
renOW bootSec-disk.code \system\filesystem\bootSec-disk.code

compASMn bootSec-grub
chmode bootSec-grub.code 1
renOW bootSec-grub.code \system\filesystem\bootSec-grub.code

compASMn partLoad
chmode partLoad.code 1
renOW partLoad.code \system\filesystem\partLoad.code

compASM partMan
chmode partMan.code 1
renOW partMan.code \system\filesystem\partMan.code

compPAS instBootSec
chmode instBootSec.code $44
renOW instBootSec.code \system\filesystem\installBootSector.code

compPAS instPartMan
chmode instPartMan.code $44
renOW instPartMan.code \system\filesystem\installPartitionManager.code

compPAS instKernel
renOW instKernel.code \system\kernel\installKernel.code

compASMn unCompress
chmode unCompress.code 9
renOW unCompress.code \system\kernel\unCompress.code

compASMn bootSelect
chmode bootSelect.code 9
renOW bootSelect.code \system\kernel\bootSelect.code

compASMn runAnyPlace
chmode runAnyPlace.code 9
renOW runAnyPlace.code \system\kernel\runAnyPlace.code
