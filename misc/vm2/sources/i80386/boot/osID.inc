db 000h,'Unused',0
db 001h,'DOS: FAT12',0
db 002h,'Xenix: root file system',0
db 003h,'Xenix: user file system',0
db 004h,'DOS: <32MB FAT16',0
db 005h,'DOS: extended',0
db 006h,'DOS: >32MB FAT16/BigDOS',0
db 007h,'OS/2: HPFS',0
db 007h,'WinNT: NTFS',0
db 007h,'Advanced Unix',0
db 007h,'QNX',0
db 008h,'AIX: boot',0
db 008h,'SplitDrive',0
db 008h,'OS/2: v1.0-1.3 only',0
db 008h,'Commodore DOS',0
db 008h,'DELL: partition spanning multiple drives',0
db 009h,'AIX: data',0
db 009h,'Coherent: file system',0
db 00Ah,'OS/2: Boot Manager',0
db 00Ah,'OPUS',0
db 00Ah,'Coherent: swap',0
db 00Bh,'Win95: FAT32',0
db 00Ch,'Win95: FAT32 (LBA mode)',0
db 00Eh,'DOS: VFAT16/BigDOS (LBA mode)',0
db 00Fh,'DOS: extended VFAT (LBA mode)',0
db 010h,'OPUS',0
db 011h,'OS/2: hidden FAT12',0
db 012h,'Compaq: Diagnostics',0
db 014h,'OS/2: hidden <32MB FAT16',0
db 016h,'OS/2: hidden >32MB FAT16',0
db 017h,'OS/2: hidden HPFS',0
db 017h,'WinNT: hidden NTFS',0
db 018h,'AST: win swap file',0
db 019h,'Willowtech: Photon coS',0
db 01Ch,'Win95: hidden FAT32',0
db 01Dh,'Win95: hidden FAT32 (LBA mode)',0
db 01Eh,'DOS: hidden VFAT16/BigDOS (LBA mode)',0
db 020h,'Willowsoft: Overture File System (OFS1)',0
db 021h,'FSo2',0
db 024h,'NEC DOS 3.x',0
db 038h,'TheOS v3 2GB',0
db 039h,'TheOS v4 spn',0
db 03Ah,'TheOS v4 4GB',0
db 03Bh,'TheOS v4 ext',0
db 03Ch,'PowerQuest PartitionMagic: recovery',0
db 040h,'Venix 80286',0
db 041h,'Personal RISC: boot',0
db 041h,'PowerPC: boot',0
db 042h,'Peter Gutmann: Secure File System (SFS)',0
db 045h,'Eumel/Elan',0
db 046h,'Eumel/Elan',0
db 047h,'Eumel/Elan',0
db 048h,'Eumel/Elan',0
db 04Dh,'QNX4.x',0
db 04Eh,'QNX4.x 2nd',0
db 04Fh,'QNX4.x 3rd',0
db 04Fh,'Oberon: boot/data',0
db 050h,'OnTrack Disk Manager: read-only',0
db 051h,'OnTrack Disk Manager: read/write',0
db 051h,'Novell',0
db 052h,'Microport System V/386',0
db 052h,'CP/M',0
db 053h,'OnTrack Disk Manager: write-only',0
db 054h,'OnTrack Disk Manager: DDO',0
db 055h,'EZ-Drive',0
db 056h,'GoldenBow VFeature',0
db 05Ch,'Priam EDisk',0
db 061h,'SpeedStor',0
db 063h,'Unix SysV/386',0
db 063h,'386/ix',0
db 063h,'MtXinu BSD 4.3',0
db 063h,'GNU HURD',0
db 064h,'Novell NetWare 286',0
db 064h,'SpeedStore',0
db 064h,'PC-Armour',0
db 065h,'Novell NetWare 386 (3.11)',0
db 067h,'Novell',0
db 068h,'Novell',0
db 069h,'Novell',0
db 070h,'DiskSecure Multi-Boot',0
db 075h,'IBM PC/IX',0
db 07Eh,'F.I.X.',0
db 080h,'Minix v1.1 - 1.4a',0
db 081h,'Linux',0
db 081h,'Minix v1.4b+',0
db 081h,'Mitac Advanced Disk Manager',0
db 082h,'Linux: swap',0
db 082h,'Prime',0
db 082h,'Unix: Solaris',0
db 083h,'Linux: native (ext2fs/xiafs)',0
db 084h,'OS/2: hidden DOS C:',0
db 085h,'Linux: Extended',0
db 086h,'WinNT: FAT16 volume/stripe set',0
db 087h,'OS/2: HPFS Fault-Tolerant mirror',0
db 087h,'OS/2: volume/stripe set',0
db 093h,'Amoeba: file system',0
db 094h,'Amoeba: bad block table',0
db 099h,'DCE376 log',0
db 099h,'Mylex EISA SCSI',0
db 0A0h,'IBM ThinkPad',0
db 0A0h,'Phoenix NoteBIOS: Power Management: Save-to-Disk',0
db 0A5h,'FreeBSD',0
db 0A5h,'BSD/386',0
db 0A6h,'OpenBSD',0
db 0A7h,'NextStep',0
db 0A9h,'NetBSD',0
db 0AAh,'Olivetti Fat12',0
db 0B7h,'BSDI: file system (secondarily swap)',0
db 0B8h,'BSDI: swap (secondarily file system)',0
db 0BEh,'Solaris: boot',0
db 0C0h,'CTOS',0
db 0C0h,'Novell DOS: secured',0
db 0C0h,'DRDOS: secured',0
db 0C1h,'DRDOS: secured FAT12',0
db 0C4h,'DRDOS: secured <32MB FAT16',0
db 0C6h,'DRDOS: secured >32MB FAT16',0
db 0C6h,'WinNT: corrupted FAT16 volume/stripe set',0
db 0C7h,'WinNT: corrupted NTFS volume/stripe set',0
db 0C7h,'Syrinx: boot',0
db 0CBh,'DRDOS: secured FAT32',0
db 0CCh,'DRDOS: secured FAT32 (LBA)',0
db 0CEh,'DRDOS: secured FAT16 (LBA)',0
db 0D0h,'Multiuser DOS: secured FAT12',0
db 0D1h,'Multiuser DOS: secured FAT12',0
db 0D4h,'Multiuser DOS: secured <32M FAT16',0
db 0D5h,'Multiuser DOS: secured extended',0
db 0D6h,'Multiuser DOS: secured >32M FAT16',0
db 0D8h,'CP/M-86',0
db 0D8h,'CP/M: concurrent',0
db 0D8h,'DOS: concurrent',0
db 0DBh,'CP/M',0
db 0DBh,'CTOS',0
db 0E1h,'SpeedStor: FAT12 extended',0
db 0E1h,'DOS: access',0
db 0E2h,'DOS: read-only (Florian Painke''s XFDISK 1.0.4)',0
db 0E3h,'DOS: read-only',0
db 0E3h,'Storage Dimensions',0
db 0E4h,'SpeedStor: FAT16',0
db 0EBh,'BeOS: BFS1',0
db 0F1h,'SpeedStor',0
db 0F1h,'Storage Dimensions',0
db 0F2h,'DOS: secondary',0
db 0F4h,'SpeedStor: FAT16 large',0
db 0F4h,'Storage Dimensions',0
db 0F5h,'Prologue',0
db 0FEh,'LANstep',0
db 0FEh,'IBM PS/2: Initial Microcode Load (IML)',0
db 0FFh,'Xenix: bad block table',0




db 0B0h,'BugOS fileSystem',0
