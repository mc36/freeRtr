{$heap 63k}
{$stack 3k}
{$sysinc system.inc}
{$sysinc bugos.inc}
{$sysinc pipeLine.inc}
{$sysinc memory.inc}
{$sysinc param.inc}

procedure immErr(a:string);
begin;
writeln(a);
halt(1);
end;

{$include usb.inc}
{$include proto_bulk.inc}
{$include cmdset_scsi.inc}
{$include storage.inc}

BEGIN;
WriteLn('usb mass storage scsi bulk v1.0, done by Mc at '#%date' '#%time'.');
doMain;
END.