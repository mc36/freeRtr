{{$define MSBmachine}
{{$define debug}
{$heap 63k}
{$stack 3k}
{$sysinc system.inc}
{$sysinc datetime.inc}
{$sysinc crt.inc}
{$sysinc filesys.inc}
{$sysinc memory.inc}
{$sysinc param.inc}
{$sysinc bugos.inc}
{$include \sources\developer\utils\codereader.inc}

{$include data.inc}
{$include utils.inc}
{$include memory.inc}
{$include listing.inc}
{$include pipeline.inc}
{$include console.inc}
{$include handle.inc}
{$include process.inc}
{$include drive.inc}
{$include exec.inc}
{$include init.inc}


BEGIN;
initializeAll;
executeLoop;
END.