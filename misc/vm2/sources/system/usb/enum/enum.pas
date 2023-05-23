{$heap 63k}
{$stack 3k}
{$sysinc system.inc}
{$sysinc bugos.inc}
{$sysinc pipeLine.inc}
{$sysinc alap.inc}
{$sysinc memory.inc}
{$sysinc filesys.inc}
{$sysinc filesys2.inc}
{$sysinc textfile.inc}
{$sysinc param.inc}

procedure immErr(a:string);
begin;
writeln(a);
halt(1);
end;

Type
  OneDriverEntryRecord=record
    typ:LongInt;
    drv:String;
    end;
Var
  driverDat:^array[1..1] of OneDriverEntryRecord;
  driverMax:LongInt;
  driverPid:LongInt;
  driverCur:LongInt;
  driverPar:String;
  path:String;

{$include enum1.inc}
{$include enum2.inc}

Function resize(n:LongInt):Boolean;
Var p:Pointer;
Begin;
if (ExtendedMemoryResize(p,n)<n) then immErr('error allocating memory!');
driverDat:=p^;
End;

Procedure readCfg;
Var
  a:String;
  i,o:LongInt;
  t:xtText;
Begin;
path:=xFileName(GetMyFullFileName,1);
if (xtOpen(t,path+'enum.txt',true)<>0) then immErr('error opening database!');
driverMax:=0;
while not xtEOF(t) do begin;
  a:=' '+xtReadLn(t,255)+' ';
  kicserel('  ',' ',a);
  a:=copy(a,2,length(a)-2);
  if (copy(a,1,1)=';') then continue;
  if (a='') then continue;
  i:=pos(' ',a);
  o:=BVal(copy(a,1,i-1)) and $ff;
  a:=copy(a,i+1,255);
  i:=pos(' ',a);
  o:=(o shl 8) or (BVal(copy(a,1,i-1)) and $ff);
  a:=copy(a,i+1,255);
  i:=pos(' ',a);
  o:=(o shl 8) or (BVal(copy(a,1,i-1)) and $ff);
  a:=copy(a,i+1,255);
  if (o=0) then continue;
  if (a='') then continue;
  inc(driverMax);
  resize(driverMax*sizeof(OneDriverEntryRecord));
  driverDat^[driverMax].typ:=o;
  driverDat^[driverMax].drv:=a;
  end;
xtClose(t);
End;


Var
  i,o:LongInt;
  a:String;
BEGIN;
WriteLn('usb enumerator v1.0, done by Mc at '#%date' '#%time'.');

readCfg;

i:=BugOS_findProcNam('usb-uhci.code');
if (i=0) then i:=BugOS_findProcNam('usb-ohci.code');
if (i=0) then i:=BugOS_findProcNam('usb-ehci.code');
if (i=0) then immErr('usb controller not found!');
driverPid:=i;
if (pipeLineCreate(pipeLine,i,65536,true)<>0) then immErr('error creating pipeline!');

bufferD[1]:=0;
usbExchange(4);
o:=bufferD[2];
writeln('# of ports: '+bstr(o));
device:=0;

for i:=0 to o-1 do begin;
  WriteLn('');
  if doPort(i) then continue;
  inc(device);
  if doMove then continue;
  if doDump then continue;
  if doConfig then continue;
  if (driverCur>driverMax) then begin;
    WriteLn('no driver for device!');
    continue;
    end;
  a:=driverDat^[driverCur].drv;
  xExecBgnd(path+a,driverPar,o);
  WriteLn('"'+a+'" "'+driverPar+'" started!');
  end;

END.