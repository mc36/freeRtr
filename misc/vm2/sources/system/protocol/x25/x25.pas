{$heap 63k}
{$stack 3k}
{$sysinc system.inc}
{$sysinc alap.inc}
{$sysinc hex.inc}
{$sysinc param.inc}
{$sysinc bugos.inc}
{$sysinc pipeline.inc}
{$sysinc random.inc}
{$sysinc memory.inc}
{$sysinc inet_tcp.inc}

{$include \sources\internet\kernel\utils\timer2.inc}

{{$define debug1}
{{$define debug2}

{$include x25mem.inc}
{$include x25pack.inc}
{$include x25core.inc}
{$include x25user.inc}


Label f1;
Var
  a:String;
  i,o,p:LongInt;
BEGIN;
WriteLn('x25 v1.0, done by Mc at '#%date' '#%time'.');
Randomize;

listenerPipe:=1;
a:=paramStr(1);
dataPipe:=BVal(a);
if (dataPipe=0) then dataPipe:=BugOS_findProcNam(a);
if (dataPipe<1) then begin;
  WriteLn('using: x25.code <process> [options]');
  WriteLn('options:');
  WriteLn('  mod8');
  WriteLn('  mod128');
  WriteLn('  reset');
  WriteLn('  norst');
  WriteLn('  chnmin=<first#>');
  WriteLn('  chnmax=<last#>');
  WriteLn('  chntx=<bytes>');
  Halt(1);
  end;
mod128:=False;
channelMin:=1;
channelMax:=254;
channelSiz:=128;
for p:=2 to paramCount do begin;
  a:=kicsi(paramStr(p));
  if (a='mod8') then begin; mod128:=False;continue; end;
  if (a='mod128') then begin; mod128:=True;continue; end;
  if (a='reset') then begin; listenerPipe:=1;continue; end;
  if (a='norst') then begin; listenerPipe:=0;continue; end;
  i:=pos('=',a);
  o:=BVal(copy(a,i+1,255));
  a:=copy(a,1,i-1);
  if (a='chnmin') then begin; channelMin:=o and $ff;continue; end;
  if (a='chnmax') then begin; channelMax:=o and $ff;continue; end;
  if (a='chntx') then begin; channelSiz:=o and $7ff;continue; end;
  end;

WriteLn('opening link...');
if (pipeLineCreate(dataPipe,dataPipe,65536,true)<>0) then immErr('error creating connection!');
for i:=1 to 16 do relequish;
i:=255;
if (pipeLineRecv(dataPipe,a[1],i)<>0) then immErr('failed to receive initial data!');
if (i<1) then immErr('initial packet not received!');
a[0]:=chr(i);
move(a[1],addrSize,sizeof(addrSize));
WriteLn('address size: '+BStr(addrSize));
move(a[5],i,sizeof(i));
WriteLn('packet size: '+BStr(i));
a:=copy(a,17+addrSize+addrSize,255);
WriteLn('name: '+copy(a,1,pos(#0,a)-1));

Write('chanmin='+BStr(channelMin)+'  chanmax='+BStr(channelMax));
if mod128 then a:='modulo128' else a:='modulo8';
WriteLn('  packets='+a+'  datamax='+BStr(channelSiz));

if mod128 then modMsk:=$7f else modMsk:=$07;
if (listenerPipe<>0) then sendRestartReq(0,0);
listenerPipe:=0;
listenerProc:=0;
ResizeMem(0);
pipeLineBegListen;
BugOS_SignDaemoning;
f1:
relequish;
timer2start;
relequishToLOC;
relequishToX25;
goto f1;
END.