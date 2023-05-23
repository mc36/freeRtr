{$heap 63k}
{$stack 3k}
{$sysinc system.inc}
{$sysinc alap.inc}
{$sysinc crt.inc}
{$sysinc param.inc}
{$sysinc filesys.inc}
{$sysinc pipeline.inc}
{$sysinc bugos.inc}

{$include cddriver.inc}

Procedure DisplayModel;
Var a,b,c:String;
Begin;
if CDdriverIdentify(a,b,c) then exit;
WriteLn('model: "'+a+'"');
WriteLn('serial: "'+b+'"');
WriteLn('firm: "'+c+'"');
End;

Function discErr:Boolean;
Var b:Boolean;
Begin;
discErr:=True;
if CDdriverCheckDisk(b) then exit;
discErr:=not b;
End;

Procedure DisplayTOC;
Var
  i,o,p:LongInt;
  b:Byte;
Begin;
if discErr then begin;
  WriteLn('disk not ready!');
  exit;
  end;
if CDdriverReadTOChdr(i,o) then exit;
WriteLn('tracks '+BStr(i)+'..'+BStr(o)+':');
for i:=i to o do begin;
  if CDdriverReadTOCntry(i,o,p,b) then exit;
  Write('track #'+BStr(i)+': (');
  Write(alakit(o)+'..'+alakit(p)+') ');
  Write(alakit((p-o)*2)+' k ');
  case b of
    1:Write('data');
    2:Write('audio');
    else Write('unknown');
    end;
  WriteLn('');
  end;
End;

Procedure extractTrack(trk:Byte;a:String);
Var
  f:xFile;
  d:array[1..2048] of byte;
  i,o,p:LongInt;
  b:Byte;
Begin;
if discErr then begin;
  WriteLn('disk not ready!');
  exit;
  end;
if CDdriverReadTOCntry(trk,o,p,b) then exit;
if (b<>1) then begin;
  WriteLn('not data track!');
  exit;
  end;
if (xCreate(a)<>0) then begin;
  WriteLn('error creating file!');
  exit;
  end;
if (xOpen(f,a,xGenFilMod_rw)<>0) then begin;
  WriteLn('error opening file!');
  exit;
  end;
WriteLn('extracting track #'+BStr(trk)+' ('+BStr(o)+'..'+BStr(p)+' blocks)...');
for o:=o to p-1 do begin;
  Write(#13+BStr(o));
  if CDdriverReadSector(o,d) then begin;
    WriteLn(#13'block #'+BStr(o)+' is unreadable!');
    fillchar(d,sizeof(d),0);
    end;
  xBlockWrite(f,d,sizeof(d));
  end;
xClose(f);
Write(#13'                       '#13);
End;



Function ReadLine:String;
Label f0,f1;
Var
  a:String;
  w:Word;
Begin;
f0:
a:='';
Write(#13'>');
f1:
w:=ReadKey;
if (w and $fe00=0) then begin;{simple key}
  w:=w and $ff;
  if (w in [0,255,13,10,8,9]) then w:=ord(' ');
  if (length(a)>250) then goto f1;
  a:=a+chr(w);
  write(chr(w));
  goto f1;
  end;
case w of
  $8001:begin;{redraw}
    clrscr;
    goto f0;
    end;
  $8003:begin;{backspace}
    if (a='') then goto f1;
    Write(#8' '#8);
    a:=copy(a,1,length(a)-1);
    goto f1;
    end;
  $8004:begin;{enter}
    WriteLn('');
    ReadLine:=a;
    exit;
    end;
  $8005:begin;{escape}
    WriteLn('');
    goto f0;
    end;
  end;
goto f1;
End;

Function xLevesz(a:String):String;
Begin;
Kicserel(#9,' ',a);
Kicserel(#255,' ',a);
Kicserel(#0,' ',a);
while (Copy(a,1,1)=' ') do Delete(a,1,1);
xLevesz:=Levesz(a);
End;

Function GetNextWord(var a:string):String;
Var i:Word;
Begin;
i:=pos(' ',a);
if (i<1) then i:=$666;
GetNextWord:=copy(a,1,i-1);
delete(a,1,i);
End;


Label f1;
Var
  a,b:String;
  i,o:LongInt;
BEGIN;
WriteLn('cd player v1.0, done by Mc at '#%date' '#%time'.');
if (paramCount<>2) then begin;
  WriteLn('using: cdplayer.code <process> <drive>');
  Halt(1);
  end;
a:=ParamStr(1);
i:=BVal(ParamStr(2));
WriteLn('opening '+a+' '+BStr(i)+'...');
if CDdriverOpen(a,i) then begin;
  WriteLn('error opening cd driver!');
  Halt(2);
  end;
f1:
WriteLn('');
a:=readLine;
b:=kicsi(xLevesz(a));
if (b='') then goto f1;
a:=GetNextWord(b);
if (a='exit') then Halt(0);
if (a='help') or (a='?') then begin;
  WriteLn(' help');
  WriteLn('~~~~~~');
  WriteLn('help            - display this screen');
  WriteLn('exit            - leave this shell');
  WriteLn('device          - display device info');
  WriteLn('open            - open cd tray');
  WriteLn('close           - close cd tray');
  WriteLn('lock            - lock cd tray');
  WriteLn('unlock          - unlock cd tray');
  WriteLn('check           - check for disk');
  WriteLn('toc             - display table of contents');
  WriteLn('play <b> <e>    - play from block <b> to block <e>');
  WriteLn('pause           - pause playing');
  WriteLn('resume          - resume playing');
  WriteLn('stop            - stop playing');
  WriteLn('playPos         - display playing position');
  WriteLn('extract <t> <f> - extract track <t> to file <f>');
  goto f1;
  end;
if (a='device') then begin;
  DisplayModel;
  goto f1;
  end;
if (a='open') then begin;
  if CDdriverOpenDoor(true) then a:='failed' else a:='succeeded';
  WriteLn(a);
  goto f1;
  end;
if (a='close') then begin;
  if CDdriverOpenDoor(false) then a:='failed' else a:='succeeded';
  WriteLn(a);
  goto f1;
  end;
if (a='lock') then begin;
  if CDdriverLockDoor(true) then a:='failed' else a:='succeeded';
  WriteLn(a);
  goto f1;
  end;
if (a='unlock') then begin;
  if CDdriverLockDoor(false) then a:='failed' else a:='succeeded';
  WriteLn(a);
  goto f1;
  end;
if (a='check') then begin;
  if discErr then a:='absent' else a:='present';
  WriteLn('disk is '+a);
  goto f1;
  end;
if (a='toc') then begin;
  DisplayTOC;
  goto f1;
  end;
if (a='play') then begin;
  i:=BVal(GetNextWord(b));
  o:=BVal(GetNextWord(b));
  if CDdriverPlayStart(i,o) then a:='failed' else a:='succeeded';
  WriteLn(a);
  goto f1;
  end;
if (a='pause') then begin;
  if CDdriverPlayPause(true) then a:='failed' else a:='succeeded';
  WriteLn(a);
  goto f1;
  end;
if (a='resume') then begin;
  if CDdriverPlayPause(false) then a:='failed' else a:='succeeded';
  WriteLn(a);
  goto f1;
  end;
if (a='stop') then begin;
  if CDdriverPlayStop then a:='failed' else a:='succeeded';
  WriteLn(a);
  goto f1;
  end;
if (a='playpos') then begin;
  if CDdriverPlayPosition(i,o) then writeLn('failed') else begin;
    WriteLn('track: '+BStr(i));
    WriteLn('block: '+BStr(o));
    end;
  goto f1;
  end;
if (a='extract') then begin;
  i:=BVal(GetNextWord(b));
  extractTrack(i,b);
  goto f1;
  end;

WriteLn('unknown command!');
goto f1;
END.