{$heap 31k}
{$stack 1k}
{$sysinc system.inc}
{$sysinc filesys.inc}
{$sysinc textfile.inc}
{$sysinc param.inc}
{$sysinc hex.inc}


Type modesTyp=record
  m:LongInt;
  x:LongInt;
  y:LongInt;
  b:LongInt;
  d:LongInt;
  l:LongInt;
  end;
Const modesMax=32;
Var
  modesDat:array[1..modesMax+1] of modesTyp;
  modesNum:LongInt;
  output,temp:String;


Function convHex(i:LongInt):String;
Var a:String;
Begin;
a:='';
repeat
  a:=byte2hextype(i)+a;
  i:=i shr 8;
  until (i=0);
convHex:='$'+a;
End;

Procedure immErr(a:String);
Begin;
WriteLn(a);
Halt(1);
End;

Procedure readUpModeList(var t:xtText;scrX,scrY,scrB:LongInt);
Label f1;
Var
  d:modesTyp;
  i,o:LongInt;
  a:String;

function get:LongInt;
begin;
i:=pos(' ',a);
if (i<1) then i:=999;
get:=BVal(copy(a,1,i-1));
a:=copy(a,i+1,255);
end;

Begin;
modesNum:=0;
repeat
  if xtEOF(t) then exit;
  a:=xtReadLn(t,255);
  until (a='---');
repeat
  if xtEOF(t) then exit;
  a:=xtReadLn(t,255);
  until (a='mode maxX maxY bits');
f1:
if xtEOF(t) then exit;
a:=xtReadLn(t,255);
d.m:=(get and $fff);
d.x:=get;
d.y:=get;
d.b:=get;
d.l:=0;
case d.b of
  8:i:=1000000;
  15:i:=4;
  16:i:=3;
  24:i:=2;
  32:i:=1;
  else goto f1;
  end;
if (d.b=scrB) then i:=0;
d.d:=abs(d.x-scrX)+abs(d.Y-scrY)+i;
o:=modesNum+1;
for i:=modesNum downto 1 do if (d.d<modesDat[i].d) then o:=i;
if (o>modesMax) then goto f1;
for i:=modesNum downto o do modesDat[i+1]:=modesDat[i];
modesDat[o]:=d;
if (modesNum<modesMax) then inc(modesNum);
goto f1;
End;


Function readUpModeInfo(var t:xtText;scrX,scrY,scrB:LongInt):LongInt;
Label f1;
Var
  red,grn,blu,a,b:String;
  bpp,lfb:LongInt;
  i:LongInt;
Begin;
readUpModeInfo:=0;
lfb:=0;
case scrB of
  8:begin; bpp:=1;red:='';grn:='';blu:=''; end;
  15:begin; bpp:=2;red:='10 5';grn:='5 5';blu:='0 5'; end;
  16:begin; bpp:=2;red:='11 5';grn:='5 6';blu:='0 5'; end;
  24:begin; bpp:=3;red:='16 8';grn:='8 8';blu:='0 8'; end;
  32:begin; bpp:=4;red:='16 8';grn:='8 8';blu:='0 8'; end;
  else exit;
  end;
repeat
  if xtEOF(t) then exit;
  a:=xtReadLn(t,255);
  until (a='---');
f1:
if xtEOF(t) then begin; readUpModeInfo:=lfb;exit; end;
a:=xtReadLn(t,255);
i:=pos(': ',a);
b:=copy(a,i+2,255);
a:=copy(a,1,i-1);
if (a='supported') then if (b<>'yes') then exit;
if (a='color mode') then if (b<>'yes') then exit;
if (a='graphic mode') then if (b<>'yes') then exit;
if (a='lfb mode') then if (b<>'yes') then exit;
if (a='bytes/scanline') then if (BVal(b)<>scrX*bpp) then exit;
if (a='resolution') then if (b<>BStr(scrX)+' '+BStr(scrY)+' '+BStr(scrB)) then exit;
if (scrB>8) then begin;
  if (a='red') then if (b<>red) then exit;
  if (a='green') then if (b<>grn) then exit;
  if (a='blue') then if (b<>blu) then exit;
  end;
if (a<>'framebuffer') then goto f1;
i:=BVal(b);
if (i=-1) then goto f1;
if (i=0) then goto f1;
lfb:=i;
goto f1;
End;


Function generateScripts(var d:modesTyp):Boolean;
Var
  a:String;
  t:xtText;

Procedure base;
Begin;
xtWriteLn(t,'run \system\process\killprocess.code console.code');
xtWriteLn(t,'run \system\process\killprocess.code graphicon.code');
xtWriteLn(t,'run \system\process\killprocess.code lfbdriver1.code');
xtWriteLn(t,'run \system\process\killprocess.code lfbdriver8.code');
xtWriteLn(t,'run \system\process\killprocess.code lfbdriver8pal.code');
xtWriteLn(t,'run \system\process\killprocess.code lfbdriver15.code');
xtWriteLn(t,'run \system\process\killprocess.code lfbdriver16.code');
xtWriteLn(t,'run \system\process\killprocess.code lfbdriver24.code');
xtWriteLn(t,'run \system\process\killprocess.code lfbdriver32.code');
End;

Begin;
generateScripts:=True;
a:=output+'gui';
xErase(a);
xCreate(a);
if (xtOpen(t,a,false)<>0) then exit;
base;
if (d.m<0) then begin;
  a:='setmode320x200.v86 ic';
  d.x:=320;
  d.y:=200;
  d.b:=8;
  d.l:=$a0000;
  end else begin;
  a:='vesasetmode.v86 ic#'+convHex(d.m or $4000);
  end;
xtWriteLn(t,'run \system\kernel\v86moni.code \system\v86utils\'+a);
if (d.b<=8) then xtWriteLn(t,'run \system\drivers\graphic\vgasetpal.code \system\graphic\pal256.data');
xtWriteLn(t,'run \system\process\daemonrun.code \system\drivers\graphic\lfbdriver'+BStr(d.b)+'.code '+convHex(d.l)+' '+BStr(d.x)+' '+BStr(d.y));
xtWriteLn(t,'run \system\process\daemonrun.code \system\graphic\graphicon.code lfbdriver'+BStr(d.b)+'.code keyboard.code \system\graphic\graphicon.cfg');
xtWriteLn(t,'exit');
xtClose(t);
a:=output+'txt';
xErase(a);
xCreate(a);
if (xtOpen(t,a,false)<>0) then exit;
base;
xtWriteLn(t,'run \system\kernel\v86moni.code \system\v86utils\setmode80x25.v86 ic');
xtWriteLn(t,'run \system\drivers\graphic\vgasetfont.code \system\graphic\font8x16.data');
xtWriteLn(t,'run \system\drivers\graphic\vgasetpal.code \system\graphic\pal16.data');
xtWriteLn(t,'run \system\process\daemonrun.code \system\drivers\console\console.code keyboard.code \system\authentication\login.code console c:\ \utils\shell.code');
xtWriteLn(t,'exit');
xtClose(t);
End;

Procedure captureRun(a:String);
Var
  i:LongInt;
  w:Word;
Begin;
xErase(temp);
i:=xExec('\system\process\capturerun.code',temp+' '+a,w);
if (i or w=0) then exit;
xErase(temp);
immErr('error running '+a+'!');
End;



Label f1;
Var
  scrX,scrY,scrB:LongInt;
  t:xtText;
  i,o:longint;
  d:modesTyp;
BEGIN;
WriteLn('graphic configurator v1.0, done by Mc at '#%date' '#%time'.');

scrX:=BVal(paramStr(1));
scrY:=BVal(paramStr(2));
scrB:=BVal(paramStr(3));
if (scrX<1) or (scrY<1) then immErr('using: gfxcfg.code <resX> <resY> [bits] [output]');
output:=paramStr(4);
if (output='') then output:='graphiCfg.';
temp:=output+'temporary';

captureRun('\system\kernel\v86moni.code \system\v86utils\vesaListModes.v86 ci');

if (xtOpen(t,temp,true)<>0) then immErr('error opening capture!');
readUpModeList(t,scrX,scrY,scrB);
xtClose(t);

for i:=1 to modesNum do begin;
  d:=modesDat[i];
  captureRun('\system\kernel\v86moni.code \system\v86utils\vesaModeInfo.v86 ci#'+convHex(d.m));
  if (xtOpen(t,temp,true)<>0) then immErr('error opening capture!');
  d.l:=readUpModeInfo(t,d.x,d.y,d.b);
  xtClose(t);
  if (d.l<>0) then goto f1;
  end;
d.m:=-1;
f1:
xErase(temp);

generateScripts(d);
WriteLn('scripts successfully generated for '+BStr(d.x)+'x'+BStr(d.y)+'x'+BStr(d.b)+'!');
END.