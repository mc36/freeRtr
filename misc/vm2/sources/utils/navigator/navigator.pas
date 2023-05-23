{$stack 63k}
{$heap 96m}
{$sysinc system.inc}
{$sysinc param.inc}
{$sysinc alap.inc}
{$sysinc crt.inc}
{$sysinc filesys.inc}
{$sysinc bugos.inc}
{$sysinc pipeline.inc}
{$include \sources\internet\kernel\utils\timer2.inc}

{$define name}

{$include gps.inc}
{$include bin.inc}
{$include spf.inc}

Type
  oneScreenRecord=array[1..25] of array[1..80] of record
    b:byte;
    c:byte;
    end;
Var
  connC:array[1..connL] of byte;
  routD:array[1..nodeL] of LongInt;
  routS:LongInt;
  routT:LongInt;
  routN:LongInt;
  dataFile:xFile;
  bx,by,zm:LongInt;
  oldScreen:oneScreenRecord;
  newScreen:oneScreenRecord;
  lastQuery:LongInt;
  lastKeypr:LongInt;


Procedure immErr(a:String);
Begin;
WriteLn(a);
halt(1);
End;

Procedure wrt(x,y,c:LongInt;a:String);
Var
  ab:array[0..1] of byte absolute a;
  i:LongInt;
Begin;
if (y<1) then exit;
if (y>25) then exit;
dec(x);
for i:=1 to ab[0] do begin;
  inc(x);
  if (x<0) then continue;
  if (x>80) then exit;
  newScreen[y][x].b:=ab[i];
  newScreen[y][x].c:=c;
  end;
End;


Procedure displayScreen;
Var x,y:LongInt;
Begin;
newScreen[25][80]:=oldScreen[25][80];
for y:=1 to 25 do for x:=1 to 80 do begin;
  if (newScreen[y][x].b=oldScreen[y][x].b) and (newScreen[y][x].c=oldScreen[y][x].c) then continue;
  TextColor(newScreen[y][x].c);
  gotoXY(x,y);
  BugOS_WriteCustomChar(chr(newScreen[y][x].b));
  end;
move(newScreen,oldScreen,sizeof(oldScreen));
End;


Procedure drawMap;
Var scr:array[1..50] of array[1..80] of byte;

procedure pnt(x,y,c:longint);
begin;
if (x<1) then exit;
if (x>80) then exit;
if (y<1) then exit;
if (y>50) then exit;
y:=51-y;
if (scr[y][x]>c) then exit;
scr[y][x]:=c;
end;

procedure lin(x1,y1,x2,y2,c:longint);
const prc=200;
var i,o,p:LongInt;
begin;
dec(x2,x1);
dec(y2,y1);
x1:=x1*prc;
y1:=y1*prc;
for i:=1 to prc do begin;
  o:=y1 div prc;
  p:=x1 div prc;
  pnt(p,o,c);
  inc(x1,x2);
  inc(y1,y2);
  end;
end;

Var
  n,c,x,y:LongInt;
  nd:oneNodeRecord;
Begin;
fillchar(scr,sizeof(scr),0);
for n:=1 to headR.nodes do begin;
  nd:=nodeD[n];
  x:=(nd.x-bx) div zm;
  y:=(nd.y-by) div zm;
  if (x<-80) then continue;
  if (y<-50) then continue;
  if (x>160) then continue;
  if (y>100) then continue;
  for c:=nd.b to nd.e do begin;
    nd:=nodeD[connD[c].b];
    lin(x,y,(nd.x-bx) div zm,(nd.y-by) div zm,connC[c]);
    end;
  end;
pnt((gps_posX-bx) div zm,(gps_posY-by) div zm,12);
for y:=1 to 23 do for x:=1 to 80 do begin;
  newScreen[y][x].c:=(scr[y*2][x] shl 4)+scr[y*2-1][x];
  newScreen[y][x].b:=223;
  end;
End;



Procedure drawStatus;
Var
  a:STring;
  ab:array[0..1] of byte absolute a;
  ab0:byte absolute a;
  i,o:LongInt;

procedure wrvl(x,y,s:LongInt;a,b:String);
var
  bb:array[0..1] of byte absolute b;
  bb0:byte absolute b;
  i,o:LongInt;
begin;
while (bb0<s) do begin;
  inc(bb0);
  bb[bb0]:=32;
  end;
bb0:=s;
wrt(x,y,$17,a);
wrt(x+length(a),y,$1f,b);
end;

function crd(i:LongInt):String;
const dvr=10000000;
var a:string;
begin;
a:=BStr(i mod dvr);
while (length(a)<7) do a:='0'+a;
crd:=BStr(i div dvr)+'.'+a;
end;

Begin;
fillchar(a,sizeof(a),32);
ab0:=80;
for i:=24 to 25 do wrt(1,i,$17,a);
wrvl(1,24,15,'x=',crd(gps_posX));
wrvl(1,25,15,'y=',crd(gps_posY));
wrvl(20,24,5,'high=',BStr(gps_posZ));
wrvl(20,25,5,'speed=',BStr(gps_sped));
wrvl(35,24,19,'date=',gps_date+' '+gps_time);
wrvl(35,25,19,'stat=',gps_stat);
wrvl(70,24,5,'prec=',BStr(gps_prec));
wrvl(70,25,5,'sat=',BStr(gps_sats));
End;



Procedure findBestRoute(i:LongInt);
Begin;
routN:=i;
routT:=getNameNode(routN);
for i:=1 to headR.conns do connC[i]:=7;
i:=findNearestNode(gps_posX,gps_posY,0,1);
routS:=calcShortestPaths(i,routT,headR.nodes,routD);
for i:=1 to routS do connC[routD[i]]:=10;
End;



Procedure selectTarget;
Label f1;
Var
  a,b:String;
  ab:array[0..1] of byte absolute a;
  ab0:byte absolute a;
  i,o,p:LongInt;
Begin;
p:=routN;
b:='';
f1:
if (p>headR.names) then p:=headR.names;
if (p<1) then p:=1;
for i:=1 to 22 do begin;
  a:=readOneName(dataFile,p+i-11);
  while (ab0<80) do begin;
    inc(ab0);
    ab[ab0]:=32;
    end;
  ab0:=80;
  wrt(1,i,7,a);
  end;
for i:=1 to 80 do inc(newScreen[11][i].c,$10);
a:=b;
while (ab0<80) do begin;
  inc(ab0);
  ab[ab0]:=32;
  end;
ab0:=80;
wrt(1,23,$27,a);
displayScreen;
i:=readkey;
case i and $ffff of
  0..$1ff:begin; {keys}
    b:=b+chr(i);
    p:=findOneName(dataFile,b);
    end;
  $8003:b:=copy(b,1,length(b)-1); {backspace}
  $8005:exit; {escape}
  $8004:begin; {enter}
    findBestRoute(p);
    exit;
    end;
  $800c:dec(p); {up}
  $800d:inc(p); {down}
  $800a:dec(p,10); {pgup}
  $800b:inc(p,10); {pgdn}
  end;
goto f1;
End;


Procedure moveToCurr;
Begin;
bx:=gps_posX-zm*40;
by:=gps_posY-zm*25;
End;


Label f1;
Var
  a,b:String;
  i,o,p:LongInt;
BEGIN;
writeLn('navigator v1.0, done by Mc at '#%date' '#%time'.');
a:=paramStr(1);
if (a='') then immErr('using: prog.code <bin>');
writeln('reading '+a+'...');

xOpen(dataFile,a,xGenFilMod_r);
readUpTables(dataFile);
writeLn('nodes='+BStr(headR.nodes)+' conns='+BStr(headR.conns));

lastQuery:=0;
lastKeypr:=0;
gpsClearData;
gpsPollData;

findBestRoute(0);
bx:=headR.begX;
by:=headR.begY;
zm:=512;

clrscr;
fillchar(oldScreen,sizeof(oldScreen),$ff);
fillchar(newScreen,sizeof(newScreen),0);

f1:
relequish;
timer2start;
if (getTimePast(lastQuery)>0) then begin;
  lastQuery:=currentTime;
  gpsPollData;
  end;
if (getTimePast(lastKeypr)>15) then moveToCurr;
drawMap;
drawStatus;
displayScreen;
if not keypressed then goto f1;
lastKeypr:=currentTime;
case ReadKey of
  43:begin; {+}
    zm:=zm shr 1;
    if (zm<1) then zm:=1;
    inc(bx,zm*40);
    inc(by,zm*25);
    end;
  45:begin; {-}
    dec(bx,zm*40);
    dec(by,zm*25);
    zm:=zm shl 1;
    end;
  $800c:inc(by,4*zm); {up}
  $800d:dec(by,4*zm); {down}
  $800e:dec(bx,8*zm); {left}
  $800f:inc(bx,8*zm); {right}
  $8005:exit; {escape}
  $8004:selectTarget; {enter}
  32:begin; lastKeypr:=0;moveToCurr; end; {space}
  end;
goto f1;
END.