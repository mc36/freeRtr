{$stack 63k}
{$heap 96m}
{$sysinc system.inc}
{$sysinc param.inc}
{$sysinc alap.inc}
{$sysinc filesys.inc}
{$sysinc textfile.inc}

{$define name}

{$include bin.inc}

Procedure immErr(a:String);
Begin;
WriteLn(a);
halt(1);
End;

Const
  gfxMX=1000;
  gfxMY=1000;

Var
  sx,sy:LongInt;
  f:xFile;
  t:xtText;
  a,b:String;
  i,o,p:LongInt;
BEGIN;
writeLn('bin2svg v1.0, done by Mc at '#%date' '#%time'.');
a:=paramStr(1);
if (a='') then immErr('using: prog.code <bin>');
writeln('reading '+a+'...');

xOpen(f,a,xGenFilMod_r);
readUpTables(f);

writeLn('nodes='+BStr(headR.nodes)+' conns='+BStr(headR.conns));

a:=paramStr(1)+'.svg';
writeLn('writing '+a+'...');
xErase(a);
xCreate(a);
xtOpen(t,a,false);
xtWriteLn(t,'<?xml version="1.0"?>');
xtWriteLn(t,'<svg version="1.1" xmlns="http://www.w3.org/2000/svg">');

sx:=((headR.endX-headR.begX) div gfxMX)+1;
sy:=((headR.endY-headR.begY) div gfxMY)+1;

for i:=1 to headR.nodes do begin;
  nodeD[i].x:=(nodeD[i].x-headR.begX) div sx;
  nodeD[i].y:=gfxMY-((nodeD[i].y-headR.begY) div sy);
  end;

for i:=1 to headR.conns do begin;
  o:=connD[i].a;
  p:=connD[i].b;
  xtWriteLn(t,'<line stroke="black" x1="'+BStr(nodeD[o].x)+'" y1="'+BStr(nodeD[o].y)+'" x2="'+BStr(nodeD[p].x)+'" y2="'+BStr(nodeD[p].y)+'"/>');
  end;

for i:=1 to headR.nodes do begin;
  xtWriteLn(t,'<circle fill="green" cx="'+BStr(nodeD[i].x)+'" cy="'+BStr(nodeD[i].y)+'" r="5"/>');
  o:=nodeD[i].nam;
  if (o<1) then continue;
  a:=readOneName(f,o);
  xtWriteLn(t,'<text fill="blue" x="'+BStr(nodeD[i].x)+'" y="'+BStr(nodeD[i].y+15)+'" >'+a+'</text>');
  end;

xtWriteLn(t,'</svg>');
xtClose(t);
END.