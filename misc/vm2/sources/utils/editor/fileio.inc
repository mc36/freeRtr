Function SaveCurrentFile(a:String):Boolean;
Var
  t:xtText;
  i:LongInt;
Begin;
SaveCurrentFile:=True;
MemoryChanged:=True;
xCreate(a);
if (xtOpen(t,a,false)<>0) then Exit;
xtSetPos(t,0);
for i:=1 to MemoryLines-1 do begin;
  a:=ReadLine(i);
  a:=Levesz(a);
  xtWriteLn(t,a);
  end;
a:=ReadLine(MemoryLines);
a:=Levesz(a);
xtWrite(t,a);
xtTruncate(t);
xtClose(t);
MemoryChanged:=False;
SaveCurrentFile:=False;
End;

Function LoadCurrentFile(a:String):Boolean;
Label f1,f2,f3;
Var
  t:xtText;
  o:LongInt;
  i:Word;

Procedure KickTabs;
Label f1;
Begin;
Kicserel(#255,' ',a);
Kicserel(#0,' ',a);
f1:
i:=pos(#9,a);
if (i=0) then exit;
delete(a,i,1);
insert(dup(8-((i-1) and 7),' '),a,i);
goto f1;
End;

Begin;
LoadCurrentFile:=True;
MemoryChanged:=True;
if (xtOpen(t,a,true)<>0) then Exit;
if ResizeMemory(1) then goto f3;
WriteLine(1,'');
o:=0;
f1:
a:='';
while not xtEOL(t) do a:=a+xtRead(t,255);
inc(o);
if ResizeMemory(o) then goto f3;
KickTabs;
WriteLine(o,a);
if xtEOF(t) then goto f2;
a:=xtReadLn(t,255);
goto f1;
f2:
MemoryChanged:=False;
LoadCurrentFile:=False;
f3:
xtClose(t);
End;
