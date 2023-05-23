Type
  OnePlayerRecord=record
    num:LongInt;                {user number}
    pipe:LongInt;               {pipeline of connection}
    name:String;                {name of user}
    read:String;                {reader buffer}
    done:Boolean;               {line readed successfully}
    kezD:onePakliRecord;        {cards in hand}
    kezN:LongInt;               {number of cards}
    vitD:onePakliRecord;        {cards taken}
    vitN:LongInt;               {number of cards}
    penz:LongInt;               {pontok}
    mond:LongInt;               {mondott 20-ak}
    end;
Var
  jatekosok:array[1..3] of OnePlayerRecord;
  kezdeniFog:LongInt;


Procedure appendLog(a:String);
Begin;
WriteLn(a);
End;

Function getNextWord(var a:String):String;
Var
  ab:array[0..1] of byte absolute a;
  ab0:byte absolute a;
  i,o:LongInt;
Begin;
while (ab0>0) and (ab[1]=32) do a:=copy(a,2,666);
o:=666;
for i:=ab0 downto 1 do if (ab[i]=32) then o:=i;
getNextWord:=copy(a,1,o-1);
a:=copy(a,o+1,666);
while (ab0>0) and (ab[1]=32) do a:=copy(a,2,666);
End;



Procedure uzenetKuldes(u:LongInt;s:String);
Const max=250;
Var
  sb:array[0..1] of byte absolute s;
  sb0:byte absolute s;
Begin;
if (sb0>max) then sb0:=max;
inc(sb0);sb[sb0]:=13;
inc(sb0);sb[sb0]:=10;
u:=u and 3;
if (u>0) then begin;
  pipeLineSend(jatekosok[u].pipe,sb[1],sb0);
  exit;
  end;
for u:=1 to 3 do pipeLineSend(jatekosok[u].pipe,sb[1],sb0);
End;



Procedure uzenetFogadas(var d:OnePlayerRecord);
Label f1,f2;
Var
  c:char;
  cb:byte absolute c;
  i,o:LongInt;
  a,b:String;
Begin;
f1:
if d.done then goto f2;
i:=sizeof(cb);
if (pipeLineRecv(d.pipe,cb,i)<>0) then i:=0;
if (i<>sizeof(cb)) then begin;
  f2:
  o:=0;
  pipeLineStats(d.pipe,o,i,i);
  if (o<>0) then exit;
  uzenetKuldes(0,'uzenet '+d.name+' kiszallt');
  appendLog('kiszallt '+BStr(d.num));
  halt(1);
  end;
if (cb=8) then begin;
  d.read:=copy(d.read,1,length(d.read)-1);
  goto f1;
  end;
if (cb>=32) then begin;
  d.read:=d.read+c;
  goto f1;
  end;
if (cb<>13) then goto f1;
b:=d.read;
a:=kicsi(getNextWord(b));
if (a='') then begin;
  d.read:='';
  goto f1;
  end;
if (a='uzenet') then begin;
  d.read:='';
  if (b='') then goto f1;
  uzenetKuldes(0,'uzenet '+d.name+'> '+b);
  goto f1;
  end;
if (a='mennyiter') then begin;
  d.read:='';
  i:=mondasConvStr2num(b);
  uzenetKuldes(d.num,'ennyiter '+BStr(mondasConvNum2ert(i))+' '+mondasConvNum2str(i)+' '+mondasConvNum2hun(i));
  goto f1;
  end;
d.done:=true;
End;



Function uzenetOlvasas(p:LongInt):String;
Begin;
if not jatekosok[p].done then begin;
  uzenetOlvasas:='';
  exit;
  end;
uzenetOlvasas:=jatekosok[p].read;
jatekosok[p].read:='';
jatekosok[p].done:=false;
End;



Procedure relequish2players;
Var i:LongInt;
Begin;
relequish;
for i:=1 to 3 do uzenetFogadas(jatekosok[i]);
End;



Procedure receivePlayer(var d:OnePlayerRecord);
Begin;
fillchar(d,sizeof(d),0);
while (pipeLineGetIncoming(d.pipe)<>0) do relequish;
End;



Procedure receiveNames;
Label f1;
Var
  i,o:LongInt;
  a,b:String;
Begin;
f1:
o:=0;
for i:=1 to 3 do if (jatekosok[i].name='') then inc(o);
if (o<1) then begin;
  a:='';
  for i:=1 to 3 do a:=a+' '+jatekosok[i].name;
  uzenetKuldes(o,'nevek'+a);
  exit;
  end;
relequish2players;
for o:=1 to 3 do begin;
  if (jatekosok[o].name<>'') then continue;
  a:=uzenetOlvasas(o);
  if (a='') then continue;
  if (kicsi(getNextWord(a))<>'nevem') then begin;
    uzenetKuldes(o,'hiba add meg a nevedet!');
    continue;
    end;
  jatekosok[o].name:=getNextWord(a);
  end;
goto f1;
End;
