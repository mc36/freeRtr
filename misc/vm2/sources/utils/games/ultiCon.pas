{$heap 31k}
{$stack 3k}
{$sysinc system.inc}
{$sysinc alap.inc}
{$sysinc crt.inc}
{$sysinc bugos.inc}
{$sysinc pipeline.inc}
{$sysinc random.inc}
{$sysinc param.inc}

{$include ultiClnt.inc}
{$include ultiServ-pakli.inc}

Const
  mindenMondas:String='passz'#13'40100'#13'20100'#13'4asz'#13'4tiz'#13'ulti'#13'betli'#13'durtmars'#13'piros'#13'terit';
  mindenKontra:String='party'#13'40100'#13'20100'#13'4asz'#13'4tiz'#13'ulti'#13'betli'#13'durtmars';
Var
  nevek:array[1..3] of String;
  lapok:String;
  lapMutat:Boolean;
  elsoDobas:Boolean;




Function doOneSelection(msk,cur:LongInt;a:String):LongInt;
Label kiir,key;
Var
  max:LongInt;
  dat:array[1..32] of string;
  i,o:LongInt;
Begin;
max:=0;
while (a<>'') do begin;
  i:=pos(#13,a);
  if (i<1) then i:=666;
  inc(max);
  dat[max]:=copy(a,1,i-1);
  a:=copy(a,i+1,666);
  end;
while keypressed do ReadKey;
kiir:
TextColor($07);
Write(#13' ');
for i:=1 to max do begin;
  if (i=cur) then o:=$10 else o:=$00;
  if ((1 shl i) and msk<>0) then inc(o,11) else inc(o,7);
  TextColor(o);
  if (o and $f0<>0) then write(#8' ');
  Write(dat[i]+' ');
  end;
key:
i:=readKey;
o:=cur;
case i of
  $800e:dec(cur);
  $800f:inc(cur);
  $8004:begin;
    TextColor($07);
    Write(#13);
    doOneSelection:=cur;
    exit;
    end;
  end;
if (cur<1) then cur:=max;
if (cur>max) then cur:=1;
if (o<>cur) then goto kiir;
goto key;
End;


Function selectOne(a:String):LongInt;
Begin;
selectOne:=doOneSelection(0,1,a);
writeLn('');
End;

Function selectMore(a:String):LongInt;
Label f1;
Var c,m:LongInt;
Begin;
c:=1;
m:=2;
f1:
c:=doOneSelection(m,c,'kesz'#13+a);
if (c<>1) then begin;
  m:=(1 shl c) xor m;
  goto f1;
  end;
WriteLn('');
selectMore:=m shr 2;
End;

Procedure displayCards(a:String);
Const
  szinErtek:array[1..4] of byte=($0c,$0e,$0a,$06);
  szinNevek:array[1..4] of String[7]=('piros ','tok   ','zold  ','makk  ');
  szamNevek:array[1..8] of String[7]=('het   ','nyolc ','kilenc','tiz   ','also  ','felso ','kiraly','asz   ');
Var
  szin:array[1..16] of byte;
  szam:array[1..16] of byte;
  num:LongInt;
  i,o:LongInt;
Begin;
a:=kicsi(a);
num:=0;
while (length(a)>=2) do begin;
  inc(num);
  szin[num]:=pos(a[1],szinKodok);
  szam[num]:=pos(a[2],lapKodok);
  a:=copy(a,3,666);
  end;
a:=' _____';
for i:=1 to num do begin;
  textColor(szinErtek[szin[i]]);
  Write(a);
  end;
textColor(7);
WriteLn('');
for o:=1 to 6 do begin;
  for i:=1 to num do begin;
    textColor(szinErtek[szin[i]]);
    write(' |'+szinNevek[szin[i]][o]+' '+szamNevek[szam[i]][o]+'|');
    end;
  textColor(7);
  WriteLn('');
  end;
a:=' ~~~~~';
for i:=1 to num do begin;
  textColor(szinErtek[szin[i]]);
  Write(a);
  end;
textColor(7);
WriteLn('');
End;


Function selectLapok(szam:LongInt):String;
Label f1,f2;
Var
  a:String;
  i,o,p:LongInt;
Begin;
displayCards(lapok);
p:=0;
o:=1;
f1:
a:='';
for i:=1 to length(lapok) shr 1 do a:=a+' /|\ '#13;
a:=a+'kesz';
o:=doOneSelection(p,o,a);
if (o>length(lapok) shr 1) then goto f2;
p:=(1 shl o) xor p;
goto f1;
f2:
a:='';
for i:=1 to length(lapok) shr 1 do if ((1 shl i) and p<>0) then a:=a+copy(lapok,i*2-1,2);
if (length(a)<>szam shl 1) then goto f1;
WriteLn('');
selectLapok:=a;
End;

Function sajat2valodi(i:LongInt):LongInt;
Var o:LongInt;
Begin;
o:=0;
if (i and $001<>0) then inc(o,mondas_party);
if (i and $002<>0) then inc(o,mondas_40szaz);
if (i and $004<>0) then inc(o,mondas_20szaz);
if (i and $008<>0) then inc(o,mondas_4asz);
if (i and $010<>0) then inc(o,mondas_4tiz);
if (i and $020<>0) then inc(o,mondas_ulti);
if (i and $040<>0) then inc(o,mondas_betli);
if (i and $080<>0) then inc(o,mondas_durt);
if (i and $100<>0) then inc(o,1);
if (i and $200<>0) then inc(o,mondas_terit);
sajat2valodi:=o;
End;



Label f1,f2,f3,f4,f5;
Var
  i,o,p:LongInt;
  a,b:String;
BEGIN;
WriteLn('ulti console v1.0, done by Mc at '#%date' '#%time'.');
a:=GetAllParameters;
b:=getNextWord(a);
if (a='') then immErr('using: con.code <process> <name>');
ultiKapcsolodik(b,a);

lapok:='';
f1:
textColor($07);
b:=ultiFogad;
f2:
a:=kicsi(getNextWord(b));
if (a='') then goto f1;
if (a='sorszamod') then goto f1;
if (a='nevek') then begin;
  for i:=1 to 3 do nevek[i]:=getNextWord(b);
  goto f1;
  end;
if (a='uzenet') then begin;
  textColor($0f);
  writeln(b);
  goto f1;
  end;
if (a='hiba') then begin;
  textColor($0c);
  writeln(b);
  goto f1;
  end;
if (a='ujjatek') then begin;
  selectOne('tovabb');
  for i:=1 to 8 do writeln('');
  writeln('===========================================================================');
  lapMutat:=true;
  goto f1;
  end;
if (a='lapjaid') then begin;
  lapok:=b;
  if lapMutat then begin;
    textColor($0f);
    WriteLn('lapjaid:');
    displayCards(lapok);
    end;
  lapMutat:=false;
  if (length(lapok)<=20) then goto f1;
  f3:
  textColor($0c);
  WriteLn('jelold meg, hogy mit dobsz a talonba!');
  b:=selectLapok(2);
  i:=selectMore(mindenMondas);
  ultiKuld('licit '+b+' '+mondasConvNum2str(sajat2valodi(i)));
  b:=ultiFogad;
  a:=b;
  a:=kicsi(getNextWord(a));
  if (a<>'jatek') then goto f3;
  lapMutat:=true;
  goto f2;
  end;
if (a='tarsad') then begin;
  textColor($0f);
  WriteLn('tarsad lapjai:');
  displayCards(b);
  goto f1;
  end;
if (a='ellenfel') then begin;
  textColor($0f);
  WriteLn('ellenfeled lapjai:');
  displayCards(b);
  goto f1;
  end;
if (a='jatek') then begin;
  i:=BVal(getNextWord(b));
  o:=mondasConvStr2num(b);
  textColor($0a);
  WriteLn(nevek[i]+' mondott egy '+mondasConvNum2hun(o)+'-t.');
  goto f1;
  end;
if (a='mehet') then begin;
  i:=BVal(b);
  textColor($0a);
  WriteLn(nevek[i]+' szerint mehet.');
  goto f1;
  end;
if (a='felvette') then begin;
  i:=BVal(b);
  textColor($0a);
  WriteLn(nevek[i]+' felvette.');
  goto f1;
  end;
if (a='szerinted') then begin;
  f5:
  textColor($0c);
  WriteLn('mehet, vagy inkabb licitalsz?');
  case selectOne('mehet'#13'felveszem'#13+penzNeve) of
    1:a:='mehet';
    2:a:='felveszem';
    3:begin;
      i:=selectMore(mindenMondas);
      ultiKuld('mennyiter '+mondasConvNum2str(sajat2valodi(i)));
      b:=ultiFogad;
      a:=kicsi(getNextWord(b));
      if (a<>'ennyiter') then goto f5;
      i:=BVal(getNextWord(b));
      getNextWord(b);
      textColor($0f);
      WriteLn('a(z) '+b+' '+BStr(i)+' '+penzNeve+'t er.');
      goto f5;
      end;
    else immErr('bug!');
    end;
  ultiKuld(a);
  goto f1;
  end;
if (a='talon') then begin;
  textColor($0f);
  WriteLn('a talon tartalma:');
  displayCards(b);
  lapMutat:=false;
  goto f1;
  end;
if (a='szine') then begin;
  textColor($0c);
  WriteLn('add meg az adut szinet!');
  case selectOne('piros'#13'tok'#13'zold'#13'makk') of
    1:a:='p';
    2:a:='t';
    3:a:='z';
    4:a:='m';
    else immErr('bug!');
    end;
  ultiKuld('szine '+a);
  goto f1;
  end;
if (a='jatszuk') then begin;
  p:=BVal(getNextWord(b));
  i:=szinConvStr2num(getNextWord(b));
  o:=mondasConvStr2num(b);
  textColor($0a);
  WriteLn(nevek[p]+' jatszik egy '+mondasConvNum2hun(o)+'-t.');
  WriteLn('az adu szine a '+szinConvNum2hun(i)+'.');
  elsoDobas:=true;
  goto f1;
  end;
if (a='vanegy') then begin;
  p:=BVal(getNextWord(b));
  textColor($0a);
  WriteLn(nevek[p]+'-nak van '+getNextWord(b)+' db 40-e es '+getNextWord(b)+' db 20-a.');
  goto f1;
  end;
if (a='kontra') then begin;
  p:=BVal(getNextWord(b));
  o:=mondasConvStr2num(b);
  textColor($0a);
  WriteLn(nevek[p]+' megkontrazta a '+mondasConvNum2hun(o)+'-t.');
  goto f1;
  end;
if (a='ujkor') then begin;
  writeln('---------------------------------------------------------------------------');
  goto f1;
  end;
if (a='dobott') then begin;
  p:=BVal(getNextWord(b));
  o:=lapConvStr2num(b);
  textColor($0f);
  WriteLn(nevek[p]+' dobott egy '+lapConvNum2hun(o)+'-t:');
  displayCards(lapConvNum2str(o));
  goto f1;
  end;
if (a='dobjal') then begin;
  if not elsoDobas then goto f4;
  elsoDobas:=false;
  displayCards(lapok);
  textColor($0c);
  WriteLn('mondd be a dolgaidat!');
  i:=selectMore('1 db 40'#13'1 db 20'#13'2 db 20'#13'3 db 20');
  a:='0';
  if (i and 2<>0) then a:='1';
  if (i and 4<>0) then a:='2';
  if (i and 8<>0) then a:='3';
  a:=' '+a;
  if (i and 1=0) then a:='0'+a else a:='1'+a;
  ultiKuld('vanegy '+a);
  textColor($0c);
  WriteLn('mondd be a kontraidat!');
  i:=selectMore(mindenKontra);
  ultiKuld('kontra '+mondasConvNum2str(sajat2valodi(i)));
  f4:
  textColor($0c);
  WriteLn('dobjal valamit!');
  ultiKuld('dobok '+selectLapok(1));
  goto f1;
  end;
if (a='renonc') then begin;
  textColor($0f);
  WriteLn('csak ezeket dobhatod:');
  displayCards(b);
  goto f4;
  end;
if (a='korveg') then begin;
  p:=BVal(getNextWord(b));
  textColor($0f);
  WriteLn('ezt a kort '+nevek[p]+' vitte.');
  goto f1;
  end;
if (a='jatekveg') then begin;
  textColor($0b);
  WriteLn('vege a jateknak, a talon ez volt:');
  displayCards(getNextWord(b));
  textColor($0b);
  WriteLn('a fizetesek a kovetkezok:');
  for i:=1 to 3 do begin;
    o:=BVal(getNextWord(b));
    if (o<0) then a:='vesztett' else a:='nyert';
    writeLn(nevek[i]+': '+BStr(Abs(o))+' '+penzNeve+'t '+a);
    end;
  WriteLn('');
  goto f1;
  end;


immErr('unknown message');
END.