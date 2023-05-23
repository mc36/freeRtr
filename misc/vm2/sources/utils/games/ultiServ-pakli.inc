Type
  onePakliRecord=array[0..63] of byte;
Const
  penzNeve='pengo';
  szinKodok:String='ptzm';
  lapKodok:String='7890afks';
Const
  mondas_szinMask=$0007;
  mondas_party=$0008;
  mondas_4asz=$0010;
  mondas_40szaz=$0020;
  mondas_ulti=$0040;
  mondas_betli=$0080;
  mondas_20szaz=$0100;
  mondas_durt=$0200;
  mondas_4tiz=$0400;
  mondas_terit=$0800;
  mondas_csszaz=$1000;
Const
  minden_szines=mondas_party or mondas_4asz or mondas_40szaz or mondas_ulti or mondas_20szaz or mondas_4tiz;
  minden_jatek=mondas_party or mondas_4asz or mondas_40szaz or mondas_ulti or mondas_betli or mondas_20szaz or mondas_durt or mondas_4tiz;
  minden_partys=mondas_4asz or mondas_4tiz or mondas_ulti;
  minden_nparty=mondas_40szaz or mondas_20szaz or mondas_durt;



Function kiAharmadik(n1,n2:LongInt):LongInt;
Var i,o:LongInt;
Begin;
o:=1;
for i:=3 downto 1 do begin;
  if (i=n1) then continue;
  if (i=n2) then continue;
  o:=i;
  end;
kiAharmadik:=o;
End;



Procedure paklibanCserel(var a,b:byte);
Var c:byte;
Begin;
c:=a;
a:=b;
b:=c;
End;



Procedure generatePakli(a,b,c,d:byte;fent:boolean;var trg:onePakliRecord);

procedure add(i:byte);
begin;
inc(trg[0]);
trg[trg[0]]:=i;
end;

Var i,o:LongInt;
Begin;
a:=((a and 3)+1) shl 4;
b:=((b and 3)+1) shl 4;
c:=((c and 3)+1) shl 4;
d:=((d and 3)+1) shl 4;
trg[0]:=0;
for i:=1 to 8 do add(a+i);
for i:=1 to 8 do add(b+i);
for i:=1 to 8 do add(c+i);
for i:=1 to 8 do add(d+i);
if fent then for i:=1 to 4 do begin;
  o:=(i*8)-7;
  paklibanCserel(trg[o+3],trg[o+4]);
  paklibanCserel(trg[o+4],trg[o+5]);
  paklibanCserel(trg[o+5],trg[o+6]);
  end;
End;



Function keresesPakliban(var pak:onePakliRecord;siz,lap:LongInt):LongInt;
Label f1;
Var i:LongInt;
Begin;
pak[0]:=siz;
for i:=1 to siz do if (pak[i]=lap) then goto f1;
i:=-1;
f1:
keresesPakliban:=i;
End;



Procedure sorbarendezPakli(var sor,trg:onePakliRecord;siz:LongInt);
Var i,o,p,q:LongInt;
Begin;
q:=0;
for p:=1 to 32 do begin;
  o:=sor[p];
  if not (o shr 4 in [1..4]) then continue;
  if not (o and 15 in [1..8]) then continue;
  for i:=q+1 to siz do if (o=trg[i]) then begin;
    inc(q);
    paklibanCserel(trg[q],trg[i]);
    break;
    end;
  end;
trg[0]:=q;
for i:=q+1 to 32 do trg[i]:=0;
End;



Function pakliban20keres(var pak:onePakliRecord;adu:LongInt):LongInt;
Var i,o,p:LongInt;
Begin;
p:=0;
for o:=1 to 4 do begin;
  i:=(o shl 4);
  if (keresesPakliban(pak,12,i+6)<0) then continue;
  if (keresesPakliban(pak,12,i+7)<0) then continue;
  if (adu=o) then inc(p,$10) else inc(p);
  end;
pakliban20keres:=p;
End;



Function doKeveres:String;
Var src,trg:onePakliRecord;

procedure oneRnd1;
var i,o:LongInt;
begin;
for o:=32 downto 1 do begin;
  i:=random(o)+1;
  trg[o]:=src[i];
  src[i]:=src[o];
  end;
src:=trg;
end;

procedure oneRnd2;
var i,o:LongInt;
begin;
i:=random(32)+1;
o:=random(32)+1;
paklibanCserel(trg[i],trg[o]);
end;

Var
  i:LongInt;
  a:string absolute trg;
Begin;
generatePakli(0,1,2,3,true,src);
for i:=1 to 8 do oneRnd1;
for i:=1 to 128 do oneRnd2;
trg[0]:=32;
doKeveres:=a;
End;






Function szinConvNum2str(i:LongInt):String;
Var a:String;
Begin;
a:=copy(szinKodok,i,1);
if (i<1) or (a='') then a:='?';
if (i=0) then a:='c';
szinConvNum2str:=a;
End;



Function szinConvStr2num(a:String):LongInt;
Var i:LongInt;
Begin;
szinConvStr2num:=-1;
if (length(a)<>1) then exit;
a:=kicsi(a);
if (a='c') then begin; szinConvStr2num:=0;exit; end;
i:=pos(a[1],szinKodok);
if (i<1) then exit;
szinConvStr2num:=i;
End;



Function szinConvNum2hun(i:LongInt):String;
Var a:String;
Begin;
case i of
  0:a:='szintelen';
  1:a:='piros';
  2:a:='tok';
  3:a:='zold';
  4:a:='makk';
  else a:='?';
  end;
szinConvNum2hun:=a;
End;



Function lapConvNum2str(i:LongInt):String;
Var
  a,b:String;
  o:LongInt;
Begin;
o:=(i shr 4) and 15;
i:=i and 15;
a:=copy(szinKodok,o,1);
b:=copy(lapKodok,i,1);
if (o<1) or (a='') then a:='?';
if (i<1) or (b='') then b:='?';
lapConvNum2str:=a+b;
End;



Function lapConvStr2num(a:String):LongInt;
Var i,o:LongInt;
Begin;
lapConvStr2num:=-1;
if (length(a)<>2) then exit;
a:=kicsi(a);
i:=pos(a[1],szinKodok);
if (i<1) then exit;
o:=pos(a[2],lapKodok);
if (o<1) then exit;
lapConvStr2num:=(i shl 4)+o;
End;



Function lapConvNum2hun(i:LongInt):String;
Var a:String;
Begin;
case i and 15 of
  1:a:='het';
  2:a:='nyolc';
  3:a:='kilenc';
  4:a:='tiz';
  5:a:='also';
  6:a:='felso';
  7:a:='kiraly';
  8:a:='asz';
  else a:='?';
  end;
lapConvNum2hun:=szinConvNum2hun(i shr 4)+' '+a;
End;



Function mondasConvStr2num(a:String):LongInt;
Var r:LongInt;
Begin;
mondasConvStr2num:=-1;
kicserel(' ','',a);
a:=kicsi(a);
r:=0;
if (kicserel('p','',a)<>0) then r:=r or mondas_party;
if (kicserel('a','',a)<>0) then r:=r or mondas_4asz;
if (kicserel('4','',a)<>0) then r:=r or mondas_40szaz;
if (kicserel('u','',a)<>0) then r:=r or mondas_ulti;
if (kicserel('b','',a)<>0) then r:=r or mondas_betli;
if (kicserel('2','',a)<>0) then r:=r or mondas_20szaz;
if (kicserel('d','',a)<>0) then r:=r or mondas_durt;
if (kicserel('t','',a)<>0) then r:=r or mondas_4tiz;
if (kicserel('r','',a)<>0) then r:=r or mondas_terit;
if (kicserel('s','',a)<>0) then r:=r or 1;
if (a<>'') then exit;
if (r and mondas_betli<>0) then if (r and minden_szines<>0) then exit;
if (r and minden_partys<>0) then r:=r or mondas_party;
if (r and minden_nparty<>0) then r:=r and (not mondas_party);
if (r and mondas_40szaz<>0) and (r and mondas_20szaz<>0) then r:=r and (not mondas_40szaz);
if (r and minden_jatek=0) then exit;
mondasConvStr2num:=r;
End;



Function mondasConvAlapra(i:LongInt):LongInt;
Label f1;
Begin;
if (i<0) then goto f1;
i:=i and minden_jatek;
if (i=mondas_party) then goto f1;
i:=i and (not mondas_party);
f1:
mondasConvAlapra:=i;
End;



Function mondasConvNum2str(i:LongInt):String;
Var a:String;
Begin;
mondasConvNum2str:='?';
if (i<0) then exit;
a:='';
if (i and mondas_terit<>0) then a:=a+'r';
if (i and mondas_szinMask=1) then a:=a+'s';
if (i and mondas_party<>0) then a:=a+'p';
if (i and mondas_4asz<>0) then a:=a+'a';
if (i and mondas_4tiz<>0) then a:=a+'t';
if (i and mondas_40szaz<>0) then a:=a+'4';
if (i and mondas_20szaz<>0) then a:=a+'2';
if (i and mondas_ulti<>0) then a:=a+'u';
if (i and mondas_betli<>0) then a:=a+'b';
if (i and mondas_durt<>0) then a:=a+'d';
mondasConvNum2str:=a;
End;



Function mondasConvNum2hun(i:LongInt):String;
Var a:String;
Begin;
mondasConvNum2hun:='okorseg a kobon';
if (i<0) then exit;
case i and mondas_szinMask of
  1:a:=' piros';
  2:a:=' tok';
  3:a:=' zold';
  4:a:=' makk';
  else a:='';
  end;
if (i and mondas_terit<>0) then a:=a+' teritett';
if (i and mondas_party<>0) then a:=a+' passz';
if (i and mondas_4asz<>0) then a:=a+' 4asz';
if (i and mondas_4tiz<>0) then a:=a+' 4tiz';
if (i and mondas_40szaz<>0) then a:=a+' 40-100';
if (i and mondas_20szaz<>0) then a:=a+' 20-100';
if (i and mondas_ulti<>0) then a:=a+' ulti';
if (i and mondas_betli<>0) then a:=a+' betli';
if (i and mondas_durt<>0) then a:=a+' durtmars';
if (i and mondas_csszaz<>0) then a:=a+' csendes szaz';
mondasConvNum2hun:=copy(a,2,666);
End;



Function mondasConvNum2ert(i:LongInt):LongInt;
Var o:LongInt;
Begin;
mondasConvNum2ert:=-1;
if (i<0) then exit;
o:=0;
if (i and mondas_party<>0) then inc(o,1);
if (i and mondas_csszaz<>0) then inc(o,2);
if (i and mondas_4asz<>0) then inc(o,3);
if (i and mondas_40szaz<>0) then inc(o,4);
if (i and mondas_ulti<>0) then inc(o,4);
if (i and mondas_betli<>0) then inc(o,5);
if (i and mondas_20szaz<>0) then inc(o,6);
if (i and mondas_durt<>0) then inc(o,7);
if (i and mondas_4tiz<>0) then inc(o,8);
if (i and mondas_szinMask=1) then o:=o*2;
if (i and mondas_terit<>0) then o:=o*2;
mondasConvNum2ert:=o;
End;
