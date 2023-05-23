Procedure doOneTeljesJatek;
Label l1,l2,l3,k1,k2,p1;
Var
  talon:onePakliRecord;
  jatek,kontra,kijon,kezdett,kikezd:LongInt;
  kijatszik,ellenfel1,ellenfel2:LongInt;
  dobasok:array[1..3] of byte;
  fillerek:array[1..3] of LongInt;
  szines:Boolean;



Function kartyaListGen(var u:OnePlayerRecord):String;
Var
  pak:onePakliRecord;
  i:LongInt;
  a:String;
Begin;
generatePakli(0,1,2,3,szines,pak);
sorbarendezPakli(pak,u.kezD,u.kezN);
u.kezN:=u.kezD[0];
a:='';
for i:=1 to u.kezN do a:=a+lapConvNum2str(u.kezD[i]);
kartyaListGen:=a;
End;



Procedure sendKartyak(u:LongInt);
Begin;
uzenetKuldes(u,'lapjaid '+kartyaListGen(jatekosok[u]));
End;



Function dobasAnalizal(var u:OnePlayerRecord;a:String;dn:LongInt):Boolean;
Label kezd,err;
Var
  pak1,pak2:onePakliRecord;

Function szinGet(s:LongInt):Boolean;
Label vege;
Var i,o,p,q,s1:LongInt;
Begin;
szinGet:=false;
s1:=s-1;
p:=s shl 4;
generatePakli(s1,s1,s1,s1,szines,pak2);
for i:=1 to dn-1 do begin;
  o:=dobasok[i];
  if (o shr 4<>s) then continue;
  if (keresesPakliban(pak2,8,o)<=keresesPakliban(pak2,8,p)) then continue;
  p:=o;
  end;
o:=jatek and mondas_szinMask;
if szines and (o<>s) then begin;
  for i:=1 to dn-1 do if (dobasok[i] shr 4=o) then p:=p or $f;
  end;
for i:=1 to keresesPakliban(pak2,8,p) do pak2[i]:=0;
sorbarendezPakli(pak2,pak2,8);
pak1:=u.kezD;
sorbarendezPakli(pak2,pak1,pak1[0]);
if (pak1[0]>0) then goto vege;
generatePakli(s1,s1,s1,s1,szines,pak2);
pak1:=u.kezD;
sorbarendezPakli(pak2,pak1,pak1[0]);
if (pak1[0]=0) then exit;
vege:
szinGet:=true;
End;

Var
  i,o,p:LongInt;
Begin;
dobasAnalizal:=True;
kartyaListGen(u);
pak1:=u.kezD;
if (dn=1) then goto kezd;
if szinGet(dobasok[1] shr 4) then goto kezd;
pak1:=u.kezD;
if szines then if szinGet(jatek and mondas_szinMask) then goto kezd;
pak1:=u.kezD;
kezd:
o:=lapConvStr2num(a);
if (o<0) then goto err;
p:=keresesPakliban(u.kezD,u.kezN,o);
if (p<0) then goto err;
i:=keresesPakliban(pak1,pak1[0],o);
if (i<0) then goto err;
dobasok[dn]:=o;
u.kezD[p]:=0;
kartyaListGen(u);
uzenetKuldes(0,'dobott '+BStr(u.num)+' '+lapConvNum2str(o));
dobasAnalizal:=False;
exit;

err:
a:='';
for i:=1 to pak1[0] do a:=a+lapConvNum2str(pak1[i]);
uzenetKuldes(u.num,'renonc '+a);
End;



Procedure korkezdCsinal;
Begin;
kijon:=kikezd-1;
kezdett:=kikezd;
fillchar(dobasok,sizeof(dobasok),0);
uzenetKuldes(0,'ujkor');
End;



Procedure korvegCsinal;
Var
  pak:onePakliRecord;

Function szinKeres(s:LongInt):LongInt;
Var i,o,p,q:LongInt;
Begin;
q:=0;
p:=0;
for i:=1 to 3 do begin;
  o:=dobasok[i];
  if (o shr 4<>s) then continue;
  o:=keresesPakliban(pak,pak[0],o);
  if (o<=p) then continue;
  q:=((kikezd+i-2) mod 3)+1;
  p:=o;
  end;
szinKeres:=q;
End;

Var
  a:String;
  i,o:LongInt;
Begin;
generatePakli(0,1,2,3,szines,pak);
o:=0;
if szines then o:=szinKeres(jatek and mondas_szinMask);
if (o<1) then o:=szinKeres(dobasok[1] shr 4);
if (o<1) then o:=kikezd;
kikezd:=o;
a:='';
for i:=1 to 3 do a:=a+lapConvNum2str(dobasok[i]);
uzenetKuldes(0,'korveg '+BStr(kikezd)+' '+a);
appendLog('kor '+BStr(kezdett)+' '+a+' '+BStr(kikezd));
move(dobasok,jatekosok[kikezd].vitD[jatekosok[kikezd].vitN+1],3);
inc(jatekosok[kikezd].vitN,3);
End;



Procedure elszamol(u,jt:LongInt;ny,cs:Boolean);
Var
  i,j:LongInt;
  a:String;
Begin;
a:=mondasConvNum2hun(jt);
j:=mondasConvNum2ert(jt);
if (kontra and jt<>0) then begin;
  a:='kontrazott '+a;
  j:=j*2;
  end;
if cs then a:='csendes '+a;
if not ny then j:=-j;
i:=kiAharmadik(u,0);
dec(fillerek[i],j);
i:=kiAharmadik(u,i);
dec(fillerek[i],j);
j:=j*2;
inc(fillerek[u],j);
a:=' a(z) '+a+'-t; '+BStr(j)+' '+penzNeve;
if ny then a:='teljesitette'+a else a:='elbukta'+a;
uzenetKuldes(0,'uzenet '+jatekosok[u].name+' '+a);
End;



Function szamol4asztiz(var u:OnePlayerRecord;m:LongInt):LongInt;
Var i,o,p:LongInt;
Begin;
p:=0;
for i:=1 to u.vitN do if (u.vitD[i] and $f=m) then inc(p);
if not szines then p:=0;
szamol4asztiz:=p;
End;



Function szamolParty(var u:OnePlayerRecord):LongInt;
Var i,o,p:LongInt;
Begin;
p:=0;
o:=u.mond and $ff;
inc(p,(o shr 4)*4);
inc(p,(o and $f)*2);
if (u.num=ellenfel1) then begin;
  move(talon[1],u.vitD[u.vitN+1],2);
  inc(u.vitN,2);
  end;
inc(p,szamol4asztiz(u,4));
inc(p,szamol4asztiz(u,8));
if (u.num=ellenfel1) then dec(u.vitN,2);
if (u.num=kikezd) then inc(p);
if not szines then p:=0;
szamolParty:=p*10;
End;



Function szamolUltimo:LongInt;
Var i,o,p:LongInt;
Begin;
szamolUltimo:=0;
if not szines then exit;
o:=((jatek and mondas_szinMask) shl 4) or 1;
p:=0;
for i:=1 to 3 do begin;
  if (dobasok[i]<>o) then continue;
  p:=((kezdett+i-2) mod 3)+1;
  end;
if (p=0) then exit;
if (p<>kikezd) then p:=-p;
szamolUltimo:=p;
End;





Var
  a,b:String;
  i,o,p,q,r:LongInt;
  pak:onePakliRecord;
Begin;
kezdeniFog:=(kezdeniFog mod 3)+1;
kijon:=kezdeniFog;
kijatszik:=kezdeniFog;
jatek:=0;
kontra:=0;
szines:=false;
WriteLn('--- uj jatek ---');
uzenetKuldes(0,'ujjatek');
a:=doKeveres;
for i:=1 to 3 do begin;
  move(a[1],jatekosok[i].kezD[1],10);
  jatekosok[i].kezN:=10;
  jatekosok[i].vitN:=0;
  a:=copy(a,11,666);
  jatekosok[i].mond:=0;
  end;
move(a[1],jatekosok[kezdeniFog].kezD[11],2);
jatekosok[kezdeniFog].kezN:=12;
pak:=jatekosok[kezdeniFog].kezD;
a:=lapConvNum2str(pak[11])+lapConvNum2str(pak[12]);
uzenetKuldes(kezdeniFog,'talon '+a);
a:='';
for i:=1 to 3 do a:=a+' '+jatekosok[i].name;
appendLog(copy(a,2,666));
for i:=1 to 3 do begin;
  appendLog(kartyaListGen(jatekosok[i]));
  sendKartyak(i);
  end;

{licitalas befogadasa}
l1:
relequish2players;
b:=uzenetOlvasas(kijon);
if (b='') then goto l1;
a:=kicsi(getNextWord(b));
if (a<>'licit') then begin;
  uzenetKuldes(kijon,'hiba licitalj valamit!');
  goto l1;
  end;
a:=getNextWord(b);
o:=lapConvStr2num(copy(a,1,2));
p:=lapConvStr2num(copy(a,3,666));
if (o<0) or (p<0) or (o=p) then begin;
  uzenetKuldes(kijon,'hiba adj meg valodi lapokat!');
  goto l1;
  end;
talon[1]:=o;
talon[2]:=p;
p:=mondasConvStr2num(b);
if (p<0) then begin;
  uzenetKuldes(kijon,'hiba adj meg valodi jatektipust!');
  goto l1;
  end;
o:=mondasConvNum2ert(p);
i:=mondasConvNum2ert(jatek);
if (o=i) then if (p<=jatek) then o:=-666;
if (o<i) then begin;
  uzenetKuldes(kijon,'hiba mondj ertekesebb jatektipust!');
  goto l1;
  end;
pak:=jatekosok[kijon].kezD;
i:=keresesPakliban(pak,12,talon[1]);
o:=keresesPakliban(pak,12,talon[2]);
if (i<0) or (o<0) then begin;
  uzenetKuldes(kijon,'hiba adj meg sajat lapokat!');
  goto l1;
  end;
pak[i]:=0;
pak[o]:=0;
jatekosok[kijon].kezD:=pak;
jatek:=p;
szines:=(jatek and minden_szines<>0);
kijatszik:=kijon;
ellenfel1:=kiAharmadik(kijatszik,0);
ellenfel2:=kiAharmadik(kijatszik,ellenfel1);
appendLog('licit '+BStr(kijatszik)+' '+mondasConvNum2str(jatek)+' '+lapConvNum2str(talon[1])+lapConvNum2str(talon[2]));

{begyujtjuk a meheteket}
l2:
uzenetKuldes(0,'jatek '+BStr(kijatszik)+' '+mondasConvNum2str(jatek));
for i:=1 to 3 do sendKartyak(i);
kijon:=kijatszik;
for p:=1 to 3 do begin;
  kijon:=(kijon mod 3)+1;
  uzenetKuldes(kijon,'szerinted');
  l3:
  relequish2players;
  b:=uzenetOlvasas(kijon);
  if (b='') then goto l3;
  a:=kicsi(getNextWord(b));
  if (a='mehet') then begin;
    uzenetKuldes(0,'mehet '+BStr(kijon));
    continue;
    end;
  if (a<>'felveszem') then begin;
    uzenetKuldes(kijon,'hiba felveszed, vagy sem?!');
    goto l3;
    end;
  uzenetKuldes(0,'felvette '+BStr(kijon));
  uzenetKuldes(kijon,'talon '+lapConvNum2str(talon[1])+lapConvNum2str(talon[2]));
  move(talon[1],jatekosok[kijon].kezD[11],2);
  jatekosok[kijon].kezN:=12;
  sendKartyak(kijon);
  goto l1;
  end;

{megkerdezzuk a szinet, ha van}
if szines and (jatek and mondas_szinMask=0) then begin;
  kijon:=kijatszik;
  uzenetKuldes(kijon,'szine');
  while (1=1) do begin;
    relequish2players;
    b:=uzenetOlvasas(kijon);
    if (b='') then continue;
    a:=kicsi(getNextWord(b));
    if (a<>'szine') then begin;
      uzenetKuldes(kijon,'hiba add meg a szinet!');
      continue;
      end;
    i:=szinConvStr2num(b);
    if (i<0) then begin;
      uzenetKuldes(kijon,'hiba adj valodi szint!');
      continue;
      end;
    jatek:=jatek or i;
    break;
    end;
  if (jatek and mondas_szinMask=1) then goto l2;
  end;

i:=jatek and mondas_szinMask;
if not szines then i:=0;
a:=BStr(kijatszik)+' '+szinConvNum2str(i)+' '+mondasConvNum2str(jatek);
uzenetKuldes(0,'jatszuk '+a);
appendLog('jatek '+a);

{40 es 20 keresese}
if szines then for i:=1 to 3 do begin;
  o:=pakliban20keres(jatekosok[i].kezD,jatek and mondas_szinMask);
  jatekosok[i].mond:=o shl 8;
  end;
p:=0;
if (jatek and mondas_40szaz<>0) then p:=$f0;
if (jatek and mondas_20szaz<>0) then p:=$0f;
if (p<>0) then begin;
  o:=jatekosok[kijatszik].mond shr 8;
  if (o<>0) then o:=(p shr 3) and p;
  for i:=1 to 3 do jatekosok[i].mond:=0;
  jatekosok[kijatszik].mond:=(o shl 8) or o;
  end;

{kezdo kor lejatszasa}
kikezd:=kijatszik;
korkezdCsinal;
for i:=1 to 3 do begin;
  kijon:=(kijon mod 3)+1;
  sendKartyak(kijon);
  uzenetKuldes(kijon,'dobjal');
  k1:
  relequish2players;
  b:=uzenetOlvasas(kijon);
  if (b='') then goto k1;
  a:=kicsi(getNextWord(b));
  if (a='vanegy') then begin;
    if (jatek and mondas_party=0) then goto k1;
    p:=BVal(getNextWord(b));
    o:=BVal(getNextWord(b));
    if (p<0) then p:=0;
    if (o<0) then o:=0;
    q:=jatekosok[kijon].mond shr 8;
    r:=(q shr 4) and $f;
    if (p>r) then p:=r;
    r:=q and $f;
    if (o>r) then o:=r;
    jatekosok[kijon].mond:=(q shl 8) or (p shl 4) or o;
    if (o<1) and (p<1) then goto k1;
    a:=BStr(kijon)+' '+BStr(p)+' '+BStr(o);
    uzenetKuldes(0,'vanegy '+a);
    appendLog('vanegy '+a);
    goto k1;
    end;
  if (a='kontra') then begin;
    if (kijon=kijatszik) then goto k1;
    p:=0;
    while (length(b)>0) do begin;
      o:=mondasConvStr2num(copy(b,1,1));
      b:=copy(b,2,666);
      if (o<0) then continue;
      p:=mondasConvAlapra(o) or p;
      end;
    p:=(p and jatek) and minden_jatek;
    if (p=0) then goto k1;
    kontra:=kontra or p;
    a:=BStr(kijon)+' '+mondasConvNum2str(p);
    uzenetKuldes(0,'kontra '+a);
    appendLog('kontra '+a);
    goto k1;
    end;
  if (a<>'dobok') then begin;
    uzenetKuldes(kijon,'hiba kontrazz, vagy dobjal valamit!');
    goto k1;
    end;
  if dobasAnalizal(jatekosok[kijon],b,i) then goto k1;
  end;
korvegCsinal;

{a hatralevo korok levezenylese}
for q:=1 to 9 do begin;
  korkezdCsinal;
  for p:=1 to 3 do begin;
    kijon:=(kijon mod 3)+1;
    sendKartyak(kijon);
    if (jatek and mondas_terit<>0) and (kijon<>kijatszik) then begin;
      o:=kiAharmadik(kijon,kijatszik);
      uzenetKuldes(kijon,'ellenfel '+kartyaListGen(jatekosok[kijatszik]));
      uzenetKuldes(kijon,'tarsad '+kartyaListGen(jatekosok[o]));
      end;
    uzenetKuldes(kijon,'dobjal');
    k2:
    relequish2players;
    b:=uzenetOlvasas(kijon);
    if (b='') then goto k2;
    if (kicsi(getNextWord(b))<>'dobok') then begin;
      uzenetKuldes(kijon,'hiba dobjal valamit!');
      goto k2;
      end;
    if dobasAnalizal(jatekosok[kijon],b,p) then goto k2;
    end;
  korvegCsinal;
  end;

{vegso elszamolas}
a:='';
for i:=1 to 3 do a:=a+' '+jatekosok[i].name+'='+BStr(szamolParty(jatekosok[i]));
uzenetKuldes(0,'uzenet fogasok:'+a);
fillchar(fillerek,sizeof(fillerek),0);
if (jatek and mondas_party<>0) then begin;
  p:=szamolParty(jatekosok[kijatszik]);
  o:=szamolParty(jatekosok[ellenfel1])+szamolParty(jatekosok[ellenfel2]);
  elszamol(kijatszik,mondas_party,(p>o),false);
  end;
q:=jatek and (mondas_40szaz or mondas_20szaz);
if (jatek and q<>0) then begin;
  p:=szamolParty(jatekosok[kijatszik]);
  elszamol(kijatszik,q,p>=100,false);
  end else begin;
  p:=szamolParty(jatekosok[kijatszik]);
  o:=szamolParty(jatekosok[ellenfel1])+szamolParty(jatekosok[ellenfel2]);
  if (p>=100) then elszamol(kijatszik,mondas_csszaz,true,false);
  if (o>=100) then elszamol(kijatszik,mondas_csszaz,false,false);
  end;
if (jatek and mondas_ulti<>0) then begin;
  elszamol(kijatszik,mondas_ulti,szamolUltimo=kijatszik,false);
  end else begin;
  i:=szamolUltimo;
  if (i<>0) then elszamol(abs(i),mondas_ulti,i>0,true);
  end;
if (jatek and mondas_betli<>0) then begin;
  elszamol(kijatszik,mondas_betli,jatekosok[kijatszik].vitN=0,false);
  end;
if (jatek and mondas_durt<>0) then begin;
  elszamol(kijatszik,mondas_durt,jatekosok[kijatszik].vitN>=30,false);
  end else if szines then begin;
  for i:=1 to 3 do if (jatekosok[i].vitN>=30) then elszamol(i,mondas_durt,true,true);
  end;
if (jatek and mondas_4asz<>0) then begin;
  elszamol(kijatszik,mondas_4asz,szamol4asztiz(jatekosok[kijatszik],8)>=4,false);
  end else begin;
  for i:=1 to 3 do if (szamol4asztiz(jatekosok[i],8)>=4) then elszamol(i,mondas_4asz,true,true);
  end;
if (jatek and mondas_4tiz<>0) then begin;
  elszamol(kijatszik,mondas_4tiz,szamol4asztiz(jatekosok[kijatszik],4)>=4,false);
  end else begin;
  for i:=1 to 3 do if (szamol4asztiz(jatekosok[i],4)>=4) then elszamol(i,mondas_4tiz,true,true);
  end;

p1:
{pontok felszorzasa, elkuldese}
p:=1;
if (jatek and mondas_terit<>0) then p:=p shl 1;
if (jatek and mondas_szinMask=1) then p:=p shl 1;
a:='';
for i:=1 to 3 do begin;
  fillerek[i]:=fillerek[i]*p;
  a:=a+' '+BStr(fillerek[i]);
  inc(jatekosok[i].penz,fillerek[i]);
  end;
uzenetKuldes(0,'jatekveg '+lapConvNum2str(talon[1])+lapConvNum2str(talon[2])+a);
appendLog('vege'+a);
a:='';
for i:=1 to 3 do a:=a+' '+jatekosok[i].name+'='+BStr(jatekosok[i].penz);
uzenetKuldes(0,'uzenet egyenleg:'+a);
End;
