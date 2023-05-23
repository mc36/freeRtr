Const
  InternalNumber=#13;
  SepCh='\';
  SpxCh=':';
  ZeroFill='00000000000000000000000000000000000000000';
Const
  DataSizeNames:Array[1..4] of String[6]=('byte','word','','dword');

Var
  upperShifter:String;
  InStr:String;
  OutStr:String;
  OutOk:Boolean;


Type
  OneMemPointRec=record
    Typ:Byte;
    Reg:LongInt;        {1}
    Num:LongInt;        {2}
    Lab:String;         {3}
    end;
Type
  OneParameterRec=record
    Typ:Byte;
    Str:String[128];
    Reg:LongInt;             {1}
    Num:String[128];         {2}
    Mem:OneMemPointRec;      {3}
    Lab:String[128];         {4}
    Cnd:LongInt;             {5}
    Frg:LongInt;             {6}
    end;

Procedure clreol;
begin;
write(#13'                                                                '#13);
end;

Function b2h(b:Byte):String;
Begin;
b2h:=byte2hextype(b);
End;

Function h2b(b:String):Byte;
Begin;
h2b:=hextype2byte(b);
End;

Function h2w(b:String):Word;
Begin;
h2w:=hextype2byte(Copy(b,1,2))*256+hextype2byte(Copy(b,3,2));
End;

Function w2h(w:Word):String;
Begin;
w2h:=byte2hextype(w div 256)+byte2hextype(w mod 256);
End;


Function RepairOneFileName(a,b:String):String;
Var i,o:Byte;
Begin;
o:=0;
for i:=1 to length(b) do if (b[i]='\') then o:=i;
b:=copy(b,1,o);
if (copy(a,2,1)<>':') and (copy(a,1,1)<>'\') then a:=b+a;
RepairOneFileName:=Kicsi(a);
End;


Function fPos(a,s:String):Byte;
Var
  i,o:LongInt;
Begin;
o:=0;
for i:=1 to Length(s) do if (Copy(s,i,Length(a))=a) then o:=i;
fPos:=o;
End;

Function xLevesz(s:String):String;
Begin;
s:=Levesz(s);
While (Copy(s,1,1)=' ') do Delete(s,1,1);
xLevesz:=s;
End;

Function BenneVan(a,b:String):Boolean;
Begin;
BenneVan:=(Pos('|'+b+'|',a)<>0);
End;


Function RepairOutData(s:String):String;
Begin;
if (Copy(s,1,1)<>SepCh) then s:=SepCh+s;
if (Copy(s,Length(s),1)<>SepCh) then s:=s+SepCh;
RepairOutData:=s;
End;

Function DecodeDataSizeName(s:String):Byte;
Var
  i:Byte;
Begin;
i:=0;
if (s='byte')  then i:=1;
if (s='word')  then i:=2;
if (s='dword') then i:=4;
if (s='fword') then i:=6;
if (s='qword') then i:=8;
if (s='tbyte') then i:=10;
DecodeDataSizeName:=i;
End;


Function Hex2ProciDat(s:String;c:Byte):String;
Var
  a,b:String;
  i,o:LongInt;
Begin;
b:='';
for o:=1 to c do begin;
  i:=Length(s)-2;
  a:=Copy(s,i+1,2);
  s:=Copy(s,1,i);
  b:=a+SepCh+b;
  end;
b:=copy(b,1,length(b)-1);
for i:=1 to Length(s) do if (Copy(s,1,1)<>Copy(s,i,1)) then b:='';
Hex2ProciDat:=b;
End;


Function AnalizeNumbers(s:String):String;
Label Vege;
Var
  i,o,p:LongInt;
  ob:Array[1..4] of Byte absolute o;
  a,b:String;
  c:char;
  Typ:Byte;
Begin;
AnalizeNumbers:='';
if not (s[1] in ['-','+','0'..'9']) then Exit;
if (s[Length(s)] in ['0'..'9']) then s:=s+'d';
While (Copy(s,1,1)='0') and (Length(s)>2) do Delete(s,1,1);
if (Length(s)=0) then Exit;
Case s[Length(s)] of
  'b':Typ:=1;
  'd':Typ:=2;
  'h':Typ:=3;
  InternalNumber:Typ:=4;
  else Exit;
  end;
s:=Nagy(Copy(s,1,Length(s)-1));
if (Typ=4) then begin; b:=s;Goto Vege; end;

if (Typ=1) then begin;
  b:='';
  While (s<>'') do begin;
    a:=Right(ZeroFill+s,8);
    s:=Copy(s,1,Length(s)-8);
    i:=bintype2byte(a);
    if (byte2bintype(i)<>a) then Exit;
    b:=b2h(i)+b;
    end;
  s:=b;
  Typ:=3;
  end;
if (Typ=2) then begin;
  o:=BVal(s);
  if (BStr(o)<>s) then Exit;
  b:='';
  While (o<>0) do begin;
    b:=b2h(ob[1])+b;
    o:=o shr 8;
    end;
  s:=b;
  Typ:=3;
  end;
if (Typ=3) then begin;
  if (Length(s)>8) then Exit;
  s:=Right(ZeroFill+s,8);
  b:=s;
  for i:=4 downto 1 do begin;
    a:=Copy(s,1,2);Delete(s,1,2);
    p:=h2b(a);
    if (b2h(p)<>a) then Exit;
    ob[i]:=p;
    end;
  end;
if (s<>'') then Exit;

Vege:
AnalizeNumbers:=b;
End;


Function AnalizeLabelName(s:String):String;
Var
  i:LongInt;
  a:String;
  Typ:Byte;
Begin;
AnalizeLabelName:='';
Typ:=0;
a:='offset ';
if (Copy(s,1,Length(a))=a) then begin;
  delete(s,1,Length(a));
  Typ:=1;
  end;
a:='segment ';
if (Copy(s,1,Length(a))=a) then begin;
  delete(s,1,Length(a));
  Typ:=2;
  end;
a:='';
for i:=1 to Length(s) do begin;
  if (s[i] in ['_','a'..'z','0'..'9']) then a:=a+s[i];
  end;
if (a[1] in ['0'..'9']) then a:=#13;
if (Length(a)>80) then a:=#13;
if (a=s) then begin;
  a:='';
  if (Typ=1) then a:='ofs'+SpxCh;
  if (Typ=2) then a:='seg'+SpxCh;
  AnalizeLabelName:=a+s;
  end;
End;



Function ConvertNumOrLabDatas(p:OneParameterRec;len:Byte;es:String):String;
Var
  a:String;
Begin;
if (es<>'') then es:=es+SpxCh;
a:='';
if (p.Typ=2) then begin;
  {Num}
  a:=Right(ZeroFill+p.Num,len*2);
  a:=Hex2ProciDat(a,Len);
  end;
if (p.Typ=4) then begin;
  {Label}
  a:=p.Lab;
  a:=DataSizeNames[Len]+SpxCh+'msb'+SpxCh+es+a;
  end;
ConvertNumOrLabDatas:=a;
End;

Function ConvertAllMemoryDatas(p:OneParameterRec;len:Byte):String;
Var a:String;
Begin;
a:='';
if (p.Mem.Typ=1) then begin;
  {Reg}
  a:=Copy(ZeroFill,1,len*2);
  a:=Hex2ProciDat(a,Len);
  end;
if (p.Mem.Typ=2) then begin;
  {Num}
  a:=Right(ZeroFill+w2h(p.Mem.Num),len*2);
  a:=Hex2ProciDat(a,Len);
  end;
if (p.Mem.Typ=3) then begin;
  {Label}
  a:=p.Mem.Lab;
  a:=DataSizeNames[Len]+SpxCh+'msb'+SpxCh+a;
  end;
ConvertAllMemoryDatas:=a;
End;


Procedure AnalizeCpuRegs(a:String;var i:LongInt);
Label f1;
Begin;
if (a[1]<>'r') then begin;
  f1:
  i:=-1;
  exit;
  end;
a:=copy(a,2,255);
i:=BVal(a);
if (i<0) or (i>31) then goto f1;
if (bstr(i)<>a) then goto f1;
End;

Procedure AnalizeFpuRegs(a:String;var i:LongInt);
Label f1;
Begin;
if (a[1]<>'f') then begin;
  f1:
  i:=-1;
  exit;
  end;
a:=copy(a,2,255);
i:=BVal(a);
if (i<0) or (i>31) then goto f1;
if (bstr(i)<>a) then goto f1;
End;

Procedure AnalizeCondRegs(a:String;var i:LongInt);
Label f1;
Begin;
if (a[1]<>'c') then begin;
  f1:
  i:=-1;
  exit;
  end;
if (a[2]<>'r') then goto f1;
a:=copy(a,3,255);
i:=BVal(a);
if (i<0) or (i>7) then goto f1;
if (bstr(i)<>a) then goto f1;
End;


Procedure ParameterPutNumber(Var d:OneParameterRec;i:LongInt);
Begin;
d.Num:=AnalizeNumbers(BStr(i));
d.Typ:=2;
End;


Procedure AnalizeCondBI(Var d:OneParameterRec);
Var
  a:String;
  i:LongInt;
Begin;
if (d.Typ<>4) then exit;
a:=kicsi(d.Lab);
i:=-1;
if (a='lt') then i:=0;
if (a='gt') then i:=1;
if (a='eq') then i:=2;
if (a='so') then i:=3;
if (i<0) then exit;
ParameterPutNumber(d,i);
End;


Procedure AnalizeCondBO(Var d:OneParameterRec);
Var
  a:String;
  i,o:LongInt;
Begin;
if (d.Typ<>4) then exit;
a:=kicsi(d.Lab);
i:=-1;
o:=0;
if (copy(a,1,1)='l') then begin;
  a:=copy(a,2,666);
  o:=1;
  end;
if (a='f') then i:=4;
if (a='fz') then i:=2;
if (a='fnz') then i:=0;
if (a='t') then i:=12;
if (a='tz') then i:=10;
if (a='tnz') then i:=8;
if (a='a') then begin; i:=20;o:=0; end;
if (a='z') then i:=18;
if (a='nz') then i:=16;
if (i<0) then exit;
ParameterPutNumber(d,i);
End;


Procedure AnalizeSPRname(Var d:OneParameterRec);
Var
  a:String;
  i:LongInt;
Begin;
if (d.Typ<>4) then exit;
a:=kicsi(d.Lab);
i:=-1;
if (a='mq') then i:=$000;
if (a='xer') then i:=$001;
if (a='vrtcu') then i:=$004;
if (a='vrtcl') then i:=$005;
if (a='udecr') then i:=$006;
if (a='lr') then i:=$008;
if (a='ctr') then i:=$009;
if (a='dsisr') then i:=$012;
if (a='dar') then i:=$013;
if (a='rtcu') then i:=$014;
if (a='rtcl') then i:=$015;
if (a='decr') then i:=$016;
if (a='sdr1') then i:=$019;
if (a='srr0') then i:=$01a;
if (a='srr1') then i:=$01b;
if (a='pid') then i:=$030;
if (a='decar') then i:=$036;
if (a='csrr0') then i:=$03a;
if (a='csrr1') then i:=$03b;
if (a='dear') then i:=$03d;
if (a='esr') then i:=$03e;
if (a='ivpr') then i:=$03f;
if (a='eie') then i:=$050;
if (a='eid') then i:=$051;
if (a='nre') then i:=$052;
if (a='cmpa') then i:=$090;
if (a='cmpb') then i:=$091;
if (a='cmpc') then i:=$092;
if (a='cmpd') then i:=$093;
if (a='icr') then i:=$094;
if (a='der') then i:=$094;
if (a='counta') then i:=$096;
if (a='countb') then i:=$097;
if (a='cmpe') then i:=$098;
if (a='cmpf') then i:=$099;
if (a='cmpg') then i:=$09a;
if (a='cmph') then i:=$09b;
if (a='lctrl1') then i:=$09c;
if (a='lctrl2') then i:=$09d;
if (a='ictrl') then i:=$09e;
if (a='bar') then i:=$09f;
if (a='vrsave') then i:=$100;
if (a='usprg0') then i:=$100;
if (a='usprg4') then i:=$104;
if (a='usprg5') then i:=$105;
if (a='usprg6') then i:=$106;
if (a='usprg7') then i:=$107;
if (a='vtbl') then i:=$10c;
if (a='vtbu') then i:=$10d;
if (a='sprg0') then i:=$110;
if (a='sprg1') then i:=$111;
if (a='sprg2') then i:=$112;
if (a='sprg3') then i:=$113;
if (a='sprg4') then i:=$114;
if (a='scomc') then i:=$114;
if (a='sprg5') then i:=$115;
if (a='scomd') then i:=$115;
if (a='sprg6') then i:=$116;
if (a='sprg7') then i:=$117;
if (a='asr') then i:=$118;
if (a='ear') then i:=$11a;
if (a='tbl') then i:=$11c;
if (a='tbu') then i:=$11d;
if (a='svr') then i:=$11e;
if (a='pir') then i:=$11e;
if (a='pvr') then i:=$11f;
if (a='hsprg0') then i:=$130;
if (a='dbsr') then i:=$130;
if (a='hsprg1') then i:=$131;
if (a='dbcr0') then i:=$134;
if (a='ibcr') then i:=$135;
if (a='dbcr1') then i:=$135;
if (a='dbcr') then i:=$136;
if (a='hdec') then i:=$136;
if (a='dbcr2') then i:=$136;
if (a='hior') then i:=$137;
if (a='mbar') then i:=$137;
if (a='rmor') then i:=$138;
if (a='iac1') then i:=$138;
if (a='hrmor') then i:=$139;
if (a='iac2') then i:=$139;
if (a='hssr0') then i:=$13a;
if (a='iac3') then i:=$13a;
if (a='hssr1') then i:=$13b;
if (a='iac4') then i:=$13b;
if (a='lpcr') then i:=$13c;
if (a='dac1') then i:=$13c;
if (a='lpidr') then i:=$13d;
if (a='dabr2') then i:=$13d;
if (a='dac2') then i:=$13d;
if (a='dvc1') then i:=$13e;
if (a='dvc2') then i:=$13f;
if (a='tsr') then i:=$150;
if (a='tcr') then i:=$154;
if (a='ivor0') then i:=$190;
if (a='ivor1') then i:=$191;
if (a='ivor2') then i:=$192;
if (a='ivor3') then i:=$193;
if (a='ivor4') then i:=$194;
if (a='ivor5') then i:=$195;
if (a='ivor6') then i:=$196;
if (a='ivor7') then i:=$197;
if (a='ivor8') then i:=$198;
if (a='ivor9') then i:=$199;
if (a='ivor10') then i:=$19a;
if (a='ivor11') then i:=$19b;
if (a='ivor12') then i:=$19c;
if (a='ivor13') then i:=$19d;
if (a='ivor14') then i:=$19e;
if (a='ivor15') then i:=$19f;
if (a='ibat0u') then i:=$210;
if (a='ibat0l') then i:=$211;
if (a='ibat1u') then i:=$212;
if (a='ibat1l') then i:=$213;
if (a='ibat2u') then i:=$214;
if (a='ibat2l') then i:=$215;
if (a='ibat3u') then i:=$216;
if (a='ibat3l') then i:=$217;
if (a='dbat0u') then i:=$218;
if (a='dbat0l') then i:=$219;
if (a='dbat1u') then i:=$21a;
if (a='dbat1l') then i:=$21b;
if (a='dbat2u') then i:=$21c;
if (a='dbat2l') then i:=$21d;
if (a='dbat3u') then i:=$21e;
if (a='dbat3l') then i:=$21f;
if (a='ibat4u') then i:=$230;
if (a='ibat4l') then i:=$231;
if (a='ibat5u') then i:=$232;
if (a='ibat5l') then i:=$233;
if (a='ibat6u') then i:=$234;
if (a='ibat6l') then i:=$235;
if (a='ibat7u') then i:=$236;
if (a='ibat7l') then i:=$237;
if (a='dbat4u') then i:=$238;
if (a='dbat4l') then i:=$239;
if (a='dbat5u') then i:=$23a;
if (a='dbat5l') then i:=$23b;
if (a='dbat6u') then i:=$23c;
if (a='dbat6l') then i:=$23d;
if (a='dbat7u') then i:=$23e;
if (a='dbat7l') then i:=$23f;
if (a='immr') then i:=$27e;
if (a='inv0') then i:=$370;
if (a='inv1') then i:=$371;
if (a='inv2') then i:=$372;
if (a='inv3') then i:=$373;
if (a='ivt0') then i:=$374;
if (a='ivt1') then i:=$375;
if (a='ivt2') then i:=$376;
if (a='ivt3') then i:=$377;
if (a='dnv0') then i:=$390;
if (a='dnv1') then i:=$391;
if (a='dnv2') then i:=$392;
if (a='dnv3') then i:=$393;
if (a='dvt0') then i:=$394;
if (a='dvt1') then i:=$395;
if (a='dvt2') then i:=$396;
if (a='dvt3') then i:=$397;
if (a='dvlim') then i:=$398;
if (a='ivlim') then i:=$399;
if (a='rstcfg') then i:=$39b;
if (a='dcbtrl') then i:=$39c;
if (a='dcbtrh') then i:=$39d;
if (a='icbtrl') then i:=$39e;
if (a='icbtrh') then i:=$39f;
if (a='ummcr0') then i:=$3a8;
if (a='upmc1') then i:=$3a9;
if (a='upmc2') then i:=$3aa;
if (a='usia') then i:=$3ab;
if (a='ummcr1') then i:=$3ac;
if (a='upmc3') then i:=$3ad;
if (a='upmc4') then i:=$3ae;
if (a='usda') then i:=$3af;
if (a='zpr') then i:=$3b0;
if (a='pid') then i:=$3b1;
if (a='mmucr') then i:=$3b2;
if (a='ccr0') then i:=$3b3;
if (a='iac3') then i:=$3b4;
if (a='iac4') then i:=$3b5;
if (a='dvc1') then i:=$3b6;
if (a='dvc2') then i:=$3b7;
if (a='mmcr0') then i:=$3b8;
if (a='pmc1') then i:=$3b9;
if (a='sgr') then i:=$3b9;
if (a='pmc2') then i:=$3ba;
if (a='dcwr') then i:=$3ba;
if (a='sia') then i:=$3bb;
if (a='sler') then i:=$3bb;
if (a='mmcr1') then i:=$3bc;
if (a='su0r') then i:=$3bc;
if (a='pmc3') then i:=$3bd;
if (a='dbcr1') then i:=$3bd;
if (a='pmc4') then i:=$3be;
if (a='sda') then i:=$3bf;
if (a='vtbl') then i:=$3cc;
if (a='vtbu') then i:=$3cd;
if (a='dmiss') then i:=$3d0;
if (a='dcmp') then i:=$3d1;
if (a='dhash1') then i:=$3d2;
if (a='dhash2') then i:=$3d3;
if (a='icdbdr') then i:=$3d3;
if (a='imiss') then i:=$3d4;
if (a='esr') then i:=$3d4;
if (a='icmp') then i:=$3d5;
if (a='dear') then i:=$3d5;
if (a='rpa') then i:=$3d6;
if (a='evpr') then i:=$3d6;
if (a='cdbcr') then i:=$3d7;
if (a='tcr') then i:=$3d8;
if (a='tsr') then i:=$3d8;
if (a='ibr') then i:=$3da;
if (a='tcr') then i:=$3da;
if (a='esasr') then i:=$3db;
if (a='pit') then i:=$3db;
if (a='tbl') then i:=$3dc;
if (a='tbu') then i:=$3dd;
if (a='sebr') then i:=$3de;
if (a='srr2') then i:=$3de;
if (a='ser') then i:=$3df;
if (a='srr3') then i:=$3df;
if (a='hid0') then i:=$3f0;
if (a='dbsr') then i:=$3f0;
if (a='hid1') then i:=$3f1;
if (a='iabr') then i:=$3f2;
if (a='dbcr0') then i:=$3f2;
if (a='hid2') then i:=$3f2;
if (a='hid2') then i:=$3f3;
if (a='dbdr') then i:=$3f3;
if (a='iac1') then i:=$3f4;
if (a='dabr') then i:=$3f5;
if (a='iac2') then i:=$3f5;
if (a='hid5') then i:=$3f5;
if (a='dac1') then i:=$3f6;
if (a='dac2') then i:=$3f7;
if (a='l2pm') then i:=$3f8;
if (a='hid2') then i:=$3f8;
if (a='l2cr') then i:=$3f9;
if (a='iabr2') then i:=$3fa;
if (a='dccr') then i:=$3fa;
if (a='ictc') then i:=$3fb;
if (a='iccr') then i:=$3fb;
if (a='thrm1') then i:=$3fc;
if (a='pbl1') then i:=$3fc;
if (a='sp') then i:=$3fd;
if (a='thrm2') then i:=$3fd;
if (a='pbu1') then i:=$3fd;
if (a='lt') then i:=$3fe;
if (a='thrm3') then i:=$3fe;
if (a='fpecr') then i:=$3fe;
if (a='pbl2') then i:=$3fe;
if (a='pir') then i:=$3ff;
if (a='pbu2') then i:=$3ff;
if (a='hid15') then i:=$3ff;
if (i<0) then exit;
ParameterPutNumber(d,i);
End;


Procedure AnalizeMemPoint(s:String;var m:OneMemPointRec);
Var
  a,b,c:String;
  i,o:LongInt;
begin;
m.Typ:=0;
m.Reg:=0;
m.Num:=0;
m.Lab:='';
i:=pos('(',s);
o:=pos(')',s);
if (i<1) then exit;
if (o<>length(s)) then exit;
a:=copy(s,i+1,o-i-1);
AnalizeCpuRegs(copy(s,i+1,o-i-1),o);
if (o<0) then exit;
m.Reg:=o;
s:=copy(s,1,i-1);

s:='+'+s;
Kicserel(' +','+',s);
Kicserel('+ ','+',s);
Kicserel(' -','-',s);
Kicserel('- ','-',s);

repeat
  b:=s;
  Kicserel('-+','-',s);
  Kicserel('+-','-',s);
  Kicserel('--','+',s);
  Kicserel('++','+',s);
  until (s=b);

s:=xLevesz(s);

if (s='') then begin;
  m.Typ:=1;
  exit;
  end;

c:=Copy(s,1,1);
Delete(s,1,1);
if (c='-') or (c='+') then else Exit;

a:=AnalizeNumbers(s);
if (a<>'') then begin;
  b:=Right(a,8);
  a:=Copy(b,1,4);
  i:=h2w(a);
  i:=i shl 16;
  i:=i+h2w(Copy(b,5,4));
  if (c='-') then i:=-i;
  m.Num:=i;
  m.Typ:=2;
  Exit;
  end;

if (c<>'+') then Exit;
a:=AnalizeLabelName(s);
if (a<>'') then begin;
  m.Lab:=a;
  m.Typ:=3;
  Exit;
  end;

End;







Procedure AnalizeParameter(s:String;Var d:OneParameterRec);
Var
  m:OneMemPointRec;
  a:String;
  i:LongInt;
Begin;
d.Typ:=0;
d.Str:=s;
if (s='') then exit;
AnalizeCpuRegs(s,i);
if (i>=0) then begin; d.Typ:=1;d.Reg:=i;Exit; end;
AnalizeCondRegs(s,i);
if (i>=0) then begin; d.Typ:=5;d.Cnd:=i;Exit; end;
AnalizeFpuRegs(s,i);
if (i>=0) then     begin; d.Typ:=6;d.Frg:=i;Exit; end;
a:=AnalizeNumbers(s);
if (a<>'') then     begin; d.Typ:=2;d.Num:=a;Exit; end;
AnalizeMemPoint(s,m);
if (m.Typ<>0) then begin; d.Typ:=3;d.Mem:=m;Exit; end;
a:=AnalizeLabelName(s);
if (a<>'') then     begin; d.Typ:=4;d.Lab:=a;Exit; end;
End;
