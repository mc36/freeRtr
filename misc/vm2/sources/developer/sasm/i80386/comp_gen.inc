Const
  InternalNumber=#13;
  SepCh='\';
  SpxCh=':';
  ZeroFill='00000000000000000000000000000000000000000';
Const
  DataSizeNames:Array[1..4] of String[6]=('byte','word','','dword');

Var
  CurrentCodeStyle:Byte;
  InStr:String;
  OutStr:String;
  OutOk:Boolean;


Type
  OneRegType=record
    Typ:Byte;{0=NoReg 1=DataReg 2=PointReg 3=SegReg 5-FpuReg 6-MmxReg}
    Pro:Byte;{0=__ 1=e__}
    Num:Byte;{1=a 2=c 3=d 4=b | 1=sp 2=bp 3=si 4=di | 1=es 2=cs 3=ss 4=ds}
             {1=fs 2=gs | 0=st(0)..7=st(7) | 0=mmx0..7=mmx7}
    Val:Byte;{1=l 2=h 3=x 201-st, 202=mmx}
    Len:Byte;{1,2,4=cpu, 201=fpu, 202=mmx}
    end;
Type
  OneMemPointRec=record
    Typ:Byte;
    Mode:Byte;             {1-16 bit  2-32 bit}
    SegR:OneRegType;
    o_Num:LongInt;         {2}
    o_Lab:String;          {3}
    o_Sum:Byte;
    {16 bit}
    o_bx:Byte;             {1}
    o_bp:Byte;
    o_si:Byte;
    o_di:Byte;
    {32 bit}
    o_Base:ShortInt;
    o_Idx:ShortInt;
    o_Mul:ShortInt;
    end;
Type
  OneParameterRec=record
    Typ:Byte;
    Str:String[128];
    Reg:OneRegType;          {1=cpu, 5=fpu, 6=mmx}
    Num:String[128];         {2}
    Mem:OneMemPointRec;      {3}
    Lab:String[128];         {4}
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
  b:=b+SepCh+a;
  end;
delete(b,1,1);
Hex2ProciDat:=b;
End;


Function AnalizeCpuConditions(s:String):LongInt;
Var
  i:LongInt;
Begin;
i:=-1;
if (s='nbe') then s:='a';
if (s='ae')  then s:='nb';
if (s='nae') then s:='b';
if (s='na')  then s:='be';
if (s='c')   then s:='b';
if (s='z')   then s:='e';
if (s='nle') then s:='g';
if (s='ge')  then s:='nl';
if (s='nge') then s:='l';
if (s='ng')  then s:='le';
if (s='nc')  then s:='nb';
if (s='nz')  then s:='ne';
if (s='po')  then s:='np';
if (s='pe')  then s:='p';

if (s='a')   then i:=$7;
if (s='b')   then i:=$2;
if (s='nb')  then i:=$3;
if (s='be')  then i:=$6;
if (s='e')   then i:=$4;
if (s='g')   then i:=$f;
if (s='nl')  then i:=$d;
if (s='l')   then i:=$c;
if (s='le')  then i:=$e;
if (s='ne')  then i:=$5;
if (s='no')  then i:=$1;
if (s='ns')  then i:=$9;
if (s='np')  then i:=$b;
if (s='o')   then i:=$0;
if (s='p')   then i:=$a;
if (s='s')   then i:=$8;
AnalizeCpuConditions:=i;
End;

Function AnalizeFpuConditions(s:String):LongInt;
Var
  i:LongInt;
Begin;
i:=-1;
if (s='c')   then s:='b';
if (s='nae') then s:='b';
if (s='ae')  then s:='nb';
if (s='nc')  then s:='nb';
if (s='z')   then s:='e';
if (s='nz')  then s:='ne';
if (s='p')   then s:='u';
if (s='pe')  then s:='u';
if (s='np')  then s:='nu';
if (s='po')  then s:='nu';
if (s='na')  then s:='be';
if (s='a')   then s:='nbe';

if (s='b')   then i:=0;
if (s='e')   then i:=1;
if (s='be')  then i:=2;
if (s='u')   then i:=3;
if (s='nb')  then i:=4;
if (s='ne')  then i:=5;
if (s='nbe') then i:=6;
if (s='nu')  then i:=7;
AnalizeFpuConditions:=i;
End;


Procedure AddNativeOpCode(len,add:byte);
Begin;
if (CurrentCodeStyle=1) then len:=6-len;
if (len<>4) then Exit;
OutStr:=b2h($66+add)+SepCh+OutStr;
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
  a:=DataSizeNames[Len]+SpxCh+es+a;
  end;
ConvertNumOrLabDatas:=a;
End;


Procedure AnalizeNoRegs(Var d:OneRegType);
Begin;
d.Typ:=0;
d.Num:=0;
d.Val:=0;
d.Pro:=0;
d.Len:=0;
End;

Procedure AnalizeDataRegs(s:String;Var d:OneRegType);
Begin;
AnalizeNoRegs(d);
if (Copy(s,1,1)='e') then begin;
  Delete(s,1,1);
  d.Pro:=1;
  end;
if (Length(s)<>2) then Exit;
Case s[1] of
  'a':d.Num:=1;
  'c':d.Num:=2;
  'd':d.Num:=3;
  'b':d.Num:=4;
  end;
Case s[2] of
  'l':begin; d.Val:=1;d.Len:=1; end;
  'h':begin; d.Val:=2;d.Len:=1; end;
  'x':begin; d.Val:=3;d.Len:=2; end;
  end;
if (d.Pro=1) and (d.Val<>3) then AnalizeNoRegs(d);
if (d.Pro=1) then d.Len:=4;
if (d.Num<>0) and (d.Val<>0) then d.Typ:=1;
End;

Procedure AnalizePointRegs(s:String;Var d:OneRegType);
Begin;
AnalizeNoRegs(d);
if (Copy(s,1,1)='e') then begin;
  Delete(s,1,1);
  d.Pro:=1;
  end;
if (Length(s)<>2) then Exit;
if (s='sp') then d.Num:=1;
if (s='bp') then d.Num:=2;
if (s='si') then d.Num:=3;
if (s='di') then d.Num:=4;
d.Len:=2;
if (d.Pro=1) then d.Len:=4;
if (d.Num<>0) then d.Typ:=2;
End;

Procedure AnalizeSegRegs(s:String;Var d:OneRegType);
Begin;
AnalizeNoRegs(d);
if (Length(s)<>2) then Exit;
if (s='es') then d.Num:=1;
if (s='cs') then d.Num:=2;
if (s='ss') then d.Num:=3;
if (s='ds') then d.Num:=4;
if (s='fs') then begin; d.Pro:=1;d.Num:=1; end;
if (s='gs') then begin; d.Pro:=1;d.Num:=2; end;
d.Len:=2;
if (d.Num<>0) then d.Typ:=3;
End;


Procedure AnalizeCTDregisters(s:String;Var Fam,Num:LongInt);
Label ErrVege;
Var
  a:String;
  i:LongInt;
Begin;
Fam:=0;
a:=Copy(s,1,2);
if (a='cr') then Fam:=1;
if (a='dr') then Fam:=2;
if (a='tr') then Fam:=3;
if (Fam=0) then Goto ErrVege;
Delete(s,1,2);
i:=Bval(s);
if (BStr(i)<>s) or (i<0) or (i>7) then Goto ErrVege;
Num:=i;
Exit;

ErrVege:
Fam:=0;
Num:=0;
End;

Procedure AnalizeFPUregisters(s:String;Var d:OneRegType);
Const RegBeg='st(';
Var o:Byte;
Begin;
AnalizeNoRegs(d);
if (s='st') then s:='st(0)';
if (Copy(s,1,Length(RegBeg))<>RegBeg) then Exit;
Delete(s,1,Length(RegBeg));
if (s[0]<>#2) then Exit;
o:=Ord(s[1])-$30;
if (o>7) then Exit;
d.Num:=o;
d.Typ:=5;
d.Pro:=0;
d.Val:=201;
d.Len:=201;
End;

Procedure AnalizeMMXregisters(s:String;Var d:OneRegType);
Const RegBeg='mm';
Var o:Byte;
Begin;
AnalizeNoRegs(d);
if (Copy(s,1,Length(RegBeg))<>RegBeg) then Exit;
Delete(s,1,Length(RegBeg));
if (s[0]<>#1) then Exit;
o:=Ord(s[1])-$30;
if (o>7) then Exit;
d.Num:=o;
d.Typ:=6;
d.Pro:=0;
d.Val:=202;
d.Len:=202;
End;

Procedure AnalizeAnyRegs(s:String;Var d:OneRegType);
Begin;
AnalizeDataRegs(s,d);
if (d.Typ<>0) then Exit;
AnalizePointRegs(s,d);
if (d.Typ<>0) then Exit;
AnalizeSegRegs(s,d);
if (d.Typ<>0) then Exit;
AnalizeNoRegs(d);
End;


Function b8_RegsToQueue(d:OneRegType):LongInt;
Var i:LongInt;
Begin;
i:=(d.Val-1)*4+d.Num;
if (d.Num=0) then i:=0;
if (d.Typ<>1) then i:=0;
if (d.Val>2) then i:=0;
b8_RegsToQueue:=i;
End;

Function b16_RegsToQueue(d:OneRegType):LongInt;
Var
  i:LongInt;
Begin;
i:=(d.Typ-1)*4+d.Num;
if (d.Num=0) then i:=0;
b16_RegsToQueue:=i;
End;

Function int_RegsToQueue(d:OneRegType):LongInt;
Var i:LongInt;
Begin;
if (d.len=1) then i:=b8_RegsToQueue(d) else i:=b16_RegsToQueue(d);
int_RegsToQueue:=i;
End;

Function All_RegsToQueue(d:OneRegType):LongInt;
Var
  i:LongInt;
  id:Array[1..4] of Byte Absolute i;
Begin;
i:=0;
id[1]:=d.Pro;
id[2]:=d.Num;
id[3]:=d.Val;
All_RegsToQueue:=i;
End;




Procedure AnalizeMemPoint(s:String;Var d:OneMemPointRec);
Label f1;
Var
  i,o,l:LongInt;
  a,b,c:String;
  r:OneRegType;
  ExpRegs:Array[1..8] of Byte;

Function te1(n:LongInt):LongInt;
Begin;
if (n>1) or (n<0) then n:=1 else n:=0;
te1:=n;
End;

Function te2(n:LongInt):Boolean;
Begin;
te2:=(n in [0,1,2,4,8]);
End;


Begin;
FillChar(d,SizeOf(d),0);
d.Typ:=0;
d.Mode:=0;
i:=Pos(':',s);
a:=Copy(s,1,i-1);
s:=Copy(s,i+1,255);
s:=xLevesz(s);
if (Copy(s,1,1)<>'[') then Exit;
if (Copy(s,Length(s),1)<>']') then Exit;
s:=Copy(s,2,Length(s)-2);

a:=xLevesz(a);
s:=xLevesz(s);

AnalizeAnyRegs(a,r);
if (a='def') then begin;
  AnalizeNoRegs(r);
  r.Typ:=0;
  end else begin;
  if (r.Typ<>3) then Exit;
  end;
d.SegR:=r;

s:='+'+s;
Kicserel(' *','*',s);
Kicserel('* ','*',s);
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

i:=Kicserel('*+','',s)+Kicserel('*-','',s)+
   Kicserel('+*','',s)+Kicserel('-*','',s)+
   Kicserel('**','',s);
if (i<>0) then Exit;
if (Copy(s,Length(s),1)='*') then Exit;


Kicserel('*',#13,s);Kicserel(#13,' * ',s);
Kicserel('+',#13,s);Kicserel(#13,' + ',s);
Kicserel('-',#13,s);Kicserel(#13,' - ',s);

s:='  '+s+'  ';

Repeat
  l:=Pos(' * ',s);
  if (l<>0) then begin;
    Delete(s,l,3);
    o:=0;
    for i:=1 to l do if (Copy(s,i,1)=' ') then o:=i;
    c:=Copy(s,1,o);
    Delete(s,1,o);
    Dec(l,o);

    o:=Length(s)+1;
    for i:=Length(s) downto l do if (Copy(s,i,1)=' ') then o:=i;
    b:=Copy(s,o,255);
    s:=Copy(s,1,o-1);

    a:=Copy(s,1,l-1);
    s:=Copy(s,l,255);
    o:=Bval(a);
    if (o=0) then begin;
      o:=Bval(s);
      s:=a;
      end;
    if (o<1) then Exit;
    if (Right(c,3)<>' + ') then Exit;

    a:='';
    for i:=1 to o do a:=a+' + '+s;
    delete(a,1,3);

    s:=c+a+b;
    l:=1;
    end;
  until (l=0);

{16 bit}
d.o_bx:=Kicserel('+ bx ','',s);
d.o_bp:=Kicserel('+ bp ','',s);
d.o_si:=Kicserel('+ si ','',s);
d.o_di:=Kicserel('+ di ','',s);

{32 bit}
ExpRegs[1]:=Kicserel('+ eax ','',s);
ExpRegs[2]:=Kicserel('+ ecx ','',s);
ExpRegs[3]:=Kicserel('+ edx ','',s);
ExpRegs[4]:=Kicserel('+ ebx ','',s);
ExpRegs[5]:=Kicserel('+ esp ','',s);
ExpRegs[6]:=Kicserel('+ ebp ','',s);
ExpRegs[7]:=Kicserel('+ esi ','',s);
ExpRegs[8]:=Kicserel('+ edi ','',s);
s:=xLevesz(s);

Kicserel(' +','+',s);
Kicserel('+ ','+',s);
Kicserel(' -','-',s);
Kicserel('- ','-',s);


o:=d.o_bx+d.o_bp+d.o_si+d.o_di;
i:=0;
for l:=1 to 8 do inc(i,ExpRegs[l]);

if (i<>0) and (o<>0) then Exit;

d.o_base:=-1;
d.o_idx:=-1;

if (o<>0) then begin;
  d.Mode:=1;  {16 bit}
  d.o_Sum:=o;
  if (o>2) then Exit;

  i:=te1(d.o_bx)+te1(d.o_bp)+te1(d.o_si)+te1(d.o_di);
  if (i<>0) then Exit;

  if (d.o_bx>0) and (d.o_bp>0) then Exit;
  if (d.o_si>0) and (d.o_di>0) then Exit;

  if (d.o_bp=o) and (s='') then s:='+0';

  o:=1;i:=0;
  end;
if (i<>0) then begin;
  d.Mode:=2;  {32 bit}
  i:=0;
  for l:=1 to 8 do if (ExpRegs[l]<>0) then begin; o:=l;inc(i); end;

  l:=ExpRegs[o];
  if (i=1) and (l mod 2=1) and (l>2) then begin;
    d.o_Base:=o-1;
    dec(l);
    if not te2(l) then Exit;
    d.o_Idx:=o-1;
    d.o_Mul:=l;
    Goto f1;
    end;

  i:=0;
  for l:=1 to 8 do begin;
    o:=ExpRegs[l];
    if not te2(o) then Exit;
    if (o<>0) then Inc(i);
    end;
  if (i>2) then Exit;
  d.o_Sum:=i;

  i:=0;
  for l:=1 to 8 do inc(i,te1(ExpRegs[l]));
  if (i>1) then Exit;

  o:=0;
  for i:=1 to 8 do if (ExpRegs[i]=1) then o:=i;
  d.o_Base:=o-1;
  ExpRegs[o]:=0;

  o:=0;
  for i:=1 to 8 do if (ExpRegs[i]>0) then o:=i;
  d.o_Idx:=o-1;
  i:=ExpRegs[o];if (o=0) then i:=0;
  d.o_Mul:=i;

  f1:
  o:=0;i:=1;
  end;

if (s='') then begin;
  d.Typ:=1;
  Exit;
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
  d.o_Num:=i;
  if (d.Mode=0) then d.Mode:=CurrentCodeStyle+1;
  d.Typ:=2;
  Exit;
  end;

if (c<>'+') then Exit;
a:=AnalizeLabelName(s);
if (a<>'') then begin;
  if (d.Mode=0) then d.Mode:=CurrentCodeStyle+1;
  d.o_Lab:=a;
  d.Typ:=3;
  Exit;
  end;

End;





Function ConvertAllMemoryDatas(d:OneMemPointRec;Plus:Byte):String;
Label Mode1,Mode2,Vege;
Var
  a,b,c:String;
  i,o,cod,len:LongInt;
  pp:OneParameterRec;
Begin;
ConvertAllMemoryDatas:='';
if (d.Typ=0) then Exit;
if (d.Mode=1) then Goto Mode1;
if (d.Mode=2) then Goto Mode2;
Exit;

Mode1:
Len:=-1;
if (d.Typ=1) then Len:=0;
if (d.Typ=2) then begin;
  {Num}
  Len:=2;
  i:=d.o_Num;
  if (-128<=i) and (i<=127) then Len:=1;
  if (i=0) and (d.o_Sum<>d.o_bp) then Len:=0;
  end;
if (d.Typ=3) then Len:=2;
if (d.o_Sum=0) then Len:=2;
if (Len=-1) then Exit;
cod:=-1;
if (d.o_Sum=0) then cod:=$06;
if (d.o_Sum=1) then begin;
  {just one register}
  if (d.o_si=1) then cod:=$04;
  if (d.o_di=1) then cod:=$05;
  if (d.o_bp=1) then cod:=$06;
  if (d.o_bx=1) then cod:=$07;
  end;
if (d.o_Sum=2) then begin;
  {two registers}
  if (d.o_bx=1) and (d.o_si=1) then cod:=$00;
  if (d.o_bx=1) and (d.o_di=1) then cod:=$01;
  if (d.o_bp=1) and (d.o_si=1) then cod:=$02;
  if (d.o_bp=1) and (d.o_di=1) then cod:=$03;
  end;
if (cod=-1) then Exit;

i:=Len*$40+Cod;
if (d.o_Sum=0) then i:=Cod;
b:=b2h(i);

if (Len>0) then begin;
  i:=-1;
  if (d.Typ=2) then begin;
    i:=2;
    a:=BStr(d.o_Num);
    a:=AnalizeNumbers(a);
    pp.Num:=a;
    end;
  if (d.Typ=3) then begin;
    i:=4;
    pp.Lab:=d.o_Lab;
    end;
  if (i=-1) then Exit;
  pp.Typ:=i;
  a:=ConvertNumOrLabDatas(pp,Len,'ofs?');
  if (a='') then Exit;
  b:=b+SepCh+a;
  end;
Goto Vege;


Mode2:
Len:=0;
if (d.o_Base=-1) and (d.Typ=1) then begin; d.o_Num:=0;d.Typ:=2; end;
a:='';
if (d.Typ=2) then begin;
  i:=d.o_Num;
  Len:=4;
  if (-128<=i) and (i<=127) then Len:=1;
  if (d.o_Base=-1) then Len:=4;
  a:=w2h(i shr 16)+w2h(i and $ffff);
  a:=Right(a,Len*2);
  b:='';
  for i:=1 to Len do b:=SepCh+Copy(a,i*2-1,2)+b;
  a:=b;
  end;
if (d.Typ=3) then begin;
  Len:=4;
  a:=SepCh+DataSizeNames[Len]+SpxCh+'ofs?'+SpxCh+d.o_Lab;
  end;
c:=a;
Cod:=d.o_Mul;
o:=-1;
Case Cod of
  0:o:=0;
  1:o:=1;
  2:o:=2;
  4:o:=3;
  8:o:=4;
  end;
if (o=-1) then Exit;
Cod:=o;

b:='';
if (d.o_base=-1) and (d.o_idx=-1) then begin;
  b:=b2h($05)+c;
  Goto Vege;
  end;
if (d.o_Base=-1) then begin;
  b:=b2h($04)+SepCh+b2h($5+d.o_Idx*$8+(Cod-1)*$40)+c;
  Goto Vege;
  end;
if (d.o_Sum=1) and (Len=0) then begin;
  i:=d.o_Base;
  b:=b2h(i);
  if (i=4) then b:=b2h($04)+SepCh+b2h($24);
  if (i=5) then b:=b2h($45)+SepCh+b2h($00);
  Goto Vege;
  end;
if (d.o_Sum=1) and (Len=0) then begin;
  i:=d.o_Base;
  b:=b2h(i);
  if (i=4) then b:=b2h($04)+SepCh+b2h($24);
  if (i=5) then b:=b2h($45)+SepCh+b2h($00);
  Goto Vege;
  end;
if (d.o_Sum=1) and (Len=1) then begin;
  i:=d.o_Base;
  b:=b2h($40+i);
  if (i=4) then b:=b2h($44)+SepCh+b2h($24);
  b:=b+c;
  Goto Vege;
  end;
if (d.o_Sum=1) and (Len=4) then begin;
  i:=d.o_Base;
  b:=b2h($80+i);
  if (i=4) then b:=b2h($84)+SepCh+b2h($24);
  b:=b+c;
  Goto Vege;
  end;
if (Cod>0) and (Len=0) then begin;
  b:=b2h($04);
  i:=(Cod-1)*$40+d.o_Base+d.o_Idx*$8;
  if (d.o_Base=5) then b:=b2h($44);
  b:=b+SepCh+b2h(i);
  if (d.o_Base=5) then b:=b+SepCh+b2h($00);
  Goto Vege;
  end;
if (Cod>0) and (Len=1) then begin;
  i:=(Cod-1)*$40+d.o_Base+d.o_Idx*$8;
  b:=b2h($44)+SepCh+b2h(i)+c;
  Goto Vege;
  end;
if (Cod>0) and (Len=4) then begin;
  i:=(Cod-1)*$40+d.o_Base+d.o_Idx*$8;
  b:=b2h($84)+SepCh+b2h(i)+c;
  Goto Vege;
  end;

b:='';
Goto Vege;

Vege:
while (copy(b,1,1)=SepCh) do delete(b,1,1);
if (b<>'') then begin;
  i:=h2b(Copy(b,1,2));
  b:=b2h(i+Plus)+Copy(b,3,255);
  i:=i;
  end;
ConvertAllMemoryDatas:=b;
End;


Function ConvertPointerSegRegs(dd:OneMemPointRec;Plus:Byte):String;
Label Vege;
Var
  c:String;
  i:LongInt;
  d:OneRegType;
Begin;
d:=dd.SegR;
c:='';
if (dd.Typ=0) then Goto Vege;
if (d.Typ=0) then begin;
  c:='nop';
  end else begin;
  if (d.Typ<>3) then Goto Vege;
  i:=$26+(d.Num-1)*8;
  if (d.Pro=1) then i:=$64+d.Num-1;
  c:=b2h(i+Plus);
  end;
i:=dd.Mode;
if (CurrentCodeStyle=1) then i:=3-i;
if (i=2) then c:=b2h($67)+SepCh+c;
Vege:
ConvertPointerSegRegs:=c;
End;


Procedure AnalizeParameter(s:String;mode:Byte;Var d:OneParameterRec);
Var
  r:OneRegType;
  m:OneMemPointRec;
  a:String;
  i:LongInt;
Begin;
d.Typ:=0;
d.Str:=s;
AnalizeAnyRegs(s,r);
if (r.Typ<>0) then begin; d.Typ:=1;d.Reg:=r;Exit; end;
if (mode and 1<>0) then begin;
  AnalizeFPUregisters(s,r);
  if (r.Typ<>0) then begin; d.Typ:=5;d.Reg:=r;Exit; end;
  end;
if (mode and 2<>0) then begin;
  AnalizeMMXregisters(s,r);
  if (r.Typ<>0) then begin; d.Typ:=6;d.Reg:=r;Exit; end;
  end;
a:=AnalizeNumbers(s);
if (a<>'') then     begin; d.Typ:=2;d.Num:=a;Exit; end;
AnalizeMemPoint(s,m);
if (m.Typ<>0) then begin; d.Typ:=3;d.Mem:=m;Exit; end;
a:=AnalizeLabelName(s);
if (a<>'') then     begin; d.Typ:=4;d.Lab:=a;Exit; end;
End;
