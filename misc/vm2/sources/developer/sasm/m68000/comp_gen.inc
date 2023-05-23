Const
  InternalNumber=#13;
  SepCh='\';
  SpxCh=':';
  ZeroFill='00000000000000000000000000000000000000000';
Const
  DataSizeNames:Array[1..4] of String[6]=('byte','word','','dword');

Var
  InStr:String;
  OutStr:String;
  OutOk:Boolean;
  CoprocessorId:LongInt;


Type
  OneRegType=record
    Num:Byte;{1=d0 2=d1 3=d2 4=d3 5=d4 6=d5 7=d6 8=d7 9=a0
              10=a1 11=a2 12=a3 13=a4 14=a5 15=a6 16=a7 17=pc}
    Len:Byte;{1,2,4}
    Scl:Byte;{0,1,2,4,8}
    end;
Type
  OneMemPointRec=record
    Num1:LongInt;
    Reg1:OneRegType;
    Reg2:OneRegType;
    Lab1:String;
    preDec:Boolean;     {predecrementing}
    pstInc:Boolean;     {postincrementing}
    Num2:LongInt;
    Reg3:OneRegType;
    Reg4:OneRegType;
    Lab2:String;
    end;
Type
  OneParameterRec=record
    Typ:Byte;
    Str:String[128];
    Reg:OneRegType;          {1}
    Num:String[128];         {2}
    Mem:OneMemPointRec;      {3}
    Lab:String[128];         {4}
    Frg:OneRegType;          {5}
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
if (s[1]='#') then s:=copy(s,2,255);
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
if (copy(s,1,1)='#') then begin;
  s:=copy(s,2,255);
  end;
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


Function ConvertAllMemoryDatas(p:OneParameterRec;s:LongInt;var b:String):LongInt;
Const
  WordLongTable:array[0..4] of Word=($ffff,$ffff,0,$ffff,$800);
  ScalingTable:array[0..8] of Word=($ffff,0,$200,$ffff,$400,$ffff,$ffff,$ffff,$600);
  BaseDispTable:array[0..4] of Word=($10,$ffff,$20,$ffff,$30);
Var
  m:OneMemPointRec;
  i,o,q:LongInt;
  a:String;

Procedure xchg(var r1,r2:OneRegType);
Var r:OneRegType;
Begin;
r:=r1;
r1:=r2;
r2:=r;
End;


Begin;
ConvertAllMemoryDatas:=-1;
if (s<2) then s:=2;
b:='nop';
case p.Typ of
  1:begin; {Reg}
    {register}
    s:=p.Reg.Num-1;
    if (s in [0..7]) then begin;
      ConvertAllMemoryDatas:=s and 7;
      exit;
      end;
    if (s in [8..15]) then begin;
      ConvertAllMemoryDatas:=(s and 7) or 8;
      exit;
      end;
    Exit;
    end;
  2:begin; {num}
    {#number}
    b:=Hex2ProciDat(p.Num,s);
    ConvertAllMemoryDatas:=$3c;
    Exit;
    end;
  3:;{mem}
  4:begin; {lab}
    {#number}
    b:='msb'+SpxCh+DataSizeNames[s]+SpxCh+p.Lab;
    if (s<2) then b:='signed'+SpxCh+b;
    ConvertAllMemoryDatas:=$3c;
    Exit;
    end;
  else exit;
  end;
m:=p.Mem;
q:=0;
if (m.Num1<>0) then inc(q,$01);
if (m.Reg1.Num<>0) then inc(q,$02);
if (m.Reg2.Num<>0) then inc(q,$04);
if (m.Lab1<>'') then inc(q,$08);
if (m.Num2<>0) then inc(q,$10);
if (m.Reg3.Num<>0) then inc(q,$20);
if (m.Reg4.Num<>0) then inc(q,$40);
if (m.Lab2<>'') then inc(q,$80);
if (q=$20) and (m.Reg3.Scl=1) and (m.Reg3.Len=4) and (m.Reg3.Num in [9..16]) then begin;
  if (m.preDec=False) and (m.pstInc=False) then begin;
    {addr}
    ConvertAllMemoryDatas:=(m.Reg3.Num-9) or $10;
    exit;
    end;
  if (m.preDec=True) and (m.pstInc=False) then begin;
    {-addr}
    ConvertAllMemoryDatas:=(m.Reg3.Num-9) or $20;
    exit;
    end;
  if (m.preDec=False) and (m.pstInc=True) then begin;
    {addr+}
    ConvertAllMemoryDatas:=(m.Reg3.Num-9) or $18;
    exit;
    end;
  end;
if (m.preDec or m.pstInc) then exit;
if (q=$10) then begin;
  if (m.Num2>=-32768) and (m.Num2<=32767) then begin;
    {num16}
    b:=Hex2ProciDat(w2h(m.Num2),2);
    ConvertAllMemoryDatas:=$38;
    exit;
    end;
  {num32}
  b:=Hex2ProciDat(w2h(m.Num2 shr 16)+w2h(m.Num2),4);
  ConvertAllMemoryDatas:=$39;
  exit;
  end;
if (q=$80) then begin;
  {num32}
  b:='msb'+SpxCh+'dword'+SpxCh+m.Lab2;
  ConvertAllMemoryDatas:=$39;
  exit;
  end;
if (q and $ef=$20) and (m.Reg3.Scl=1) and (m.Reg3.Len=4) and
 (m.Reg3.Num in [9..16]) and (m.Num2>=-32768) and (m.Num2<32768) then begin;
  {num16,addr}
  b:=Hex2ProciDat(w2h(m.Num2),2);
  ConvertAllMemoryDatas:=(m.Reg3.Num-9) or $28;
  exit;
  end;
if (q and $6f=$20) and (m.Reg3.Scl=1) and (m.Reg3.Len=4) and (m.Reg3.Num=17) then begin;
  {num16,pc}
  if (m.Lab2='') then b:='numofs'+SpxCh+w2h(m.Num2) else b:='ofs?'+SpxCh+m.Lab2;
  b:='msb'+SpxCh+'word'+SpxCh+'rela2beg'+SpxCh+'add'+SpxCh+'FFFFFFFE'+SpxCh+b;
  ConvertAllMemoryDatas:=$3a;
  exit;
  end;
if (q and $ef=$60) and (m.Reg3.Scl=1) and (m.Reg3.Len=4) and
 (m.Reg3.Num in [9..16]) and (m.Num2>=-128) and (m.Num2<128) and
 (m.Reg4.Scl>=1) and (m.Reg4.Len>=2) and (m.Reg4.Num in [1..16]) then begin;
  {num8,addr,any.size*scale}
  i:=(m.Reg4.Num-1) shl 12;
  i:=i or WordLongTable[m.Reg4.Len];
  i:=i or ScalingTable[m.Reg4.Scl];
  b:=b2h(i shr 8)+SepCh+b2h(m.Num2);
  ConvertAllMemoryDatas:=(m.Reg3.Num-9) or $30;
  exit;
  end;
{$ifdef m68010only}
if (q and $ff=$e0) and (m.Reg3.Scl=1) and (m.Reg3.Len=4) and
 (m.Reg3.Num in [9..16]) and (m.Num2>=-128) and (m.Num2<128) and
 (m.Reg4.Scl>=1) and (m.Reg4.Len>=2) then begin;
  {label,addr,any.size*scale}
  i:=(m.Reg4.Num-1) shl 12;
  i:=i or WordLongTable[m.Reg4.Len];
  i:=i or ScalingTable[m.Reg4.Scl];
  b:=b2h(i shr 8)+SepCh+'ofs?'+SpxCh+'byte'+SpxCh+'signed'+SpxCh+m.Lab2;
  ConvertAllMemoryDatas:=(m.Reg3.Num-9) or $30;
  exit;
  end;
if (q and $6f=$60) and (m.Reg3.Scl=1) and (m.Reg3.Len=4) and
 (m.Reg3.Num=17) and (m.Num2>=-128) and (m.Num2<128) and
 (m.Reg4.Scl>=1) and (m.Reg4.Len>=2) and (m.Reg4.Num in [1..16]) then begin;
  {num8,pc,any.size*scale}
  if (m.Lab2='') then b:='numofs'+SpxCh+b2h(m.Num2) else b:='ofs?'+SpxCh+m.Lab2;
  i:=(m.Reg4.Num-1) shl 12;
  i:=i or WordLongTable[m.Reg4.Len];
  i:=i or ScalingTable[m.Reg4.Scl];
  b:=b2h(i shr 8)+SepCh+'byte'+SpxCh+'rela2beg'+SpxCh+'add'+SpxCh+'FFFFFFFE'+SpxCh+b;
  ConvertAllMemoryDatas:=$3b;
  exit;
  end;
{$endif}
if (m.Reg2.Num>8) and (m.Reg2.Scl=1) and (m.Reg2.Len=4) then xchg(m.Reg1,m.Reg2);
if (m.Reg4.Num>8) and (m.Reg4.Scl=1) and (m.Reg4.Len=4) then xchg(m.Reg3,m.Reg4);
if (m.Reg1.Scl>1) or (m.Reg1.Len<4) then xchg(m.Reg1,m.Reg2);
if (m.Reg3.Scl>1) or (m.Reg3.Len<4) then xchg(m.Reg3,m.Reg4);
if (q and $f=$0) and (m.Reg3.Num<17) and (m.Reg4.Num<17) then begin;
  {addr+any.size*scale+number/label}
  s:=0;
  b:='';
  if (m.Lab2<>'') then begin;
    b:=SepCh+'dword'+SpxCh+'msb'+SpxCh+'ofs?'+SpxCh+m.Lab2;
    s:=4;
    end;
  if (m.Num2<>0) then begin;
    b:=SepCh+Hex2ProciDat(w2h(m.Num2 shr 16)+w2h(m.Num2),4);
    s:=4;
    if (m.Num2>=-32768) and (m.Num2<32768) then begin;
      b:=SepCh+Hex2ProciDat(w2h(m.Num2),2);
      s:=2;
      end;
    end;
  if (m.Reg3.Scl=0) then begin;
    m.Reg3.Num:=16;
    i:=$180;
    end else begin;
    if (m.Reg3.Scl<>1) then exit;
    if (m.Reg3.Len<>4) then exit;
    if not (m.Reg3.Num in [8..16]) then exit;
    i:=$100;
    end;
  if (m.Reg4.Scl=0) then begin;
    i:=i or $40;
    end else begin;
    i:=i or ((m.Reg4.Num-1) shl 12);
    i:=i or WordLongTable[m.Reg4.Len];
    i:=i or ScalingTable[m.Reg4.Scl];
    end;
  i:=i or BaseDispTable[s];
  b:=b2h(i shr 8)+SepCh+b2h(i)+b;
  ConvertAllMemoryDatas:=(m.Reg3.Num-9) or $30;
  exit;
  end;
if (q and $f=$0) and (m.Reg3.Num=17) and (m.Reg4.Num<17) then begin;
  {pc+any.size*scale+number/label}
  s:=0;
  b:='';
  if (m.Lab2<>'') then begin;
    b:='ofs?'+SpxCh+m.Lab2;
    s:=4;
    end;
  if (m.Num2<>0) then begin;
    b:=w2h(m.Num2 shr 16)+w2h(m.Num2);
    s:=4;
    if (m.Num2>=-32768) and (m.Num2<32768) then begin;
      b:=w2h(m.Num2);
      s:=2;
      end;
    b:='numofs'+SpxCh+b;
    end;
  if (s<>0) then b:=SepCh+'msb'+SpxCh+'rela2beg'+SpxCh+'add'+SpxCh+'FFFFFFFE'+SpxCh+DataSizeNames[s]+SpxCh+b;
  if (m.Reg3.Scl=0) then begin;
    i:=$180;
    end else begin;
    if (m.Reg3.Scl<>1) then exit;
    if (m.Reg3.Len<>4) then exit;
    i:=$100;
    end;
  if (m.Reg4.Scl=0) then begin;
    i:=i or $40;
    end else begin;
    i:=i or ((m.Reg4.Num-1) shl 12);
    i:=i or WordLongTable[m.Reg4.Len];
    i:=i or ScalingTable[m.Reg4.Scl];
    end;
  i:=i or BaseDispTable[s];
  b:=b2h(i shr 8)+SepCh+b2h(i)+b;
  ConvertAllMemoryDatas:=$3b;
  exit;
  end;


if (q and $f=$0) and (m.Reg3.Num<17) and (m.Reg4.Num<17) then begin;
  {addr+any.size*scale+number/label}
  s:=0;
  b:='';
  if (m.Lab2<>'') then begin;
    b:=SepCh+'dword'+SpxCh+'msb'+SpxCh+'ofs?'+SpxCh+m.Lab2;
    s:=4;
    end;
  if (m.Num2<>0) then begin;
    b:=SepCh+Hex2ProciDat(w2h(m.Num2 shr 16)+w2h(m.Num2),4);
    s:=4;
    if (m.Num2>=-32768) and (m.Num2<32768) then begin;
      b:=SepCh+Hex2ProciDat(w2h(m.Num2),2);
      s:=2;
      end;
    end;
  if (m.Reg3.Scl=0) then begin;
    m.Reg3.Num:=16;
    i:=$180;
    end else begin;
    if (m.Reg3.Scl<>1) then exit;
    if (m.Reg3.Len<>4) then exit;
    if not (m.Reg3.Num in [9..16]) then exit;
    i:=$100;
    end;
  if (m.Reg4.Scl=0) then begin;
    i:=i or $40;
    end else begin;
    i:=i or ((m.Reg4.Num-1) shl 12);
    i:=i or WordLongTable[m.Reg4.Len];
    i:=i or ScalingTable[m.Reg4.Scl];
    end;
  i:=i or BaseDispTable[s];
  b:=b2h(i shr 8)+SepCh+b2h(i)+b;
  ConvertAllMemoryDatas:=(m.Reg3.Num-9) or $30;
  exit;
  end;
if (q and $f=$0) and (m.Reg3.Num=17) and (m.Reg4.Num<17) then begin;
  {pc+any.size*scale+number/label}
  s:=0;
  b:='';
  if (m.Lab2<>'') then begin;
    b:='ofs?'+SpxCh+m.Lab2;
    s:=4;
    end;
  if (m.Num2<>0) then begin;
    b:=w2h(m.Num2 shr 16)+w2h(m.Num2);
    s:=4;
    if (m.Num2>=-32768) and (m.Num2<32768) then begin;
      b:=w2h(m.Num2);
      s:=2;
      end;
    b:='numofs'+SpxCh+b;
    end;
  if (s<>0) then b:=SepCh+'msb'+SpxCh+'rela2beg'+SpxCh+'add'+SpxCh+'FFFFFFFE'+SpxCh+DataSizeNames[s]+SpxCh+b;
  if (m.Reg3.Scl=0) then begin;
    i:=$180;
    end else begin;
    if (m.Reg3.Scl<>1) then exit;
    if (m.Reg3.Len<>4) then exit;
    i:=$100;
    end;
  if (m.Reg4.Scl=0) then begin;
    i:=i or $40;
    end else begin;
    i:=i or ((m.Reg4.Num-1) shl 12);
    i:=i or WordLongTable[m.Reg4.Len];
    i:=i or ScalingTable[m.Reg4.Scl];
    end;
  i:=i or BaseDispTable[s];
  b:=b2h(i shr 8)+SepCh+b2h(i)+b;
  ConvertAllMemoryDatas:=$3b;
  exit;
  end;
if (m.Reg1.Num in [9..16]) and (m.Reg2.Num=0) and (m.Reg3.Num<17) and (m.Reg4.Num<17) then begin;
  {[number/label+addr]+any.size*scale+number/label}
  s:=0;
  b:='';
  if (m.Lab2<>'') then begin;
    b:=SepCh+'dword'+SpxCh+'msb'+SpxCh+'ofs?'+SpxCh+m.Lab1;
    s:=4;
    end;
  if (m.Num1<>0) then begin;
    b:=SepCh+Hex2ProciDat(w2h(m.Num1 shr 16)+w2h(m.Num1),4);
    s:=4;
    if (m.Num1>=-32768) and (m.Num1<32768) then begin;
      b:=SepCh+Hex2ProciDat(w2h(m.Num1),2);
      s:=2;
      end;
    end;
  q:=0;
  a:='';
  if (m.Lab2<>'') then begin;
    a:=SepCh+'dword'+SpxCh+'msb'+SpxCh+'ofs?'+SpxCh+m.Lab2;
    s:=4;
    end;
  if (m.Num2<>0) then begin;
    a:=SepCh+Hex2ProciDat(w2h(m.Num2 shr 16)+w2h(m.Num2),4);
    q:=4;
    if (m.Num2>=-32768) and (m.Num2<32768) then begin;
      a:=SepCh+Hex2ProciDat(w2h(m.Num2),2);
      q:=2;
      end;
    end;
  if (m.Reg1.Scl=0) then begin;
    m.Reg1.Num:=16;
    i:=$180;
    end else begin;
    if (m.Reg1.Scl<>1) then exit;
    if (m.Reg1.Len<>4) then exit;
    if not (m.Reg1.Num in [9..16]) then exit;
    i:=$100;
    end;
  if (m.Reg3.Num=0) then m.Reg3:=m.Reg4;
  if (m.Reg3.Scl=0) then begin;
    m.Reg3.Num:=16;
    i:=i or $40;
    end else begin;
    i:=i or ((m.Reg3.Num-1) shl 12);
    i:=i or WordLongTable[m.Reg3.Len];
    i:=i or ScalingTable[m.Reg3.Scl];
    end;
  i:=i or BaseDispTable[s];
  i:=i or (BaseDispTable[q] shr 4);
  if (i and $40=0) then i:=i or 4;
  b:=b2h(i shr 8)+SepCh+b2h(i)+b+a;
  ConvertAllMemoryDatas:=(m.Reg1.Num-9) or $30;
  exit;
  end;
if (m.Reg1.Num=17) and (m.Reg2.Num=0) and (m.Reg3.Num<17) and (m.Reg4.Num<17) then begin;
  {[number/label+pc]+any.size*scale+number/label}
  s:=0;
  b:='';
  if (m.Lab1<>'') then begin;
    b:='ofs?'+SpxCh+m.Lab1;
    s:=4;
    end;
  if (m.Num1<>0) then begin;
    b:=w2h(m.Num1 shr 16)+w2h(m.Num1);
    s:=4;
    if (m.Num1>=-32768) and (m.Num1<32768) then begin;
      b:=w2h(m.Num1);
      s:=2;
      end;
    b:='numofs'+SpxCh+b;
    end;
  if (s<>0) then b:=SepCh+'msb'+SpxCh+'rela2beg'+SpxCh+'add'+SpxCh+'FFFFFFFE'+SpxCh+DataSizeNames[s]+SpxCh+b;
  q:=0;
  a:='';
  if (m.Lab2<>'') then begin;
    a:=SepCh+'dword'+SpxCh+'msb'+SpxCh+'ofs?'+SpxCh+m.Lab2;
    s:=4;
    end;
  if (m.Num2<>0) then begin;
    a:=SepCh+Hex2ProciDat(w2h(m.Num2 shr 16)+w2h(m.Num2),4);
    q:=4;
    if (m.Num2>=-32768) and (m.Num2<32768) then begin;
      a:=SepCh+Hex2ProciDat(w2h(m.Num2),2);
      q:=2;
      end;
    end;
  if (m.Reg1.Scl=0) then begin;
    i:=$180;
    end else begin;
    if (m.Reg1.Scl<>1) then exit;
    if (m.Reg1.Len<>4) then exit;
    i:=$100;
    end;
  if (m.Reg3.Num=0) then m.Reg3:=m.Reg4;
  if (m.Reg3.Scl=0) then begin;
    m.Reg3.Num:=16;
    i:=i or $40;
    end else begin;
    i:=i or ((m.Reg3.Num-1) shl 12);
    i:=i or WordLongTable[m.Reg3.Len];
    i:=i or ScalingTable[m.Reg3.Scl];
    end;
  i:=i or BaseDispTable[s];
  i:=i or (BaseDispTable[q] shr 4);
  if (i and $40=0) then i:=i or 4;
  b:=b2h(i shr 8)+SepCh+b2h(i)+b+a;
  ConvertAllMemoryDatas:=$3b;
  exit;
  end;
if (m.Reg1.Num<17) and (m.Reg2.Num<17) and (m.Reg3.Num=0) and (m.Reg4.Num=0) then begin;
  {[number/label+addr+any.size*scale]+number/label}
  s:=0;
  b:='';
  if (m.Lab2<>'') then begin;
    b:=SepCh+'dword'+SpxCh+'msb'+SpxCh+'ofs?'+SpxCh+m.Lab1;
    s:=4;
    end;
  if (m.Num1<>0) then begin;
    b:=SepCh+Hex2ProciDat(w2h(m.Num1 shr 16)+w2h(m.Num1),4);
    s:=4;
    if (m.Num1>=-32768) and (m.Num1<32768) then begin;
      b:=SepCh+Hex2ProciDat(w2h(m.Num1),2);
      s:=2;
      end;
    end;
  q:=0;
  a:='';
  if (m.Lab2<>'') then begin;
    a:=SepCh+'dword'+SpxCh+'msb'+SpxCh+'ofs?'+SpxCh+m.Lab2;
    s:=4;
    end;
  if (m.Num2<>0) then begin;
    a:=SepCh+Hex2ProciDat(w2h(m.Num2 shr 16)+w2h(m.Num2),4);
    q:=4;
    if (m.Num2>=-32768) and (m.Num2<32768) then begin;
      a:=SepCh+Hex2ProciDat(w2h(m.Num2),2);
      q:=2;
      end;
    end;
  if (m.Reg1.Scl=0) then begin;
    m.Reg1.Num:=16;
    i:=$180;
    end else begin;
    if (m.Reg1.Scl<>1) then exit;
    if (m.Reg1.Len<>4) then exit;
    if not (m.Reg1.Num in [9..16]) then exit;
    i:=$100;
    end;
  if (m.Reg2.Scl=0) then begin;
    m.Reg2.Num:=16;
    i:=i or $40;
    end else begin;
    i:=i or ((m.Reg2.Num-1) shl 12);
    i:=i or WordLongTable[m.Reg2.Len];
    i:=i or ScalingTable[m.Reg2.Scl];
    end;
  i:=i or BaseDispTable[s];
  i:=i or (BaseDispTable[q] shr 4);
  b:=b2h(i shr 8)+SepCh+b2h(i)+b+a;
  ConvertAllMemoryDatas:=(m.Reg1.Num-9) or $30;
  exit;
  end;
if (m.Reg1.Num=17) and (m.Reg2.Num<17) and (m.Reg3.Num=0) and (m.Reg4.Num=0) then begin;
  {[number/label+pc+any.size*scale]+number/label}
  s:=0;
  b:='';
  if (m.Lab1<>'') then begin;
    b:='ofs?'+SpxCh+m.Lab1;
    s:=4;
    end;
  if (m.Num1<>0) then begin;
    b:=w2h(m.Num1 shr 16)+w2h(m.Num1);
    s:=4;
    if (m.Num1>=-32768) and (m.Num1<32768) then begin;
      b:=w2h(m.Num1);
      s:=2;
      end;
    b:='numofs'+SpxCh+b;
    end;
  if (s<>0) then b:=SepCh+'msb'+SpxCh+'rela2beg'+SpxCh+'add'+SpxCh+'FFFFFFFE'+SpxCh+DataSizeNames[s]+SpxCh+b;
  q:=0;
  a:='';
  if (m.Lab2<>'') then begin;
    a:=SepCh+'dword'+SpxCh+'msb'+SpxCh+'ofs?'+SpxCh+m.Lab2;
    s:=4;
    end;
  if (m.Num2<>0) then begin;
    a:=SepCh+Hex2ProciDat(w2h(m.Num2 shr 16)+w2h(m.Num2),4);
    q:=4;
    if (m.Num2>=-32768) and (m.Num2<32768) then begin;
      a:=SepCh+Hex2ProciDat(w2h(m.Num2),2);
      q:=2;
      end;
    end;
  if (m.Reg1.Scl=0) then begin;
    m.Reg1.Num:=16;
    i:=$180;
    end else begin;
    if (m.Reg1.Scl<>1) then exit;
    if (m.Reg1.Len<>4) then exit;
    i:=$100;
    end;
  if (m.Reg2.Scl=0) then begin;
    m.Reg2.Num:=16;
    i:=i or $40;
    end else begin;
    i:=i or ((m.Reg2.Num-1) shl 12);
    i:=i or WordLongTable[m.Reg2.Len];
    i:=i or ScalingTable[m.Reg2.Scl];
    end;
  i:=i or BaseDispTable[s];
  i:=i or (BaseDispTable[q] shr 4);
  b:=b2h(i shr 8)+SepCh+b2h(i)+b+a;
  ConvertAllMemoryDatas:=$3b;
  exit;
  end;

End;


Function ConvertNumOrLabDatas(p:OneParameterRec;len,min:Byte;es:String):String;
Var
  a:String;
Begin;
if (es<>'') then es:=es+SpxCh;
if (len<min) then len:=min;
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



Function AnalizeSizePostfix1(var a:String;d:LongInt):LongInt;
Var
  b:String;
  i:LongInt;
Begin;
AnalizeSizePostfix1:=d;
d:=pos('.',a);
if (d<1) then exit;
b:=copy(a,d+1,255);
i:=0;
if (b='b') then i:=1;
if (b='w') then i:=2;
if (b='l') then i:=4;
if (i<1) then exit;
a:=copy(a,1,d-1);
AnalizeSizePostfix1:=i;
End;

Function AnalizeSizePostfix2(var a:String;d:LongInt):LongInt;
Var
  b:String;
  i:LongInt;
Begin;
AnalizeSizePostfix2:=d;
d:=pos('.',a);
if (d<1) then exit;
b:=copy(a,d+1,255);
i:=-1;
if (b='l') then i:=0;
if (b='s') then i:=1;
if (b='x') then i:=2;
if (b='p') then i:=3;
if (b='w') then i:=4;
if (b='d') then i:=5;
if (b='b') then i:=6;
if (i<0) then exit;
a:=copy(a,1,d-1);
AnalizeSizePostfix2:=i;
End;


Function AnalizeCpuRegs(a:String;var r:OneRegType):Boolean;
Label vege;
Var i,o:LongInt;
Begin;
if (copy(a,1,1)<>'z') then r.Scl:=1 else begin;
  a:=copy(a,2,255);
  r.Scl:=0;
  end;
i:=pos('*',a);
if (i>0) then begin;
  o:=BVal(copy(a,i+1,255));
  if not (o in [1,2,4,8]) then goto vege;
  if (r.Scl<>0) then r.Scl:=o;
  a:=copy(a,1,i-1);
  end;
r.Len:=AnalizeSizePostfix1(a,4);
i:=0;
if (a[1]='d') then begin;
  i:=BVal(a[2]);
  if (i<0) or (i>7) then goto vege;
  if (BStr(i)<>copy(a,2,255)) then goto vege;
  inc(i,1);
  end;
if (a[1]='a') then begin;
  i:=BVal(a[2]);
  if (i<0) or (i>7) then goto vege;
  if (BStr(i)<>copy(a,2,255)) then goto vege;
  inc(i,9);
  end;
if (a='pc') then i:=17;
if (i<>0) then begin;
  r.Num:=i;
  AnalizeCpuRegs:=True;
  exit;
  end;
vege:
r.Num:=0;
r.Len:=0;
AnalizeCpuRegs:=False;
End;

Function AnalizeFpuRegs(a:String;var r:OneRegType):Boolean;
Label vege;
Var i,o:LongInt;
Begin;
r.Len:=4;
i:=0;
if (copy(a,1,2)='fp') then begin;
  i:=BVal(a[3]);
  if (i<0) or (i>7) then goto vege;
  if (BStr(i)<>copy(a,3,255)) then goto vege;
  inc(i,1);
  end;
if (i<>0) then begin;
  r.Num:=i;
  AnalizeFpuRegs:=True;
  exit;
  end;
vege:
r.Num:=0;
r.Len:=0;
AnalizeFpuRegs:=False;
End;


Function AnalizeMemPoint(s:String;var m:OneMemPointRec):Boolean;
Var
  a,b,c:String;
  i,o:LongInt;

Function get(var a:String):String;
Var i:LongInt;
Begin;
i:=pos(',',a);
if (i<1) then i:=666;
get:=copy(a,1,i-1);
a:=copy(a,i+1,255);
End;

Procedure xchg(var r1,r2:OneRegType);
Var r:OneRegType;
Begin;
r:=r1;
r1:=r2;
r2:=r;
End;

Function proc(var r1,r2:OneRegType;var l:String;var n:LongInt):Boolean;
Label f1;
Var r:OneRegType;
Begin;
proc:=True;
f1:
if (s='') then begin;
  proc:=False;
  exit;
  end;
b:=get(s);

a:=AnalizeNumbers(b);
if (a<>'') then begin;
  inc(n,BVal('$'+a));
  goto f1;
  end;
if AnalizeCpuRegs(b,r) then begin;
  if (r1.Num=0) then begin; r1:=r;goto f1; end;
  if (r2.Num=0) then begin; r2:=r;goto f1; end;
  exit;
  end;
a:=AnalizeLabelName(b);
if (a='') then exit;
if (l<>'') then exit;
l:=a;
goto f1;
End;

begin;
AnalizeMemPoint:=False;
fillChar(m,sizeof(m),0);
m.preDec:=(copy(s,1,1)='-');
m.pstInc:=(copy(s,length(s),1)='+');
if m.preDec then s:=copy(s,2,255);
if m.pstInc then s:=copy(s,1,length(s)-1);
if (copy(s,1,1)<>'(') then exit;
if (copy(s,length(s),1)<>')') then exit;
s:=Copy(s,2,Length(s)-2);

Kicserel(', ',',',s);
Kicserel(' ,',',',s);

if (copy(s,1,1)='[') then begin;
  i:=pos(']',s);
  if (i<1) then exit;
  c:=copy(s,i+1,255);
  s:=copy(s,2,i-2);
  if proc(m.Reg1,m.Reg2,m.Lab1,m.Num1) then exit;
  s:=c;
  if (s[1]=',') then s:=copy(s,2,255);
  end;

if proc(m.Reg3,m.Reg4,m.Lab2,m.Num2) then exit;

if (m.Num1<>0) and (m.Lab1<>'') then exit;
if (m.Num2<>0) and (m.Lab2<>'') then exit;
if (m.Reg1.Num<>0) and (m.Reg2.Num<>0) and (m.Reg3.Num<>0) then exit;
if (m.Reg3.Num<>0) and (m.Reg4.Num<>0) and (m.Reg1.Num<>0) then exit;
AnalizeMemPoint:=True;
End;


Function AnalizeCpuRegList(s:String):LongInt;
Label f1;
Var
  r:OneRegType;
  i,o:LongInt;
  a:String;
Begin;
AnalizeCpuRegList:=-1;
if (copy(s,1,1)<>'<') then exit;
if (copy(s,length(s),1)<>'>') then exit;
s:=copy(s,2,length(s)-2);
o:=0;
f1:
if (s='') then begin;
  AnalizeCpuRegList:=o;
  exit;
  end;
i:=pos(',',s);
if (i=0) then i:=$666;
a:=copy(s,1,i-1);
s:=copy(s,i+1,255);
if not AnalizeCpuRegs(a,r) then exit;
i:=r.Num-1;
if (i<0) or (i>15) then exit;
o:=o or (1 shl i);
goto f1;
End;

Function AnalizeFpuRegList(s:String):LongInt;
Label f1;
Var
  r:OneRegType;
  i,o:LongInt;
  a:String;
Begin;
AnalizeFpuRegList:=-1;
if (copy(s,1,1)<>'<') then exit;
if (copy(s,length(s),1)<>'>') then exit;
s:=copy(s,2,length(s)-2);
o:=0;
f1:
if (s='') then begin;
  AnalizeFpuRegList:=o;
  exit;
  end;
i:=pos(',',s);
if (i=0) then i:=$666;
a:=copy(s,1,i-1);
s:=copy(s,i+1,255);
if not AnalizeFpuRegs(a,r) then exit;
i:=r.Num-1;
if (i<0) or (i>7) then exit;
o:=o or (1 shl i);
goto f1;
End;

Function AnalizeFpuCond(s:String):LongInt;
Var i:LongInt;
Begin;
i:=-1;
if (s='eq') then i:=1;
if (s='ne') then i:=14;
if (s='gt') then i:=18;
if (s='ngt') then i:=29;
if (s='ge') then i:=19;
if (s='nge') then i:=28;
if (s='lt') then i:=20;
if (s='nlt') then i:=27;
if (s='le') then i:=21;
if (s='nle') then i:=26;
if (s='gl') then i:=22;
if (s='ngl') then i:=25;
if (s='gle') then i:=23;
if (s='ngle') then i:=24;
if (s='ogt') then i:=2;
if (s='ule') then i:=13;
if (s='oge') then i:=3;
if (s='ult') then i:=12;
if (s='olt') then i:=4;
if (s='uge') then i:=11;
if (s='ole') then i:=5;
if (s='ugt') then i:=10;
if (s='ogl') then i:=6;
if (s='ueq') then i:=9;
if (s='or') then i:=7;
if (s='un') then i:=8;
if (s='f') then i:=0;
if (s='t') then i:=15;
if (s='sf') then i:=16;
if (s='st') then i:=31;
if (s='seq') then i:=17;
if (s='sne') then i:=30;
AnalizeFpuCond:=i;
End;

Function AnalizeCpuCond(s:String):LongInt;
Var i:LongInt;
Begin;
i:=-1;
if (s='t') then i:=0;
if (s='f') then i:=1;
if (s='hi') then i:=2;
if (s='ls') then i:=3;
if (s='cc') then i:=4;
if (s='cs') then i:=5;
if (s='ne') then i:=6;
if (s='eq') then i:=7;
if (s='vc') then i:=8;
if (s='vs') then i:=9;
if (s='pl') then i:=10;
if (s='mi') then i:=11;
if (s='ge') then i:=12;
if (s='lt') then i:=13;
if (s='gt') then i:=14;
if (s='le') then i:=15;
AnalizeCpuCond:=i;
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
if AnalizeCpuRegs(s,r) then begin; d.Typ:=1;d.Reg:=r;Exit; end;
if AnalizeFpuRegs(s,r) then begin; d.Typ:=5;d.Frg:=r;Exit; end;
a:=AnalizeNumbers(s);
if (a<>'') then begin; d.Typ:=2;d.Num:=a;Exit; end;
if AnalizeMemPoint(s,m) then begin; d.Typ:=3;d.Mem:=m;Exit; end;
a:=AnalizeLabelName(s);
if (a<>'') then begin; d.Typ:=4;d.Lab:=a;Exit; end;
End;
