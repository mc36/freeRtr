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
  OneParameterRec=record
    Typ:Byte;
    Str:String[128];
    Reg:LongInt;             {1}
    Num:String[128];         {2}
    Lab:String[128];         {3}
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

Function EzzelKezd(a,b:String):Boolean;
Label f1;
Var i,o:LongInt;
Begin;
EzzelKezd:=false;
f1:
i:=pos('|',a);
if (i<1) then exit;
dec(i);
if (i<1) then begin;
  a:=copy(a,2,666);
  goto f1;
  end;
if (copy(a,1,i)<>copy(b,1,i)) then begin;
  a:=copy(a,i+1,666);
  goto f1;
  end;
EzzelKezd:=true;
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


Function AnalizeCond(a:String):LongInt;
Var i,o:LongInt;
Begin;
a:=kicsi(a);
o:=0;
if (copy(a,length(a)-1,666)=',a') then begin;
  a:=copy(a,1,length(a)-2);
  o:=$10;
  end;
i:=-1;
if (a='n') then i:=0;
if (a='e') then i:=1;
if (a='le') then i:=2;
if (a='l') then i:=3;
if (a='leu') then i:=4;
if (a='cs') then i:=5;
if (a='neg') then i:=6;
if (a='vs') then i:=7;
if (a='a') then i:=8;
if (a='ne') then i:=9;
if (a='g') then i:=10;
if (a='ge') then i:=11;
if (a='gu') then i:=12;
if (a='cc') then i:=13;
if (a='pos') then i:=14;
if (a='vc') then i:=15;
if (i>=0) then inc(i,o);
AnalizeCond:=i;
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


Function ConvertRegOrSimm13(p:OneParameterRec;op:LongInt):String;
Var
  a:String;
  i:LongInt;
Begin;
a:='';
op:=op and $ffffc000;
if (p.Typ=1) then begin;
  {reg}
  i:=p.Reg or op;
  a:=Hex2ProciDat(AnalizeNumbers(BStr(i)),4);
  end;
op:=op or $2000;
if (p.Typ=2) then begin;
  {Num}
  i:=(BVal('$'+p.Num) and $1fff) or op;
  a:=Hex2ProciDat(AnalizeNumbers(BStr(i)),4);
  end;
if (p.Typ=3) then begin;
  {Label}
  a:=SpxCh+'or'+SpxCh+AnalizeNumbers(BStr(op))+SpxCh+p.Lab;
  a:='dword'+SpxCh+'msb'+SpxCh+'and'+SpxCh+'1FFF'+a;
  end;
ConvertRegOrSimm13:=a;
End;

Procedure AnalizeCpuRegs(a:String;var i:LongInt);
Label f1;
Var c:char;
Begin;
if (a='sp') then begin; i:=14;exit; end;
if (a='lr') then begin; i:=15;exit; end;
if (a='fp') then begin; i:=30;exit; end;
c:=a[1];
a:=copy(a,2,255);
i:=BVal(a);
if (bstr(i)<>a) then goto f1;
if (c='r') then begin;
  if (i<0) or (i>31) then goto f1;
  exit;
  end;
if (c='g') then begin;
  if (i<0) or (i>7) then goto f1;
  exit;
  end;
if (c='o') then begin;
  if (i<0) or (i>7) then goto f1;
  inc(i,8);
  exit;
  end;
if (c='l') then begin;
  if (i<0) or (i>7) then goto f1;
  inc(i,16);
  exit;
  end;
if (c='i') then begin;
  if (i<0) or (i>7) then goto f1;
  inc(i,24);
  exit;
  end;
f1:
i:=-1;
End;







Procedure AnalizeParameter(s:String;Var d:OneParameterRec);
Var
  a:String;
  i:LongInt;
Begin;
d.Typ:=0;
d.Str:=s;
if (s='') then exit;
AnalizeCpuRegs(s,i);
if (i>=0) then begin; d.Typ:=1;d.Reg:=i;Exit; end;
a:=AnalizeNumbers(s);
if (a<>'') then begin; d.Typ:=2;d.Num:=a;Exit; end;
a:=AnalizeLabelName(s);
if (a<>'') then begin; d.Typ:=3;d.Lab:=a;Exit; end;
End;
