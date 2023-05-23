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


Type
  OneMemPointRec=record
    Typ:Byte;
    Neg:Boolean;
    Upd:Boolean;
    Bas:LongInt;             {5}
    Reg:LongInt;             {1}
    Num:LongInt;             {2}
    Lab:String[128];         {3}
    SfN:LongInt;             {4}
    SfL:String[128];         {6}
    end;
  OneParameterRec=record
    Typ:Byte;
    Str:String[128];
    Reg:LongInt;             {1}
    Num:String[128];         {2}
    Lab:String[128];         {3}
    Rg2:LongInt;             {4}
    Mem:OneMemPointRec;      {5}
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
  b:=b+SepCh+a;
  end;
delete(b,1,1);
Hex2ProciDat:=b;
End;


Function AnalizeCond(a:String):LongInt;
Var i:LongInt;
Begin;
a:=kicsi(a);
i:=-1;
if (a='eq') then i:=0;
if (a='ne') then i:=1;
if (a='cs') then i:=2;
if (a='hs') then i:=2;
if (a='cc') then i:=3;
if (a='lo') then i:=3;
if (a='mi') then i:=4;
if (a='pl') then i:=5;
if (a='vs') then i:=6;
if (a='vc') then i:=7;
if (a='hi') then i:=8;
if (a='ls') then i:=9;
if (a='ge') then i:=10;
if (a='lt') then i:=11;
if (a='gt') then i:=12;
if (a='le') then i:=13;
if (a='al') then i:=14;
AnalizeCond:=i;
End;


Function AnalizeCondS(var c:String;m:LongInt):LongInt;
Label f1;
Var
  i,o:LongInt;
  d:String;
Begin;
o:=0;
d:=copy(c,1,m);
c:=copy(c,m+1,666);
i:=AnalizeCond(copy(c,length(c)-1,666));
if (i>=0) then begin;
  c:=copy(c,1,length(c)-2);
  goto f1;
  end;
if (copy(c,length(c),666)='s') then begin;
  c:=copy(c,1,length(c)-1);
  inc(o,$10);
  end;
i:=AnalizeCond(copy(c,length(c)-1,666));
if (i>=0) then begin;
  c:=copy(c,1,length(c)-2);
  goto f1;
  end;
i:=AnalizeCond('al');
f1:
c:=d+c;
AnalizeCondS:=(i shl 12)+o;
End;


Function AnalizeMultpl(a:String;store:Boolean):LongInt;
Var i:LongInt;
Begin;
a:=kicsi(a);
i:=-1;
if store then begin;
  if (a='ed') then a:='da';
  if (a='ea') then a:='ia';
  if (a='fd') then a:='db';
  if (a='fa') then a:='ib';
  end else begin;
  if (a='fa') then a:='da';
  if (a='fd') then a:='ia';
  if (a='ea') then a:='db';
  if (a='ed') then a:='ib';
  end;
if (a='ia') then i:=$08800000;
if (a='ib') then i:=$09800000;
if (a='da') then i:=$08000000;
if (a='db') then i:=$09000000;
AnalizeMultpl:=i;
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



Procedure AnalizeCpuRegs(a:String;var i:LongInt);
Label f1;
Var c:char;
Begin;
if (a='sp') then begin; i:=13;exit; end;
if (a='lr') then begin; i:=14;exit; end;
if (a='pc') then begin; i:=15;exit; end;
c:=a[1];
a:=copy(a,2,255);
i:=BVal(a);
if (bstr(i)<>a) then goto f1;
if (c='r') then begin;
  if (i<0) or (i>15) then goto f1;
  exit;
  end;
if (c='a') then begin;
  if (i<1) or (i>4) then goto f1;
  dec(i);
  exit;
  end;
if (c='v') then begin;
  if (i<1) or (i>8) then goto f1;
  inc(i,3);
  exit;
  end;
f1:
i:=-1;
End;



Procedure AnalizeFpuRegs(a:String;var i:LongInt);
Label f1;
Var c:char;
Begin;
c:=a[1];
a:=copy(a,2,255);
i:=BVal(a);
if (bstr(i)<>a) then goto f1;
if (c='c') then begin;
  if (i<0) or (i>15) then goto f1;
  exit;
  end;
f1:
i:=-1;
End;



Procedure AnalizeFpuNums(a:String;var i:LongInt);
Label f1;
Var c:char;
Begin;
c:=a[1];
a:=copy(a,2,255);
i:=BVal(a);
if (bstr(i)<>a) then goto f1;
if (c='p') then begin;
  if (i<0) or (i>15) then goto f1;
  exit;
  end;
f1:
i:=-1;
End;


Function AnalizeRegsList(a:String):LongInt;
Label f1;
Var i,o,p:LongInt;
Begin;
AnalizeRegsList:=-1;
if (copy(a,1,1)<>'{') then exit;
if (copy(a,length(a),666)<>'}') then exit;
a:=copy(a,2,length(a)-2);
a:=','+a+',';
kicserel(',,',',',a);
a:=copy(a,2,666);
p:=0;
f1:
i:=pos(',',a);
if (i<1) then begin;
  AnalizeRegsList:=p;
  exit;
  end;
AnalizeCpuRegs(copy(a,1,i-1),o);
if (o<0) then exit;
a:=copy(a,i+1,666);
inc(p,1 shl o);
goto f1;
End;


Function AnalizeRotates(a:String):LongInt;
Var i:LongInt;
Begin;
i:=-1;
a:=kicsi(copy(a,1,3));
if (a='lsl') then i:=0;
if (a='lsr') then i:=1;
if (a='asr') then i:=2;
if (a='ror') then i:=3;
AnalizeRotates:=i;
End;


Procedure AnalizeMemPoint(s:String;Var d:OneMemPointRec);

function nxt:longint;
var i,o:longint;
begin;
i:=pos('+',s);
o:=pos('-',s);
if (i<1) then i:=666;
if (o<1) then o:=666;
if (o<i) then i:=o;
nxt:=i;
end;

Var
  a:String;
  i,o:LongInt;
Begin;
fillchar(d,sizeof(d),0);
if (copy(s,1,1)<>'[') then exit;
if (copy(s,length(s),666)='!') then begin;
  s:=copy(s,1,length(s)-1);
  d.Upd:=true;
  end;
if (copy(s,length(s),666)<>']') then exit;
s:=copy(s,2,length(s)-2);
kicserel(',','+',s);
kicserel(' ','+',s);
s:='+'+s;
repeat
  a:=s;
  Kicserel('-+','-',s);
  Kicserel('+-','-',s);
  Kicserel('--','+',s);
  Kicserel('++','+',s);
  until (s=a);
if (copy(s,1,1)<>'+') then exit;
s:=copy(s,2,666);
i:=nxt;
a:=copy(s,1,i-1);
s:=copy(s,i,666);
AnalizeCpuRegs(a,i);
if (i<0) then exit;
d.Bas:=i;
if (s='') then begin;
  d.Typ:=5;
  exit;
  end;
a:=copy(s,1,1);
if (a='-') then begin; d.Neg:=true;a:='+'; end;
if (a<>'+') then exit;
s:=copy(s,2,666);
i:=nxt;
a:=copy(s,1,i-1);
s:=copy(s,i,666);
AnalizeCpuRegs(a,i);
if (i>=0) then begin;
  d.Typ:=1;
  d.Reg:=i;
  if (s='') then exit;
  s:=copy(s,2,666);
  d.Typ:=0;
  if (copy(s,4,1)<>'+') then exit;
  o:=AnalizeRotates(s);
  if (o<0) then exit;
  s:=copy(s,5,666);
  a:=AnalizeNumbers(s);
  if (a<>'') then begin;
    d.SfN:=(BVal('$'+a) and $ff)+(o shl 16);
    d.Typ:=4;
    exit;
    end;
  a:=AnalizeLabelName(s);
  if (a='') then exit;
  d.SfN:=(o shl 16);
  d.SfL:=a;
  d.Typ:=6;
  exit;
  end;
if (s<>'') then exit;
s:=AnalizeNumbers(a);
if (s<>'') then begin;
  d.Typ:=2;
  d.Num:=BVal('$'+s);
  exit;
  end;
s:=AnalizeLabelName(a);
if (s<>'') then begin;
  d.Typ:=3;
  d.Lab:=s;
  exit;
  end;

End;



Function ConvertNumOrLabDatas(p:OneParameterRec;len:Byte):String;
Var a:String;
Begin;
a:='';
if (p.Typ=2) then begin;
  {Num}
  a:=Right(ZeroFill+p.Num,len*2);
  a:=Hex2ProciDat(a,Len);
  end;
if (p.Typ=3) then begin;
  {Label}
  a:=p.Lab;
  a:=DataSizeNames[Len]+SpxCh+a;
  end;
ConvertNumOrLabDatas:=a;
End;


Function Convert2posNumLabConst(p:OneParameterRec;len,cns,ps,sz:LongInt):String;
Var
  a:String;
  i:LongInt;
Begin;
a:='';
ps:=ps and 31;
sz:=sz and 31;
if (p.Typ=2) then begin;
  {Num}
  a:=Right(ZeroFill+p.Num,len*2);
  i:=BVal('$'+a);
  i:=((1 shl sz)-1) and i;
  i:=(i shl ps) or cns;
  a:=AnalizeNumbers(bstr(i));
  a:=Hex2ProciDat(a,Len);
  end;
if (p.Typ=3) then begin;
  {Label}
  a:=DataSizeNames[Len]+SpxCh+'shl'+SpxCh+AnalizeNumbers(bstr(ps));
  a:=a+SpxCh+'and'+SpxCh+AnalizeNumbers(bstr(((1 shl sz)-1) shl ps));
  a:=a+SpxCh+'or'+SpxCh+AnalizeNumbers(bstr(cns))+SpxCh+p.Lab;
  end;
Convert2posNumLabConst:=a;
End;




Function ConvertMemPointCoproc(d:OneMemPointRec;v:LongInt):String;
Var
  i,o:LongInt;
  a:String;
Begin;
ConvertMemPointCoproc:='';
if (d.Typ=5) then begin; d.Typ:=2;d.Num:=0; end;
v:=v or $01000000;
v:=(d.Bas shl 16) or v;
if not d.Neg then v:=v or $00800000;
if d.Upd then v:=v or $00200000;
case d.Typ of
  2:begin; {number}
    v:=((d.Num shr 2) and $ff) or v;
    ConvertMemPointCoproc:=Hex2ProciDat(AnalizeNumbers(BStr(v)),4);
    exit;
    end;
  3:begin; {label}
    a:='byte'+SpxCh+'shr'+SpxCh+'2'+SpxCh+'and'+SpxCh+'FF'+SpxCh+d.Lab;
    ConvertMemPointCoproc:=a+SepCh+Hex2ProciDat(AnalizeNumbers(BStr(v shr 8)),3);
    exit;
    end;
  else exit;
  end;
End;




Function ConvertMemPointWord(d:OneMemPointRec;v:LongInt):String;
Var
  i,o:LongInt;
  a:String;
Begin;
ConvertMemPointWord:='';
if (d.Typ=5) then begin; d.Typ:=2;d.Num:=0; end;
v:=v or $01000000;
v:=(d.Bas shl 16) or v;
if not d.Neg then v:=v or $00800000;
if d.Upd then v:=v or $00200000;
case d.Typ of
  1:begin; {reg}
    v:=v or $02000000;
    inc(v,d.Reg);
    ConvertMemPointWord:=Hex2ProciDat(AnalizeNumbers(BStr(v)),4);
    exit;
    end;
  4:begin; {sft num}
    v:=v or $02000000;
    inc(v,d.Reg);
    inc(v,(d.SfN and $1f) shl 7);
    inc(v,((d.SfN shr 16) and 3) shl 5);
    ConvertMemPointWord:=Hex2ProciDat(AnalizeNumbers(BStr(v)),4);
    exit;
    end;
  6:begin; {sft lab}
    v:=v or $02000000;
    inc(v,d.Reg);
    inc(v,(d.SfN and 3) shl 5);
    a:='F80'+SpxCh+'or'+SpxCh+AnalizeNumbers(bstr(v))+SpxCh+d.SfL;
    ConvertMemPointWord:='dword'+SpxCh+'shl'+SpxCh+'7'+SpxCh+'and'+SpxCh+a;
    exit;
    end;
  2:begin; {num}
    inc(v,d.Num and $fff);
    ConvertMemPointWord:=Hex2ProciDat(AnalizeNumbers(BStr(v)),4);
    end;
  3:begin; {lab}
    a:='dword'+SpxCh+'and'+SpxCh+'FFF'+SpxCh+'or'+SpxCh;
    ConvertMemPointWord:=a+AnalizeNumbers(bstr(v))+SpxCh+d.Lab;
    end;
  else exit;
  end;
End;



Function ConvertMemPointMisc(d:OneMemPointRec;v:LongInt):String;
Var
  i,o:LongInt;
  a:String;
Begin;
ConvertMemPointMisc:='';
if (d.Typ=5) then begin; d.Typ:=2;d.Num:=0; end;
v:=v or $01000090;
v:=(d.Bas shl 16) or v;
if not d.Neg then v:=v or $00800000;
if d.Upd then v:=v or $00200000;
case d.Typ of
  1:begin; {reg}
    inc(v,d.Reg);
    ConvertMemPointMisc:=Hex2ProciDat(AnalizeNumbers(BStr(v)),4);
    exit;
    end;
  2:begin; {num}
    v:=v or $00400000;
    inc(v,d.Num and $f);
    inc(v,((d.Num shr 4) and $f) shl 8);
    ConvertMemPointMisc:=Hex2ProciDat(AnalizeNumbers(BStr(v)),4);
    end;
  else exit;
  end;
End;



Function ConvertMemPointData(b:String;v:LongInt):String;
Var
  i,o,p:LongInt;
  a:String;
Begin;
ConvertMemPointData:='';
i:=pos(' ',b);
if (i<1) then i:=666;
AnalizeCpuRegs(copy(b,1,i-1),o);
if (o>=0) then begin;
  inc(v,o);
  b:=copy(b,i+1,666);
  if (b='') then begin;
    ConvertMemPointData:=Hex2ProciDat(AnalizeNumbers(BStr(v)),4);
    exit;
    end;
  if (copy(b,4,1)<>' ') then exit;
  i:=AnalizeRotates(b);
  if (i<0) then exit;
  b:=copy(b,5,666);
  inc(v,i shl 5);
  AnalizeCpuRegs(b,o);
  if (o>=0) then begin;
    inc(v,o shl 8);
    inc(v,$10);
    ConvertMemPointData:=Hex2ProciDat(AnalizeNumbers(BStr(v)),4);
    exit;
    end;
  a:=AnalizeNumbers(b);
  if (a<>'') then begin;
    inc(v,(BVal('$'+a) and $1f) shl 7);
    ConvertMemPointData:=Hex2ProciDat(AnalizeNumbers(BStr(v)),4);
    exit;
    end;
  a:=AnalizeLabelName(b);
  if (a='') then exit;
  a:='F80'+SpxCh+'or'+SpxCh+AnalizeNumbers(bstr(v))+SpxCh+a;
  ConvertMemPointData:='dword'+SpxCh+'shl'+SpxCh+'7'+SpxCh+'and'+SpxCh+a;
  exit;
  end;
i:=pos('!',b);
if (i<1) then i:=666;
p:=BVal(copy(b,i+1,666)) and $1e;
b:=copy(b,1,i-1);
v:=((((32-p) and $1e) shl 7) or $02000000) or v;
a:=AnalizeNumbers(b);
if (a<>'') then begin;
  v:=((BVal('$'+a) shr p) and $ff) or v;
  ConvertMemPointData:=Hex2ProciDat(AnalizeNumbers(BStr(v)),4);
  exit;
  end;
a:=AnalizeLabelName(b);
if (a='') then exit;
b:=a;
if (p>0) then a:='shr'+SpxCh+AnalizeNumbers(bstr(p))+SpxCh else a:='';
a:='dword'+SpxCh+a+'and'+SpxCh+'FF'+SpxCh+'or'+SpxCh;
ConvertMemPointData:=a+AnalizeNumbers(bstr(v))+SpxCh+b;
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
AnalizeFpuRegs(s,i);
if (i>=0) then begin; d.Typ:=4;d.Rg2:=i;Exit; end;
a:=AnalizeNumbers(s);
if (a<>'') then begin; d.Typ:=2;d.Num:=a;Exit; end;
AnalizeMemPoint(s,m);
if (m.Typ<>0) then begin; d.Typ:=5;d.Mem:=m;Exit; end;
a:=AnalizeLabelName(s);
if (a<>'') then begin; d.Typ:=3;d.Lab:=a;Exit; end;
End;
