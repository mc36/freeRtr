{$stack 2k}
{$heap 64k}
{$sysinc system.inc}
{$sysinc param.inc}
{$sysinc alap.inc}
{$sysinc bin.inc}
{$sysinc hex.inc}
{$sysinc datetime.inc}
{$sysinc filesys.inc}
{$sysinc filesys2.inc}
{$sysinc textfile.inc}

Const PrgTxt='sAsm-m68000 v1.0, done by Mc at '#%date' '#%time'.';

{$define doCPU}
{$define doFPU}
{$define doSYN}

{$include comp_gen.inc}
{$ifdef doCPU}{$include comp_cpu.inc}{$endif}
{$ifdef doFPU}{$include comp_fpu.inc}{$endif}
{$ifdef doSYN}{$include comp_syn.inc}{$endif}

Label nel,el;

Function ConvertInText(s:String):String;
Var
  d:String;
  i,o:LongInt;
  c,oc:LongInt;
  quo:Boolean;
Begin;
oc:=-1;
quo:=False;
d:='';
While (s<>'') do begin;
  c:=Ord(s[1]);
  delete(s,1,1);
  if (c=59) and (quo=False) then begin;
    c:=-c;
    s:='';
    end;
  if (c=39) then begin;
    quo:=not quo;
    if (oc<>-c) then c:=-c;
    if (quo=False) then begin;
      s:=xLevesz(s);
      if (Copy(s,1,1)=';') then s:='';
      d:=d+InternalNumber;
      if (s<>'') then d:=d+',';
      end;
    if (quo=False) and (Copy(s,1,1)=',') then d:=Copy(d,1,Length(d)-1);
    end;
  if (quo=False) then begin;
    if (c in [$41..$5a]) then inc(c,$20);
    end;
  if (c>=0) then begin;
    if (quo=False) then begin;
      if (oc=c) and (c=32) then else d:=d+Chr(c);
      end else begin;
      d:=d+b2h(c);
      end;
    end;
  oc:=c;
  end;

d:=xLevesz(d);
ConvertInText:=d;
End;

Const
  MaxIncs=10;
Var
  td:Array[1..maxincs] of record
    n:String;
    t:xtText;
    i:LongInt;
    end;
  tn:LongInt;
  tt:xtText;
  a,b,c:String;
  i:LongInt;


Procedure WriteErr(s:String);
Begin;
Write(#13+'(');
Write(BStr(td[tn].i));
Write(') ');
Write(td[tn].n);
Write(': ');
Write(s);
WriteLn('');
Halt(1);
End;

Procedure OutOneLine(a,c:String);
Begin;
a:=RepairOutData(a);
xtWrite(tt,a);
a:=Copy('                                            ',1,45-Length(a));
xtWriteLn(tt,a+' ;'+c);
End;

Procedure UpdateCurrentFile;
Begin;
clreol;
Write('(      ) ');
if (tn>0) then Write(td[tn].n);
End;



Procedure OpenNewFile(a:String);
Begin;
if (tn>0) then a:=RepairOneFileName(a,td[tn].n);
inc(tn);
if (tn>MaxIncs) then begin;
  dec(tn);
  WriteErr('Too may includes!');
  end;
td[tn].n:=a;
td[tn].i:=0;
if (xtOpen(td[tn].t,a,true)<>0) then WriteErr('Error opening file!');
UpdateCurrentFile;
Kicserel('\','/',a);
OutOneLine(SepCh+'filebeg'+SpxCh+a+SepCh,'');
End;

Procedure CloseOldFile;
Begin;
OutOneLine(SepCh+'fileend'+SepCh,'');
if (tn>0) then xtClose(td[tn].t);
dec(tn);
if (tn<0) then tn:=0;
UpdateCurrentFile;
End;

Function GetNextLine:String;
Var
  a:String;
Begin;
if xtEOF(td[tn].t) then begin;
  a:=#13;
  end else begin;
  inc(td[tn].i);
  a:='';
  while not xtEOL(td[tn].t) do a:=a+xtRead(td[tn].t,255);
  a:=a+xtReadLn(td[tn].t,255);
  end;
GetNextLine:=a;
End;


Procedure resetState;
Begin;
CoprocessorId:=1 shl 25;
tn:=0;
End;



BEGIN;
WriteLn(PrgTxt);

c:=ParamStr(1);
if (xFileName(c,2)='') then begin;
  WriteLn('Using: sAsm.code <AsmFile>');
  Halt(2);
  end;
WriteLn('Assembling '+c+'...');
resetState;
UpdateCurrentFile;

a:=xFileName(c,1)+xFileName(c,2)+'.obj';
xErase(a);
if (xCreate(a)<>0) then WriteErr('Error creating obj file!');
if (xtOpen(tt,a,false)<>0) then WriteErr('Error opening obj file!');
OpenNewFile(c);

While (tn>0) do begin;
  OutOk:=False;OutStr:='';
  a:='';c:=GetNextLine;
  Write(#13'('+BStr(td[tn].i));
  if (Copy(c,1,1)=#13) then begin;
    CloseOldFile;
    Goto nel;
    end;
  b:=ConvertInText(c);
  if (b='') then Goto el;
  a:='include';
  if (Copy(b,1,Length(a))=a) then begin;
    delete(b,1,Length(a));
    b:=xLevesz(b);
    OpenNewFile(b);
    Goto nel;
    end;

  InStr:=b;
  OutOk:=False;OutStr:='';
  {$ifdef doCPU}if (OutOk=False) then CompileOneCpuLine;{$endif}
  {$ifdef doFPU}if (OutOk=False) then CompileOneFpuLine;{$endif}
  {$ifdef doSYN}if (OutOk=False) then CompileOneSyntaxLine;{$endif}
  if (OutOk=False) then WriteErr('Error: '''+c+'''');
  el:
  OutOneLine(OutStr,c);
  nel:
  end;

xtClose(tt);

clreol;
WriteLn('Successfully finished!');
Halt(0);
END.