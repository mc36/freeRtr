Function GetAllParameters:String;
Begin;
asm VarAddr trg @result;
asm add ? trg 1;
asm sysCall file.MyParam;
asm VarAddr trg @result;
asm movW dubub [trg] c;
End;

Function GetMyFullFileName:String;
Begin;
asm VarAddr trg @result;
asm add ? trg 1;
asm sysCall file.MyName;
asm VarAddr trg @result;
asm movW dubub [trg] c;
End;

Function ParamCount:Word;
Var
  i,o:Byte;
  a:String;
Begin;
a:=GetAllParameters;
o:=0;
for i:=1 to Length(a) do if (a[i]=' ') then inc(o);
inc(o,a<>'');
ParamCount:=o;
End;

Function ParamStr(Index:Byte):String;
Label f1,f2;
Var
  i,o:Byte;
  a:String;
Begin;
if (index=0) then begin;
  ParamStr:=GetMyFullFileName;
  exit;
  end;
a:=GetAllParameters;
o:=1;
f1:
if (o=Index) then Goto f2;
if (Copy(a,1,1)=' ') then inc(o);
a:=Copy(a,2,255);
if (a='') then Goto f2;
Goto f1;
f2:
o:=Length(a)+1;
for i:=o downto 1 do if (a[i]=' ') then o:=i;
a:=Copy(a,1,o-1);
ParamStr:=a;
End;
