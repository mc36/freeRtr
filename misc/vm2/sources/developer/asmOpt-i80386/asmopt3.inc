Procedure DoTheCommentRemoval;
Var
  i,o:LongInt;
  a,b:String;
Begin;
for o:=1 to LinesNum do begin;
  a:=AllLinesData[o];
  i:=pos(';',a);
  if (i<1) then i:=$666;
  a:=copy(a,1,i-1);
  a:=xLevesz(a);
  a:=kicsi(a);
  b:=copy(a,1,3);
  if (b='db') or (b='dw') or (a='dd') then continue;
  AllLinesData[o]:=a;
  end;
End;


Procedure DoTheStackOverUsage;
Label f1;
Var
  i,n:LongInt;
  a:String;
  myinstr:Boolean;
Begin;
ClearValuesForProc;
for n:=1 to LinesNum do begin;
  a:=AllLinesData[n];
  FelbontOpcode(a);
  KijavitMemParams;
  myinstr:=(OpCode[0]='pop');
  if myinstr then begin;
    i:=StackValues[StackPointer].l;
    a:=OneValueRead(opCode[1],true);
    end;
  ProcessCurrentInstruction(n);
  if not myinstr then goto f1;
  if (a<>OneValueRead(opCode[1],true)) then goto f1;
  CommentOutOneLine(i);
  CommentOutOneLine(n);
  f1:
  end;
End;

Procedure DoTheMultipleByOne;
Label f1;
Var
  i,n:LongInt;
  a:String;
  myinstr:Boolean;
Begin;
ClearValuesForProc;
for n:=1 to LinesNum do begin;
  a:=AllLinesData[n];
  FelbontOpcode(a);
  KijavitMemParams;
  a:=OpCode[0];
  if (a='imul') then if (OneValueRead(opCode[2],true)='%v%1') then begin;
    CommentOutOneLine(n);
    goto f1;
    end;
  if (a='add') or (a='sub') or (a='or') or (a='xor') then
   if (OneValueRead(opCode[2],true)='%v%0') then begin;
    CommentOutOneLine(n);
    goto f1;
    end;
  if (a='and') then if (OneValueRead(opCode[1],true)='%v%0') then begin;
    CommentOutOneLine(n);
    goto f1;
    end;
  if (a='mul') then if (OneValueRead(opCode[1],true)='%v%1') then begin;
    CommentOutOneLine(n);
    goto f1;
    end;
  ProcessCurrentInstruction(n);
  f1:
  end;
End;

Procedure DoTheDuplicatedMove;
Label f1;
Var
  i,n:LongInt;
  a:String;
  myinstr:Boolean;
Begin;
ClearValuesForProc;
for n:=1 to LinesNum do begin;
  a:=AllLinesData[n];
  FelbontOpcode(a);
  KijavitMemParams;
  if (OpCode[0]='mov') then
   if (OneValueRead(OpCode[1],true)=OneValueRead(OpCode[2],true)) then begin;
    CommentOutOneLine(n);
    goto f1;
    end;
  ProcessCurrentInstruction(n);
  f1:
  end;
End;
