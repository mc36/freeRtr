Const
  LinesMax=500;
Type
  OneLineType=string[127];
  AllLinesRec=array[1..LinesMax] of OneLineType;
Var
  AllLinesData:AllLinesRec;
  RemovedLines:LongInt;
  ProcsedLines:LongInt;
  LinesNum:LongInt;
  LinesBeg:LongInt;


Procedure CommentOutOneLine(n:LongInt);
Var a:String;
Begin;
a:=AllLinesData[n];
if (a='') then exit;
if (a[1]=';') then exit;
a:=';;'+a;
AllLinesData[n]:=a;
inc(RemovedLines);
End;
