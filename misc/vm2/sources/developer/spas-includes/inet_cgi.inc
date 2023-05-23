Var
  CurrentReqFile:xFile;
  CurrentResFile:xtText;
  CurrentReqSize:LongInt;
  CurrentReqDat:LongInt;
  CurrentReqBeg:LongInt;
Var
  CurrentReqNam:String;
  CurrentReqPos:longInt;
  CurrentReqLen:LongInt;
  CurrentReqTyp:Byte;    {1=system, 2=user, 3=file}


Function OpenRequestFile(s:String):Boolean;
Begin;
OpenRequestFile:=True;
if (xOpen(CurrentReqFile,s+'.q',xGenFilMod_r)<>0) then Exit;
if (xtOpen(CurrentResFile,s+'.r',False)<>0) then Exit;
CurrentReqSize:=xFileSize(CurrentReqFile);
OpenRequestFile:=False;
End;

Procedure CloseRequestFile;
Begin;
xClose(CurrentReqFile);
xtTruncate(CurrentResFile);
xtClose(CurrentResFile);
End;

Function FindNextRequest(first:Boolean):Boolean;
Var
  namLen:Byte absolute CurrentReqNam;
  i:LongInt;
Begin;
FindNextRequest:=True;
if first then CurrentReqBeg:=0 else begin;
  xSeek(CurrentReqFile,CurrentReqBeg);
  xBlockRead(CurrentReqFile,i,SizeOf(i));
  if (i=0) then Exit;
  if (i>=CurrentReqSize) then Exit;
  CurrentReqBeg:=i;
  end;
xSeek(CurrentReqFile,CurrentReqBeg);
xBlockRead(CurrentReqFile,i,SizeOf(i));
if (i=0) then Exit;
if (i>CurrentReqSize) then Exit;
xBlockRead(CurrentReqFile,CurrentReqTyp,SizeOf(CurrentReqTyp));
xBlockRead(CurrentReqFile,namLen,sizeof(namLen));
xBlockRead(CurrentReqFile,CurrentReqNam[1],namLen);
CurrentReqPos:=0;
CurrentReqDat:=xFilePos(CurrentReqFile);
CurrentReqLen:=i-CurrentReqDat;
if (CurrentReqLen<0) then Exit;
FindNextRequest:=False;
End;

Procedure SeekRequestData(p:LongInt);
Begin;
if (p<0) then p:=0;
if (p>CurrentReqLen) then p:=CurrentReqLen;
CurrentReqPos:=p;
End;

function ReadRequestData(Var d;s:LongInt):Word;
Label f1;
Const Max=$8000;
Var i:LongInt;
Begin;
i:=CurrentReqLen-CurrentReqPos;
if (i>Max) then i:=Max;
if (s>i) then s:=i;
if (s=0) then goto f1;
xSeek(CurrentReqFile,CurrentReqDat+CurrentReqPos);
xBlockRead(CurrentReqFile,d,s);
inc(CurrentReqPos,s);
f1:
ReadRequestData:=s;
End;

Function ReadRequestString:String;
Var
  a:String;
  as:Byte absolute a;
Begin;
as:=ReadRequestData(a[1],$ff);
ReadRequestString:=a;
End;

Procedure WriteResponseHeader(typ:Byte;str:String);
{types: 1-error, 2-moved, 3-successStdType, 4-successCustType}
Begin;
xtSetPos(CurrentResFile,0);
xtWrite(CurrentResFile,Chr(typ)+str[0]);
xtWrite(CurrentResFile,str);
End;

Procedure SeekInResponseFile(movement:LongInt);
Begin;
xtSetPos(CurrentResFile,xtGetPos(CurrentResFile)+movement);
End;

Procedure WriteResponseString(s:String);
Begin;
xtWrite(CurrentResFile,s);
End;

Procedure WriteLnResponseString(s:String);
Begin;
xtWrite(CurrentResFile,s);
xtWrite(CurrentResFile,#13#10);
End;

Procedure WriteResponseData(var d;s:Word);
Var
  dd:array[1..1] of Char absolute d;
  i:Word;
Begin;
for i:=1 to s do xtWrite(CurrentResFile,dd[i]);
End;
