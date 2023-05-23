{$heap 11k}
{$stack 3k}
{$sysinc system.inc}
{$sysinc alap.inc}
{$sysinc hex.inc}
{$sysinc param.inc}
{$sysinc filesys.inc}
{$sysinc textfile.inc}
{$sysinc inet_cgi.inc}
Const
  ProggyName='Request Dumper v1.0/CGI';
  MaxCharsInLine=18;


Label f1,f2;
Var
  a,b,d:String;
  c:Char;
  cc:Byte absolute c;

Procedure FlushCurrentLine(s:String);
Begin;
While (Length(a)<MaxCharsInLine*3+2) do a:=a+' ';
WriteResponseString(a);
WriteResponseString(b);
WriteLnResponseString(s);
a:='';b:='';
End;

BEGIN;
WriteLn(ProggyName+', done by Mc at '#%date' '#%time'.');
a:=GetAllParameters;
if OpenRequestFile(a) then Halt(1);
if FindNextRequest(True) then Halt(2);
WriteResponseHeader(3,'html');
WriteLnResponseString('<HTML><TITLE>Request Dump</TITLE><BODY>');

WriteLnResponseString('fileName: '+a+'<BR>');
WriteLnResponseString('fileSize: '+BStr(CurrentReqSize)+'<BR>');

repeat
  WriteLnResponseString('');
  WriteResponseString('<H2><U>'+CurrentReqNam+'</U></H2>');
  Case CurrentReqTyp of
    1:a:='system';
    2:a:='user.data';
    3:a:='user.file';
    else a:='unknown (0x'+Byte2HexType(CurrentReqTyp)+')';
    end;
  WriteLnResponseString('Type: '+a+'<BR>');
  WriteResponseString('Size: '+Alakit(CurrentReqLen));
  WriteResponseString('<PRE>');
  a:='';b:='';d:='';
  f1:
  if (d='') then d:=ReadRequestString;
  if (d='') then goto f2;
  c:=d[1];delete(d,1,1);
  a:=a+Byte2HexType(cc)+' ';
  if (cc in [0,9,10,13,255]) then cc:=$01;
  b:=b+'&#'+BStr(cc)+';';
  if (Length(a)=MaxCharsInLine*3) then FlushCurrentLine('');
  goto f1;
  f2:
  FlushCurrentLine('</PRE>');
  until FindNextRequest(False);

WriteLnResponseString('<BR><I>Created by '+ProggyName+'</I>');
WriteLnResponseString('</BODY></HTML>');
CloseRequestFile;
WriteLn('Terminated Successfully!');
END.