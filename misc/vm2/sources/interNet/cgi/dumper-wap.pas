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
  MaxCharsInLine=4;

Label f1,f2;
Var
  a,b,d:String;
  c:Char;

Procedure FlushCurrentLine(s:String);
Begin;
While (Length(a)<MaxCharsInLine*3+2) do a:=a+' ';
WriteResponseString(a);
WriteResponseString(b);
WriteResponseString(s);
a:='';b:='';
WriteLnResponseString('<br/>');
End;

BEGIN;
WriteLn(ProggyName+', done by Mc at '#%date' '#%time'.');
a:=GetAllParameters;
if OpenRequestFile(a) then Halt(1);
if FindNextRequest(True) then Halt(2);
WriteResponseHeader(3,'wml');
WriteLnResponseString('<?xml version="1.0" encoding="ISO-8859-1"?>');
WriteLnResponseString('<!DOCTYPE wml PUBLIC "-//WAPFORUM//DTD WML 1.1//EN" "http://www.wapforum.org/DTD/wml_1.1.xml">');
WriteLnResponseString('<wml>');
WriteLnResponseString('<card id="wapdump" title="wap dump">');
WriteLnResponseString('<p>');

WriteLnResponseString('fileName: '+a+'<br/>');
WriteLnResponseString('fileSize: '+alakit(CurrentReqSize)+'<br/>');

repeat
  WriteLnResponseString('');
  WriteLnResponseString(CurrentReqNam+':<br/>');
  Case CurrentReqTyp of
    1:a:='system';
    2:a:='user.data';
    3:a:='user.file';
    else a:='unknown (0x'+Byte2HexType(CurrentReqTyp)+')';
    end;
  WriteLnResponseString('Type: '+a+'<br/>');
  WriteLnResponseString('Size: '+Alakit(CurrentReqLen)+'<br/>');
  a:='';b:='';d:='';
  f1:
  if (d='') then d:=ReadRequestString;
  if (d='') then goto f2;
  c:=d[1];delete(d,1,1);
  a:=a+Byte2HexType(ord(c))+' ';
  if (ord(c) in [0,9,10,13,255]) then c:=#1;
  b:=b+'&#'+BStr(ord(c))+';';
  if (Length(a)=MaxCharsInLine*3) then FlushCurrentLine('');
  goto f1;
  f2:
  FlushCurrentLine('<br/>');
  until FindNextRequest(False);

WriteLnResponseString('<br/>Created by '+ProggyName);
WriteLnResponseString('</p>');
WriteLnResponseString('</card>');
WriteLnResponseString('</wml>');
CloseRequestFile;
WriteLn('Terminated Successfully!');
END.