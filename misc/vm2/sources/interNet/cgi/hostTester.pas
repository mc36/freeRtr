{$heap 11k}
{$stack 3k}
{$sysinc system.inc}
{$sysinc alap.inc}
{$sysinc hex.inc}
{$sysinc param.inc}
{$sysinc filesys.inc}
{$sysinc textfile.inc}
{$sysinc memory.inc}
{$sysinc inet_addr.inc}
{$sysinc inet_cgi.inc}
Const ProggyName='Host Tester v1.0/CGI';
Label f1,Vege;
Var
  a:String;
  i:LongInt;
  client:String;
  host:String;
  url:String;
  browser:String;

Procedure PutOne(l,n:String);
Begin;
WriteResponseString('Click <A HREF="');
WriteResponseString(l);
WriteResponseString(host);
WriteResponseString('">here</A> to test my ');
WriteResponseString(n);
WriteResponseString(' server.');
WriteLnResponseString('<BR>');
End;

BEGIN;
WriteLn(ProggyName+', done by Mc at '#%date' '#%time'.');
a:=GetAllParameters;
if OpenRequestFile(a) then Halt(1);
if FindNextRequest(True) then Halt(2);

client:='';
host:='';
url:='';
browser:='';
repeat
  a:=Kicsi(CurrentReqNam);
  Case CurrentReqTyp of
    1:begin;
      if (a='browser') then browser:=ReadRequestString;
      if (a='host') then host:=ReadRequestString;
      if (a='url') then url:='/'+ReadRequestString;
      if (a='client') then client:=ReadRequestString;
      end;
    2:;
    3:;
    end;
  until FindNextRequest(False);

WriteResponseHeader(3,'html');
WriteLnResponseString('<HTML><TITLE>Servers on this Host</TITLE><BODY>');

a:=ipAddr2string(client[1]);
WriteLnResponseString('Your IP Address: '+a+'<BR>');
WriteLnResponseString('Your Browser: '+browser+'<BR>');
WriteLnResponseString('Current Server: '+host+'<BR>');
WriteLnResponseString('Current URL: '+a+'<BR>');
WriteLnResponseString('<BR>');

WriteLnResponseString('<HR>');
PutOne('ftp://','FTP');
PutOne('http://','HTTP');
PutOne('telnet://','TelNet');
PutOne('ssh://','ssh');
PutOne('mailto:postmaster@','smtp');
WriteLnResponseString('<HR>');

WriteLnResponseString('<BR><I>Created by '+ProggyName+'</I>');
WriteLnResponseString('</BODY></HTML>');
Vege:
CloseRequestFile;
WriteLn('Terminated Successfully!');
END.