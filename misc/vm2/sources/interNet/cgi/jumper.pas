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
  ProggyName='Browser Redirector v1.0/CGI';


Var
  a:String;
  i:LongInt;
  where:String;
  host:String;
  url:String;

BEGIN;
WriteLn(ProggyName+', done by Mc at '#%date' '#%time'.');
a:=GetAllParameters;
if OpenRequestFile(a) then Halt(1);
if FindNextRequest(True) then Halt(2);

where:='';
host:='';
url:='';
repeat
  a:=Kicsi(CurrentReqNam);
  Case CurrentReqTyp of
    1:begin;
      if (a='host') then host:=ReadRequestString;
      if (a='url') then url:=ReadRequestString;
      end;
    2:begin;
      if (a='where') then where:=ReadRequestString;
      end;
    3:;
    end;
  until FindNextRequest(False);

if (where<>'') then begin;
  if (pos('://',where)=0) then where:='http://'+where+'/';
  WriteResponseHeader(2,where);
  CloseRequestFile;
  WriteLn('Terminated Successfully!');
  halt(0);
  end;
WriteResponseHeader(3,'html');
WriteLnResponseString('<HTML><TITLE>Redirect Browser</TITLE><BODY>');

WriteLnResponseString('Current Server: '+host+'<BR>');
a:='\'+url;Kicserel('\','/',a);
WriteLnResponseString('Current URL: '+a+'<BR>');
WriteLnResponseString('<BR><BR>');
WriteLnResponseString('<FORM ACTION='+a+'>');
WriteLnResponseString('<I>Where would you jump?</I>');
WriteLnResponseString('<INPUT type="text" name="where" value=""><BR><BR>');
WriteLnResponseString('<INPUT type="submit" name="todo" value="Go!">');
WriteLnResponseString('<INPUT type="reset" name="todo" value="Clear">');
WriteLnResponseString('</FORM>');

WriteLnResponseString('<BR><I>Created by '+ProggyName+'</I>');
WriteLnResponseString('</BODY></HTML>');
CloseRequestFile;
WriteLn('Terminated Successfully!');
END.