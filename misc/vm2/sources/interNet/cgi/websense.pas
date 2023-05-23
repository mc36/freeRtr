{$heap 63k}
{$stack 3k}
{$sysinc system.inc}
{$sysinc alap.inc}
{$sysinc param.inc}
{$sysinc filesys.inc}
{$sysinc filesys2.inc}
{$sysinc textfile.inc}
{$sysinc inet_cgi.inc}


Var
  url,client,page:String;
  i,o:LongInt;
  a,b:String;
  ab:array[0..1] of byte absolute a;
  ab0:byte absolute a;
  t:xtText;
BEGIN;
WriteLn('websense reporter v1.0, done by Mc at '#%date' '#%time'.');
if OpenRequestFile(GetAllParameters) then Halt(1);
if FindNextRequest(True) then Halt(2);

url:='';
client:='';
page:='';
repeat
  a:=Kicsi(CurrentReqNam);
  Case CurrentReqTyp of
    1:begin;
      if (a='url') then url:='/'+ReadRequestString;
      if (a='client') then client:=ReadRequestString;
      end;
    2:begin;
      if (a='url') then page:=ReadRequestString;
      end;
    3:;
    end;
  until FindNextRequest(False);

WriteResponseHeader(3,'html');
a:=GetMyFullFileName;
if (xtOpen(t,xFileName(a,1)+xFileName(a,2)+'.html',true)<>0) then begin;
  WriteLnResponseString('<html><head><title>blocked</title></head><body>');
  WriteLnResponseString('the url ('+page+') is blocked!<br>');
  WriteLnResponseString('</body></html>');
  CloseRequestFile;
  exit;
  end;
while not xtEOF(t) do begin;
  a:=xtReadLn(t,255);
  if (a<>'<url>') then begin;
    WriteLnResponseString(a);
    continue;
    end;
  SeekInResponseFile(-2);
  a:=page;
  for i:=1 to ab0 do WriteResponseString('&#'+BStr(ab[i])+';');
  end;
CloseRequestFile;
END.