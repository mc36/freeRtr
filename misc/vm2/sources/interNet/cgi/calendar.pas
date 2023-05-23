{$heap 15k}
{$stack 3k}
{$sysinc system.inc}
{$sysinc alap.inc}
{$sysinc hex.inc}
{$sysinc param.inc}
{$sysinc filesys.inc}
{$sysinc textfile.inc}
{$sysinc datetime.inc}
{$sysinc inet_cgi.inc}
Const
  ProggyName='Calendar Creator v1.0/CGi';

Type
  OneMonthRecord=record
    ev:word;
    ho:word;
    het:word;
    d:array[1..7] of array[1..7] of byte;
    end;

Procedure GetOneMonth(ev,ho:integer;var d:OneMonthRecord);
Var nap,het,i:word;
Begin;
fillchar(d,sizeof(d),0);
while (ho<1) do begin; dec(ev);inc(ho,12); end;
while (ho>12) do begin; inc(ev);dec(ho,12); end;
nap:=GetDayOfWeek(ev,ho,1);
if (nap=0) then nap:=7;
d.ev:=ev;
d.ho:=ho;
het:=1;
for i:=1 to GetDaysInMonth(ev,ho) do begin;
  d.d[nap][het]:=i;
  inc(nap);
  if (nap>7) then begin; inc(het);nap:=1; end;
  end;
if (nap=1) then dec(het);
d.het:=het;
End;

Const
  hoNevek:array[1..12] of String[31]=('january','febuary','march','april','may','june','july','august','september','october','november','december');
  napNevek:array[1..7] of String[31]=('monday','tuesday','wednesday','thursday','friday','saturday','sunday');
Var
  a:String;
  i,o,p:LongInt;
  dat:OneMonthRecord;
  url,year,month:String;
  cYear,cMonth,cDay:Word;
BEGIN;
WriteLn(ProggyName+', done by Mc at '#%date' '#%time'.');
a:=GetAllParameters;
if OpenRequestFile(a) then Halt(1);
if FindNextRequest(True) then Halt(2);

url:='';year:='';month:='';
repeat
  a:=Kicsi(CurrentReqNam);
  Case CurrentReqTyp of
    1:begin;
      if (a='url') then url:='/'+ReadRequestString;
      end;
    2:begin;
      if (a='year') then year:=ReadRequestString;
      if (a='month') then month:=ReadRequestString;
      end;
    3:;
    end;
  until FindNextRequest(False);

xGetDate(cYear,cMonth,cDay);

if (bstr(bval(year))<>year) then year:=bstr(cYear);
if (bstr(bval(month))<>month) then month:=bstr(cMonth);

cYear:=bval(year);
cMonth:=bval(month);

WriteResponseHeader(3,'html');
WriteLnResponseString('<html><head><title>Calendar</title></head><body>');

WriteLnResponseString('<form action="'+url+'">');
WriteLnResponseString('Calculate&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;');
WriteLnResponseString('year=<input type=text value='+bstr(cYear)+' name=year size=10 maxlength=6>');
WriteLnResponseString('month=<input type=text value='+bstr(cMonth)+' name=month size=10 maxlength=6>');
WriteLnResponseString('<input type=submit value="Go!">');
WriteLnResponseString('</form><hr>');

WriteLnResponseString('');
for i:=-5 to 6 do begin;
  GetOneMonth(cYear,cMonth+i,dat);
  WriteLnResponseString('<b><i><u>'+BStr(dat.ev)+'. '+HoNevek[dat.ho]+'</u></i></b>');
  WriteLnResponseString('<table border=2 cellspacing=2 cellpadding=0>');
  for o:=1 to 7 do begin;
    WriteLnResponseString('<tr>');
    WriteLnResponseString('  <td align=right><b>'+NapNevek[o]+'</b></td>');
    for p:=1 to dat.het do begin;
      a:=bstr(dat.d[o][p]);
      if (a='0') then a:='&nbsp;';
      WriteLnResponseString('  <td align=center>'+a+'</td>');
      end;
    WriteLnResponseString('  </tr>');
    end;
  WriteLnResponseString('</table><br>');
  WriteLnResponseString('');
  end;

WriteLnResponseString('<br><i>Created by '+ProggyName+'</i>');
WriteLnResponseString('</body>');
WriteLnResponseString('</html>');
CloseRequestFile;
END.