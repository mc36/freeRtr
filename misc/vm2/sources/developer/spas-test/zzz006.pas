{$stack 512}
{$heap 65500}
{$sysinc system.inc}

Procedure OneTest(max:Word);
Var i,o:Word;
Begin;
for o:=1 to 10 do begin;
  i:=1;
  while (i<10) do begin;
    Write(' '+BStr(i));
    if (i=max) then break;
    inc(i);
    end;
  WriteLn('');
  i:=1;
  repeat
    Write(' '+BStr(i));
    if (i=max) then break;
    inc(i);
    until (i>=10);
  WriteLn('');
  for i:=1 to 9 do begin;
    Write(' '+BStr(i));
    if (i=max) then break;
    end;
  WriteLn('');
  if (o>=1) then exit;
  end;
End;

var i:byte;
BEGIN;
OneTest(3);
OneTest(5);
OneTest(7);
OneTest(11);
END.