{$stack 512}
{$heap 65500}
{$sysinc system.inc}


Var
  i:integer;
  a:string;
BEGIN;
i:=-10;
repeat
  case i of
    1:a:='egy';
    2:a:='ketto';
    3:a:='harom';
    4:a:='negy';
    5:a:='ot';
    6:a:='hat';
    7:a:='het';
    8:a:='nyolc';
    9:a:='kilenc';
    else a:=BStr(i);
    end;
  write('hello '+a+'!'#13#10);
  inc(i);
  until (i>10);

END.