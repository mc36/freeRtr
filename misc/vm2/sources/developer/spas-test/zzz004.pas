{$stack 512}
{$heap 65500}
{$sysinc system.inc}

Var
  i:LongInt;
  p:Pointer;
  ip:^LongInt;
  o:longint absolute i;
BEGIN;
i:=8;
p:=@i;
p:=p^;
ip:=p^;
inc(ip^);
inc(o);
WriteLn(BStr(i));
END.