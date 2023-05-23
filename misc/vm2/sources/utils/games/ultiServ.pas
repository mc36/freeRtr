{$heap 31k}
{$stack 3k}
{$sysinc system.inc}
{$sysinc alap.inc}
{$sysinc bugos.inc}
{$sysinc pipeline.inc}
{$sysinc random.inc}

{$include ultiServ-pakli.inc}
{$include ultiServ-mem.inc}
{$include ultiServ-jatek.inc}


Var i:LongInt;
BEGIN;
WriteLn('ulti server v1.0, done by Mc at '#%date' '#%time'.');
randomize;
kezdeniFog:=random(3);
WriteLn('waiting for players...');
pipeLineBegListen;
for i:=1 to 3 do begin;
  receivePlayer(jatekosok[i]);
  jatekosok[i].num:=i;
  uzenetKuldes(i,'sorszamod '+BStr(i));
  end;
pipeLineEndListen;
WriteLn('waiting for names...');
receiveNames;

while (1=1) do doOneTeljesJatek;
END.