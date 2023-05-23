{$heap 31k}
{$stack 3k}
{$sysinc system.inc}
{$sysinc alap.inc}
{$sysinc bugos.inc}
{$sysinc pipeline.inc}
{$sysinc random.inc}
{$sysinc param.inc}

{$include ultiClnt.inc}

Label f1;
Var
  i,o:LongInt;
  a,b:String;
BEGIN;
WriteLn('ulti client v1.0, done by Mc at '#%date' '#%time'.');
randomize;
a:=GetAllParameters;
if (a='') then immErr('using: csil.code <process>');
ultiKapcsolodik(a,'magdicsil');

f1:
b:=ultiFogad;
a:=kicsi(getNextWord(b));
if (a='') then goto f1;
if (a='sorszamod') then goto f1;
if (a='nevek') then goto f1;
if (a='uzenet') then goto f1;
if (a='ujjatek') then begin;
  writeln('------------------------------------------------');
  goto f1;
  end;
if (a='lapjaid') then goto f1;
if (a='tarsad') then goto f1;
if (a='ellenfel') then goto f1;
if (a='jatek') then goto f1;
if (a='mehet') then goto f1;
if (a='felvette') then goto f1;
if (a='szerinted') then begin;
  ultiKuld('mehet');
  goto f1;
  end;
if (a='talon') then begin;
  ultiKuld('licit '+b+' p');
  goto f1;
  end;
if (a='szine') then begin;
  ultiKuld('szine '+copy('pztm',random(4)+1,1));
  goto f1;
  end;
if (a='jatszuk') then goto f1;
if (a='vanegy') then goto f1;
if (a='kontra') then goto f1;
if (a='ujkor') then goto f1;
if (a='dobott') then goto f1;
if (a='dobjal') then begin;
  ultiKuld('dobok valamit');
  goto f1;
  end;
if (a='renonc') then begin;
  a:=getNextWord(b);
  ultiKuld('dobok '+copy(a,random(length(a) shr 1)*2+1,2));
  goto f1;
  end;
if (a='korveg') then goto f1;
if (a='jatekveg') then goto f1;

immErr('unknown message');
END.