{$heap 127k}
{$stack 3k}
{$sysinc system.inc}
{$sysinc filesys.inc}
{$sysinc pipeline.inc}
{$sysinc bugos.inc}
{$sysinc memory.inc}
{$sysinc hex.inc}
{$sysinc param.inc}
{$sysinc crt.inc}
{$sysinc inet_tcp.inc}
{$sysinc inet_addr.inc}
{$sysinc inet_dns.inc}

{$include screen.inc}
{$include connect.inc}
{$include terminal.inc}

Label f1,vege;
Var
  a:String;
  i,o:longint;
BEGIN;
WriteLn('terminal v1.0, done by Mc at '#%date' '#%time'.');

fillchar(TerminalScr,sizeof(TerminalScr),0);
scr_clear;

if openConnection then begin;
  move(currentBuf,TerminalScr.d,sizeof(currentBuf));
  goto vege;
  end;

f1:
doTerminalLoop;
case doMainMenu of
  1:goto f1;
  2:doFileReceive;
  3:doFileSending;
  4:changeEmulator;
  5:doStartCapture;
  6:doSaveScreenshot;
  7:sendOneKey(escapeKey);
  8:doConnectInfo;
  9:goto vege;
  end;
goto f1;

vege:
doFinalClear;
END.