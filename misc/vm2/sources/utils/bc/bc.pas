{$stack 16k}
{$heap 127k}
{$sysinc system.inc}
{$sysinc param.inc}
{$sysinc hex.inc}
{$sysinc alap.inc}
{$sysinc crt.inc}
{$sysinc filesys.inc}
{$sysinc filesys2.inc}
{$sysinc textfile.inc}
{$sysinc bugos.inc}

Const
  PrgTxt='BugOS Commander v1.0, done by Mc at '#%date' '#%time'.';
  ColNorm:Byte=$07;
  ColPanB:Byte=$17;
  ColPanD:Byte=$1f;
  ColPanF:Byte=$1b;
  ColPanE:Byte=$1a;
  ColPanS:Byte=$1e;
  ColPanC:Byte=$2f;
  ColWinB:Byte=$71;
  ColWinT:Byte=$70;
  ColErrB:Byte=$47;
  ColErrT:Byte=$4f;
  CfgEdit:String='c:\utils\editor.code';
  CfgView:String='c:\utils\viewer.code';
  CfgVert:Boolean=true;
  CfgTwpn:Boolean=true;

{$include panel.inc}
{$include screen.inc}
{$include bc1.inc}
{$include bc2.inc}


Label f1,f2,f3,vege;
Var i,o,p:LongInt;
BEGIN;
WriteLn(PrgTxt);
fillchar(PanelDat,sizeof(PanelDat),0);
ReadUpConfiguration;
PanelResizeAll;
PromptString:='';
PanelCur:=2;

for i:=1 to 2 do begin;
  PanelDat[i].pat:=xGetDir;
  PanelReadUpOne(i);
  PanelSortOne(i);
  end;
PanelSelectCurr;
RefreshScr:=$ff;
goto f3;

f1:
o:=PanelDat[PanelCur].cur;
p:=PanelDat[PanelCur].beg;
f2:
i:=ReadKey;
if ProcessOneKey(i) then goto vege;
PanelAlignCheckOne(PanelDat[PanelCur]);
if keypressed then goto f2;
if (RefreshScr=0) then begin;
  if (p<>PanelDat[PanelCur].beg) then begin; RefreshScr:=2;goto f3; end;
  if (o=PanelDat[PanelCur].cur) then goto f1;
  PanelPutOutLine(PanelDat[PanelCur],o);
  PanelPutOutLine(PanelDat[PanelCur],PanelDat[PanelCur].cur);
  PanelPutOutCurrnt(PanelDat[PanelCur]);
  PutOutCursor;
  goto f1;
  end;
f3:
if (RefreshScr and $80<>0) then begin;
  PanelResizeAll;
  textColor(ColNorm);clrscr;
  RefreshScr:=$40;
  end;
if CfgTwpn then begin;
  if (RefreshScr and $40<>0) then begin;
    for i:=1 to 2 do PanelDrawOutOne(PanelDat[i]);
    RefreshScr:=$20;
    end;
  if (RefreshScr and $20<>0) then begin;
    for i:=1 to 2 do begin;
      PanelPutOutWhole(PanelDat[i]);
      PanelPutOutCurrnt(PanelDat[i]);
      end;
    RefreshScr:=1;
    end;
  end else RefreshScr:=(RefreshScr shr 4) or RefreshScr or 1;
if (RefreshScr and 4<>0) then begin;
  PanelDrawOutOne(PanelDat[PanelCur]);
  RefreshScr:=RefreshScr or 2;
  end;
if (RefreshScr and 2<>0) then begin;
  PanelPutOutWhole(PanelDat[PanelCur]);
  PanelPutOutCurrnt(PanelDat[PanelCur]);
  end;
if (RefreshScr and 1<>0) then PutOutPrompt;
PutOutCursor;
RefreshScr:=0;
goto f1;

vege:
GotoXY(1,ScrSizY);
textColor($07);
WriteLn('');
Halt(0);
END.
