{$heap 127k}
{$stack 1k}
{$sysinc system.inc}
{$sysinc alap.inc}
{$sysinc crt.inc}
{$sysinc hex.inc}
{$sysinc param.inc}
{$sysinc filesys.inc}
{$sysinc filesys2.inc}
{$sysinc textfile.inc}
{$sysinc bugos.inc}
{$sysinc pipeline.inc}
{$sysinc random.inc}
{$sysinc datetime.inc}

procedure clrEol;
begin;
Write(#13+'                                                                 '#13);
end;

Const
  ProggyName='mailer v1.0';

{$include mailer1.inc}
{$include screen.inc}
{$include mailer2.inc}
{$include mailer3.inc}
{$include mailer4.inc}

Label f1,f2,f3;
Var
  a:String;
  t:xtText;
BEGIN;
WriteLn(proggyName+', done by Mc at '#%date' '#%time'.');
Randomize;

ConsoleSize(ScrSizX,ScrSizY);
dataPath:=ParamStr(1);
if (dataPath='') then dataPath:='.';
dataPath:=repairPath(dataPath);

WriteLn('working path: '+dataPath);

WriteLn('reading system configuration...');
if (xtOpen(t,'c:\system\localHost.text',true)<>0) then exit;
xtReadLn(t,255);
xtReadLn(t,255);
xtReadLn(t,255);
a:=xtReadLn(t,255);
xtClose(t);
TimeZone:=copy(a,1,pos(' ',a)-1);

WriteLn('reading user configuration...');
if (xtOpen(t,dataPath+UserConfiguration,true)<>0) then exit;
userName:=xtReadLn(t,255);
userAddr:=xtReadLn(t,255);
xtClose(t);

CreateMessageIndex(dataPath);
CreateFriendsIndex(dataPath);

MsgSelBeg:=0;MsgSelCur:=1;
MsgPrtBeg:=0;MsgPrtCur:=0;
UsrSelBeg:=0;UsrSelCur:=1;

f1:
case ChooseOneMessage of
  1:begin; {read}
    if UnpackCurrentPart then goto f1;
    executeProgramInMe(myFavoriteViewer,dataPath+UnpackedFilNam);
    goto f1;
    end;
  2:begin; {reply}
    PrepareHeaderForWriting;
    if (EditMessageHeader<>2) then goto f1;
    if UnpackCurrentPart then goto f1;
    if GenerateReplyMsg then goto f1;
    executeProgramInMe(myFavoriteEditor,dataPath+EditingFilNam);
    goto f3;
    end;
  3:begin; {compose}
    fillchar(MsgSelDat,sizeof(MsgSelDat),0);
    PrepareHeaderForWriting;
    MsgSelDat.subj:='';
    if (EditMessageHeader<>2) then goto f1;
    if GenerateNewMsg then goto f1;
    executeProgramInMe(myFavoriteEditor,dataPath+EditingFilNam);
    goto f3;
    end;
  else goto f2;
  end;
goto f1;
f3:
case EditMessageHeader of
  0:goto f1; {cancel}
  1:begin; {save}
    SaveTheMessageForSending;
    goto f1;
    end;
  2:begin; {edit}
    executeProgramInMe(myFavoriteEditor,dataPath+EditingFilNam);
    goto f3;
    end;
  else goto f1;
  end;

goto f1;
f2:
TextColor($07);
clrscr;
DeleteSelectedMessages;
DeleteTemporaryFiles;
END.