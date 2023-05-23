Procedure ReadUpConfiguration;
Var
  t:xtText;
  a:String;
  i:LongInt;
Begin;
a:=xFileName(ParamStr(0),1)+xFileName(ParamStr(0),2)+'.cfg';
if (xtOpen(t,a,true)<>0) then exit;
ColNorm:=BVal(xtReadLn(t,255));
ColPanB:=BVal(xtReadLn(t,255));
ColPanD:=BVal(xtReadLn(t,255));
ColPanF:=BVal(xtReadLn(t,255));
ColPanE:=BVal(xtReadLn(t,255));
ColPanS:=BVal(xtReadLn(t,255));
ColPanC:=BVal(xtReadLn(t,255));
ColWinB:=BVal(xtReadLn(t,255));
ColWinT:=BVal(xtReadLn(t,255));
ColErrB:=BVal(xtReadLn(t,255));
ColErrT:=BVal(xtReadLn(t,255));
CfgEdit:=xtReadLn(t,255);
CfgView:=xtReadLn(t,255);
CfgVert:=(BVal(xtReadLn(t,255))=1);
CfgTwpn:=(BVal(xtReadLn(t,255))=1);
xtClose(t);
for i:=1 to paramCount do begin;
  a:=kicsi(paramStr(i));
  if (a='vert') then CfgVert:=true;
  if (a='hori') then CfgVert:=false;
  if (a='two') then CfgTwpn:=true;
  if (a='one') then CfgTwpn:=false;
  end;
End;

Procedure FinWordChangeDrv(var a:String;p:String);
Begin;
if (kicsi(copy(p,1,3))=kicsi(copy(a,1,3))) then a:=p;
End;




Function ProcessOneKey(w:Word):Boolean;
Label f1,f2;
Var
  d:OneFileRecord;
  p:pointer;
  i,o:LongInt;
  a,b:String;
Begin;
ProcessOneKey:=False;
case w of
  $8001:RefreshScr:=$ff;
  $801d:begin;{f10}
    xChDir(PanelDat[PanelCur].pat);
    ProcessOneKey:=True;
    Exit;
    end;
  $8002:begin;{tab}
    inc(PanelCur);
    PanelSelectCurr;
    RefreshScr:=RefreshScr or $20;
    end;
  $8008:PanelDat[PanelCur].cur:=1;{home}
  $8009:PanelDat[PanelCur].cur:=PanelDat[PanelCur].max;{end}
  $800a:dec(PanelDat[PanelCur].cur,PanelDat[PanelCur].ScrSY shr 1);{pgup}
  $800b:inc(PanelDat[PanelCur].cur,PanelDat[PanelCur].ScrSY shr 1);{pgdn}
  $800c:dec(PanelDat[PanelCur].cur);{up}
  $800d:inc(PanelDat[PanelCur].cur);{down}
  $8014:begin;{f1}
    ProcessHelpScreen;
    RefreshScr:=RefreshScr or $40;
    end;
  $8015:begin;{f2}
    DoWorkCompare(PanelDat[PanelCur],PanelDat[3-PanelCur]);
    RefreshScr:=RefreshScr or $40;
    PanelSelectCurr;
    end;
  $8016:begin;{f3}
    if PanelCurrEntry(d) then exit;
    if (d.flg and xRights_Directory<>0) then exit;
    DoWorkExecPrg(CfgView,d.nam,false);
    end;
  $8017:begin;{f4}
    if PanelCurrEntry(d) then exit;
    if (d.flg and xRights_Directory<>0) then exit;
    DoWorkExecPrg(CfgEdit,d.nam,false);
    end;
  $8018:begin;{f5}
    DoWorkCopy(PanelDat[PanelCur],PanelDat[3-PanelCur].pat);
    RefreshScr:=RefreshScr or $40;
    PanelRereadOneNow(3-PanelCur);
    PanelSelectCurr;
    end;
  $8019:begin;{f6}
    DoWorkRename(PanelDat[PanelCur],PanelDat[3-PanelCur].pat);
    RefreshScr:=RefreshScr or $40;
    PanelRereadOneNow(PanelCur);
    PanelSelectCurr;
    end;
  $801a:begin;{f7}
    RefreshScr:=RefreshScr or $40;
    a:='';
    if AskForOneText('make directory','enter name of new directory:',a) then exit;
    w:=xMkDir(PanelDat[PanelCur].pat+a);
    if ShowErrorMessage(w,'mkdir '+a) then exit;
    PanelDat[PanelCur].cur:=1;
    entriesBuf^[PanelDat[PanelCur].mem].nam:=a;
    PanelRereadOneNow(PanelCur);
    end;
  $801b:begin;{f8}
    DoWorkDelete(PanelDat[PanelCur]);
    RefreshScr:=RefreshScr or $40;
    PanelRereadOneNow(PanelCur);
    end;
  $801c:begin;{f9}
    DoWorkUserMenu(a,b);
    RefreshScr:=RefreshScr or $40;
    if (a<>'') then DoWorkExecPrg(a,b,true);
    end;
  $8418:begin;{alt+f5}
    if PanelCurrEntry(d) then exit;
    b:=d.nam;
    a:=b;
    RefreshScr:=RefreshScr or $40;
    if AskForOneText('make link','enter link name of '+a+':',a) then exit;
    if (kicsi(a)=kicsi(b)) then exit;
    w:=xMkLink(PanelDat[PanelCur].pat+b,PanelDat[PanelCur].pat+a);
    if ShowErrorMessage(w,'mklink '+b+'-->'+a) then exit;
    PanelDat[PanelCur].cur:=1;
    entriesBuf^[PanelDat[PanelCur].mem].nam:=a;
    PanelRereadOneNow(PanelCur);
    end;
   $8419:begin;{alt+f6}
    if PanelCurrEntry(d) then exit;
    b:=d.nam;
    a:=b;
    RefreshScr:=RefreshScr or $40;
    if AskForOneText('rename','enter new name of '+a+':',a) then exit;
    if (kicsi(a)=kicsi(b)) then exit;
    w:=xRename(PanelDat[PanelCur].pat+b,PanelDat[PanelCur].pat+a);
    if ShowErrorMessage(w,'rename '+b+'-->'+a) then exit;
    PanelDat[PanelCur].cur:=1;
    entriesBuf^[PanelDat[PanelCur].mem].nam:=a;
    PanelRereadOneNow(PanelCur);
    end;
  $8415:begin;{alt+f2}
    w:=2;
    a:='right';
    goto f2;
    end;
  $8414:begin;{alt+f1}
    w:=1;
    a:='left';
    f2:
    RefreshScr:=RefreshScr or $40;
    a:=DoWorkChangeDrv(a,PanelDat[w].pat);
    if (a='') then exit;
    FinWordChangeDrv(a,PanelDat[3-PanelCur].pat);
    FinWordChangeDrv(a,PanelDat[PanelCur].pat);
    PanelDat[w].pat:=a;
    PanelRereadOneNow(w);
    PanelAlignCheckOne(PanelDat[w]);
    PanelSelectCurr;
    end;
  $8006:begin;{insert}
    if PanelCurrEntry(d) then exit;
    d.sel:=not d.sel;
    entriesBuf^[PanelDat[PanelCur].cur+PanelDat[PanelCur].mem-1]:=d;
    inc(PanelDat[PanelCur].cur);
    RefreshScr:=RefreshScr or 2;
    end;
  $002a:begin;{*}
    doSelectAllFiles(PanelDat[PanelCur],true);
    RefreshScr:=RefreshScr or 2;
    end;
  $022a:begin;{ctrl+*}
    doSelectAllFiles(PanelDat[PanelCur],false);
    RefreshScr:=RefreshScr or 2;
    end;
  $002b:begin;{+}
    RefreshScr:=RefreshScr or $40;
    a:='';
    if AskForOneText('select files','enter file mask to select:',a) then exit;
    doSelectFileMask(PanelDat[PanelCur],a,true);
    end;
  $002d:begin;{-}
    RefreshScr:=RefreshScr or $40;
    a:='';
    if AskForOneText('deselect files','enter file mask to deselect:',a) then exit;
    doSelectFileMask(PanelDat[PanelCur],a,false);
    end;
  $8004:begin;{enter}
    while (copy(PromptString,1,1)=' ') do PromptString:=copy(PromptString,2,255);
    if (PromptString<>'') then begin;
      i:=pos(' ',PromptString);
      if (i<1) then i:=$666;
      a:=copy(PromptString,1,i-1);
      b:=copy(PromptString,i+1,255);
      DoWorkExecPrg(a,b,true);
      PromptString:='';
      exit;
      end;
    f1:
    if PanelCurrEntry(d) then exit;
    if (d.flg and xRights_Directory<>0) then begin;
      RefreshScr:=RefreshScr or 4;
      a:=d.nam;
      if (a<>'..') then begin;
        PanelDat[PanelCur].pat:=PanelDat[PanelCur].pat+a+'\';
        PanelReadUpOne(PanelCur);
        PanelSortOne(PanelCur);
        exit;
        end;
      a:=PanelDat[PanelCur].pat;
      a:=copy(a,1,length(a)-1);
      o:=pos('\',a);
      for i:=1 to length(a) do if (a[i]='\') then o:=i;
      PanelDat[PanelCur].pat:=copy(a,1,o);
      a:=copy(a,o+1,255);
      PanelReadUpOne(PanelCur);
      PanelSortOne(PanelCur);
      i:=PanelFindOneFile(PanelDat[PanelCur],a);
      if (i>0) then PanelDat[PanelCur].cur:=i;
      exit;
      end;
    if (d.flg and (xRights_OwnExec or xRights_AnyExec)=0) then exit;
    DoWorkExecPrg(PanelDat[PanelCur].pat+d.nam,'',true);
    end;
  $8204:begin;{ctrl+enter}
    if PanelCurrEntry(d) then exit;
    PromptString:=PromptString+d.nam;
    RefreshScr:=RefreshScr or 1;
    end;
  $8304:begin;{shift+ctrl+enter}
    if PanelCurrEntry(d) then exit;
    PromptString:=PromptString+PanelDat[PanelCur].pat+d.nam;
    RefreshScr:=RefreshScr or 1;
    end;
  $025b:begin;{ctrl+[}
    PromptString:=PromptString+PanelDat[1].pat;
    RefreshScr:=RefreshScr or 1;
    end;
  $025d:begin;{ctrl+]}
    PromptString:=PromptString+PanelDat[2].pat;
    RefreshScr:=RefreshScr or 1;
    end;
  $025c:begin;{ctrl+\}
    PanelDat[PanelCur].pat:=copy(PanelDat[PanelCur].pat,1,3);
    PanelRereadOneNow(PanelCur);
    RefreshScr:=RefreshScr or 4;
    end;
  $8003:begin;{backspace}
    PromptString:=copy(PromptString,1,length(PromptString)-1);
    RefreshScr:=RefreshScr or 1;
    end;
  $8005:begin;{escape}
    PromptString:='';
    RefreshScr:=RefreshScr or 1;
    end;
  $0472:begin;{alt+r}
    PanelRereadOneNow(PanelCur);
    RefreshScr:=RefreshScr or $20;
    end;
  $0464:begin;{alt+d}
    DoWorkDiskInfo(PanelDat[PanelCur].pat);
    RefreshScr:=RefreshScr or $40;
    end;
  $0466:begin;{alt+f}
    if PanelCurrEntry(d) then exit;
    DoWorkFileInfo(PanelDat[PanelCur].pat,d.nam);
    RefreshScr:=RefreshScr or $40;
    end;
  $0461:begin;{alt+a}
    if PanelCurrEntry(d) then exit;
    DoWorkFileRights(PanelDat[PanelCur].pat,d.nam);
    RefreshScr:=RefreshScr or $40;
    end;
  $820a:begin;{ctrl+pgup}
    if PanelDat[PanelCur].rot then exit;
    PanelDat[PanelCur].cur:=1;
    goto f1;
    end;
  $820b:begin;{ctrl+pgdn}
    if PanelCurrEntry(d) then exit;
    if (d.flg and xRights_Directory<>0) then goto f1;
    end;
  else begin;
    if (w and $fe00<>0) then exit;
    PromptString:=PromptString+chr(w);
    RefreshScr:=RefreshScr or 1;
    end;
  end;
End;
