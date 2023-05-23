{$heap 63k}
{$stack 3k}
{$sysinc system.inc}
{$sysinc alap.inc}
{$sysinc hex.inc}
{$sysinc filesys.inc}
{$sysinc pipeline.inc}
{$sysinc bugos.inc}
{$sysinc filesys2.inc}
{$sysinc textfile.inc}
{$sysinc datetime.inc}
{$sysinc random.inc}
{$sysinc param.inc}
{$sysinc crt.inc}

Const
  PrgTxt='browser v1.0';

{$include output.inc}
{$include screen.inc}
{$include tagger.inc}
{$include html.inc}
{$include ftp.inc}
{$include gopher.inc}
{$include local.inc}
{$include proto.inc}
{$include userif.inc}

Label f1;
Var
  b:Boolean;
  a:String;
BEGIN;
WriteLn(PrgTxt+', done by Mc at '#%date' '#%time'.');
Randomize;
dataPath:=paramStr(1);
if (dataPath='') then dataPath:='.';
dataPath:=repairPath(dataPath);
WriteLn('working path: '+dataPath);

currentUrl:='about://about';
currentUsr:='';
currentPwd:='';
currentPrx:='';
currentFnd:='';
append2bookmark(HistoryFileName,#13,'');
append2bookmark(HistoryFileName,getCurrentDate+' '+getCurrentTime,'');
ConsoleSize(screenSizX,screenSizY);
currCharSet:=charSetAnsiDos;

f1:
a:=dataPath+TemporaryHtml;
xErase(a);
doDownloadOneFile(a);
xCreate(a);
outputBegin;
WriteLn('decoding content...');
if (downloadedFile<>'') then inputOpen(downloadedFile);
case decodingMethod of
  1:decodeCurrentHtml(sourceFile);
  2:decodeCurrentFtp(sourceFile);
  3:decodeCurrentTextfile(sourceFile);
  4:decodeCurrentBinary(sourceFile);
  5:decodeCurrentDirectory(copy(currentUrl,8,666));
  6:decodeCurrentHistory(sourceFile,false,'bookmarked sites');
  7:decodeCurrentHistory(sourceFile,true,'visited pages');
  8:decodeCurrentAbout;
  9:decodeCurrentGopher(sourceFile);
  else decodeCurrentUnknown;
  end;
if (downloadedFile<>'') then inputClose;
append2bookmark(HistoryFileName,currentTit,currentUrl);
b:=doUserInterface;
outputClose;
if b then goto f1;
TextColor(7);
WriteLn('');
outputErase;
END.