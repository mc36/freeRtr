{$heap 63k}
{$stack 3k}
{$sysinc system.inc}
{$sysinc alap.inc}
{$sysinc param.inc}
{$sysinc filesys.inc}
{$sysinc filesys2.inc}
{$sysinc textfile.inc}
{$sysinc bugos.inc}
{$include codeReader.inc}
Var
  DeveloperPath:String;
  targetPlatform:String;
  targetPrefix:String;


Procedure ImmErr(a:String);
Begin;
WriteLn(a);
Halt(2);
End;

Function getWord(var a:String):String;
Var i:Word;
Begin;
while (copy(a,1,1)=' ') do a:=copy(a,2,666);
i:=pos(' ',a);
if (i<1) then i:=666;
getWord:=copy(a,1,i-1);
a:=copy(a,i,666);
while (copy(a,1,1)=' ') do a:=copy(a,2,666);
End;

Procedure unescapeString(var a:String);
Begin;
if (copy(a,1,1)='^') then a:=copy(a,2,666);
End;

Function doExec(fn,par,err:String):Boolean;
Var w1,w2:Word;
Begin;
doExec:=True;
w1:=xExec(fn,par,w2);
fn:=BStr(w1);
if (pos('|'+fn+'|',err)<>0) then exit;
if (w1<>0) then immErr('error: '+xGetErrorName(w1));
if (w2>0) then immErr('bad return code: '+BStr(w2));
WriteLn('');
doExec:=False;
End;

Function xCopy(n1,n2:String;append:Boolean):LongInt;
Label vege;
Var
  f1,f2:xFile;
  buf:array[1..8*1024] of byte;
  p,s,o,i:LongInt;
Begin;
WriteLn('copy '+n1+'-->'+n2+'...');
fillchar(f1,sizeof(f1),0);
fillchar(f2,sizeof(f2),0);
i:=xOpen(f1,n1,xGenFilMod_r);
if (i<>0) then goto vege;
i:=xCreate(n2);
if append then i:=0;
if (i<>0) then goto vege;
i:=xOpen(f2,n2,xGenFilMod_rw);
if (i<>0) then goto vege;
xSeek(f2,xFileSize(f2));
s:=xFileSize(f1);
p:=0;
while (p<s) do begin;
  o:=s-p;
  if (o>sizeof(buf)) then o:=sizeof(buf);
  i:=xBlockRead(f1,buf,o);
  if (i<>0) then goto vege;
  i:=xBlockWrite(f2,buf,o);
  if (i<>0) then goto vege;
  inc(p,o);
  end;
vege:
xClose(f1);
o:=xClose(f2);
if (i=0) then i:=o;
xCopy:=i;
End;

Function xJustRights(f:String;r:LongInt):LongInt;
Var i,o:LongInt;
Begin;
BugOS_MyOwnerInfo(o,i);
xJustRights:=xSetRight(f,o,r);
End;


Procedure ProcessCommand(c:String);
Label f1,f2;
Const
  rightsToGive=xRights_OwnRead+xRights_OwnExec;
Var
  skipErrors:Boolean;
  a:String;
  i,o:LongInt;
Begin;
skipErrors:=False;
f1:
a:=kicsi(getWord(c));
if (a='') then exit;
if (copy(a,1,1)=';') then exit;
WriteLn('');
Write('make: '+a+' ');WriteLn(c);
if (a='make') then begin;
  doExec(paramStr(0),'^'+c+' ^'+targetPlatform+' ^'+targetPrefix,'');
  exit;
  end;
if (a='compasmn') then begin;
  if (kicsi(targetPlatform)='vm') then immErr('not supported!');
  c:=xFileName(c,1)+xFileName(c,2);
  xErase(c+'.code');
  doExec(DeveloperPath+'sasm\sasm-'+targetPlatform+'.code',c+'.asm','');
  doExec(DeveloperPath+'sasm\link4sasm.code',c+'.obj /EXTcode /DOB','');
  xJustRights(c+'.code',rightsToGive);
  Exit;
  end;
if (a='compasm') then begin;
  if (kicsi(targetPlatform)='vm') then immErr('not supported!');
  c:=xFileName(c,1)+xFileName(c,2);
  xErase(c+'.code');
  doExec(DeveloperPath+'sasm\sasm-'+targetPlatform+'.code',c+'.asm','');
  doExec(DeveloperPath+'sasm\link4sasm.code',c+'.obj /EXTcode /DOB','');
  doExec(DeveloperPath+'utils\appendCrc32.code',c+'.code','');
  xJustRights(c+'.code',rightsToGive);
  Exit;
  end;
if (a='comppas') then begin;
  c:=xFileName(c,1)+xFileName(c,2);
  xErase(c+'.code');
  doExec(DeveloperPath+'spas\spas.code',c+'.pas '+targetPlatform,'');
  if (kicsi(targetPlatform)='vm') then begin;
    doExec(DeveloperPath+'vm-compiler\vm-compiler.code',c+'.vma '+c+'.obj','');
    xErase(c+'.vma');
    doExec(DeveloperPath+'sasm\link4sasm.code',c+'.obj /EXTcode /DOB','');
    doExec(DeveloperPath+'utils\appendCrc32.code',c+'.code sizelsb','');
    end else begin;
    doExec(DeveloperPath+'vm-compiler\bugos-'+targetPlatform+'.code',c+'.vma '+c+'.asm','');
    xErase(c+'.vma');
    if not doExec(DeveloperPath+'utils\asmopt-'+targetPlatform+'.code',c+'.asm','|7|') then begin;
      xErase(c+'.asm');
      xRename(c+'.opt',c+'.asm');
      xErase(c+'.opt');
      end;
    doExec(DeveloperPath+'sasm\sasm-'+targetPlatform+'.code',c+'.asm','');
    xErase(c+'.asm');
    doExec(DeveloperPath+'sasm\link4sasm.code',c+'.obj /EXTcode /DOB','');
    doExec(DeveloperPath+'utils\appendCrc32.code',c+'.code','');
    end;
  xJustRights(c+'.code',rightsToGive);
  Exit;
  end;
if (a='exec') then begin;
  a:=getWord(c);
  doExec(a,c,'');
  Exit;
  end;
if (a='noerr') then begin;
  skipErrors:=True;
  goto f1;
  end;
if (a='del') then begin;
  i:=xErase(c);
  goto f2;
  end;
if (a='ren') then begin;
  a:=getWord(c);
  i:=xRename(a,targetPrefix+c);
  goto f2;
  end;
if (a='renow') then begin;
  a:=getWord(c);
  xErase(targetPrefix+c);
  i:=xRename(a,targetPrefix+c);
  goto f2;
  end;
if (a='renap') then begin;
  a:=getWord(c);
  i:=xCopy(a,targetPrefix+c,true);
  if (i<>0) then goto f2;
  xErase(a);
  goto f2;
  end;
if (a='copy') then begin;
  a:=getWord(c);
  i:=xCopy(a,targetPrefix+c,false);
  goto f2;
  end;
if (a='copyow') then begin;
  a:=getWord(c);
  xErase(targetPrefix+c);
  i:=xCopy(a,targetPrefix+c,false);
  goto f2;
  end;
if (a='copyap') then begin;
  a:=getWord(c);
  i:=xCopy(a,targetPrefix+c,true);
  goto f2;
  end;
if (a='chmode') then begin;
  a:=getWord(c);
  i:=xJustRights(a,BVal(c));
  goto f2;
  end;
if (a='ifplat') then begin;
  a:=getWord(c);
  if (kicsi(targetPlatform)<>kicsi(a)) then exit;
  goto f1;
  end;
if (a='ifnoplat') then begin;
  a:=getWord(c);
  if (kicsi(targetPlatform)=kicsi(a)) then exit;
  goto f1;
  end;
if (a='exit') then begin;
  WriteLn('');WriteLn('');WriteLn('');
  WriteLn('make: exited by command!');
  Halt(0);
  exit;
  end;
if (a='echo') then begin;
  WriteLn(c);
  exit;
  end;


immErr('invalid command!');
f2:
if skipErrors then exit;
if (i=0) then exit;
immErr('error: '+xGetErrorName(i));
End;

Var
  a:String;
  t:xtText;
BEGIN;
WriteLn('maker v1.0, done by Mc at '#%date' '#%time'.');
DeveloperPath:=xFileName(paramStr(0),1)+'..\';
targetPlatform:=paramStr(2);
a:=getCompilerPlatform;
if (targetPlatform='') then targetPlatform:=a;
unescapeString(targetPlatform);
targetPrefix:=paramStr(3);
if (targetPrefix='') then
 if (kicsi(targetPlatform)<>kicsi(a)) then targetPrefix:='\'+targetPlatform;
unescapeString(targetPrefix);
a:=paramStr(1);
if (a='') then a:='makefile';
unescapeString(a);
WriteLn('run='+getCompilerPlatform+'  target='+targetPlatform+'  path='+targetPrefix+'  tools='+DeveloperPath);
WriteLn('processing '+a+'...');
if (xtOpen(t,a,true)<>0) then immErr('error opening file');
a:=xFileName(a,1);
if (a<>'') then xChDir(a);
while not xtEOF(t) do begin;
  a:=xtReadLn(t,255);
  ProcessCommand(a);
  end;
xtClose(t);
WriteLn('');WriteLn('');WriteLn('');
WriteLn('make: Successfully finished!');
Halt(0);
END.