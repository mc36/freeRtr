{$heap 63k}
{$stack 3k}
{$sysinc system.inc}
{$sysinc alap.inc}
{$sysinc param.inc}
{$sysinc filesys.inc}
{$sysinc filesys2.inc}
{$sysinc textfile.inc}
Const
  develPath='\utils\developer\';
  outFile='output';

Procedure ImmErr(a:String);
Begin;
WriteLn(a);
Halt(2);
End;

Procedure doAsm(a:String);
Var w1,w2:Word;
Begin;
w1:=xExec(develPath+'sasm\sasm-i80386.code',a+'.asm use32',w2);
if (w1=0) and (w2=0) then exit;
immErr('error assembling '+a+'!');
End;

Procedure doLink(a:String);
Var w1,w2:Word;
Begin;
w1:=xExec(develPath+'sasm\link4sasm.code',a+'.obj /dob /extCode',w2);
if (w1=0) and (w2=0) then exit;
immErr('error linking '+a+'!');
End;

Procedure doSum(a:String);
Var w1,w2:Word;
Begin;
a:=a+'.code';
w1:=xExec(develPath+'utils\appendcrc32.code',a,w2);
xSetRight(a,0,$44);
if (w1=0) and (w2=0) then exit;
immErr('error summing '+a+'!');
End;



Var
  t1,t2,t3:xtText;
  fn:String;
  a:String;
BEGIN;
WriteLn('etherComp v1.0, done by Mc at '#%date' '#%time'.');
a:=ParamStr(1);
fn:=xFileName(a,2);
if (fn='') then immErr('using: etherComp.code <nicFile>');
if (xtOpen(t1,'ethermain.obj',true)<>0) then begin;
  doAsm('etherMain');
  if (xtOpen(t1,'ethermain.obj',true)<>0) then immErr('error opening object file!');
  end;
doAsm(fn);
if (xtOpen(t2,fn+'.obj',true)<>0) then immErr('error opening object file!');
xErase(outFile+'.obj');
xCreate(outFile+'.obj');
if (xtOpen(t3,outFile+'.obj',false)<>0) then immErr('error creating object file!');
while not xtEOF(t1) do begin;
  a:=xtReadLn(t1,255);
  if (copy(a,pos(';',a),255)<>';;|;;') then begin;
    xtWriteLn(t3,a);
    continue;
    end;
  xtSetPos(t2,0);
  while not xtEOF(t2) do begin;
    a:=xtReadLn(t2,255);
    xtWriteLn(t3,a);
    end;
  end;
xtClose(t3);
xtClose(t2);
xtClose(t1);
xErase(fn+'.obj');
doLink(outFile);
doSum(outFile);
xErase(fn+'.code');
xRename(outFile+'.code',fn+'.code');
END.