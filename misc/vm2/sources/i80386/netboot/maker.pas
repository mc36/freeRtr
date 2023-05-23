{$sysinc system.inc}
{$sysinc filesys.inc}
{$sysinc textfile.inc}
{$sysinc param.inc}
Const asmer='\utils\developer\sasm\sasm-i80386.code';

Procedure immErr(a:String);
Begin;
WriteLn(a);
Halt(1);
End;

Var
  a,b:String;
  t:xtText;
BEGIN;
b:=GetAllParameters;
if (b='') then immErr('using: maker <drivername>');
a:=b+'.make';
WriteLn('generating '+a+'...');
xErase(a);
xCreate(a);
if (xtOpen(t,a,false)<>0) then immErr('error opening file!');
a:=b+'.obj';
xtWriteLn(t,'noerr del '+b+'.code');
xtWriteLn(t,'noerr del '+b+'.cod');
xtWriteLn(t,'noerr del '+b+'.rom');
xtWriteLn(t,'exec '+asmer+' ..\driver-netcards\'+b+'.asm');
xtWriteLn(t,'exec '+asmer+' bootrom.asm');
xtWriteLn(t,'renOW bootrom.obj '+a);
xtWriteLn(t,'copyAP ..\driver-netcards\'+a+' '+a);
xtWriteLn(t,'del ..\driver-netcards\'+a);
xtWriteLn(t,'exec \utils\developer\sasm\link4sasm.code '+a+' /dob /extcode');
xtWriteLn(t,'exec \utils\compressor.code c1 '+b+'.code '+b+'.cod');
xtWriteLn(t,'copyOW \system\kernel\uncompress.code '+b+'.code');
xtWriteLn(t,'copyAP '+b+'.cod '+b+'.code');
xtWriteLn(t,'exec \utils\developer\utils\makerom.code '+b+'.code '+b+'.rom');
xtWriteLn(t,'del '+b+'.code');
xtWriteLn(t,'del '+b+'.cod');
xtClose(t);
WriteLn('successful!');
END.