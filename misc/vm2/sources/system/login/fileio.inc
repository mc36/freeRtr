Var
  loginFileHdr:xFile;
  loginFileMax:LongInt;

Procedure readEntry(n:LongInt;var d:OneLoginUserDataRecord);
Begin;
if (n<1) or (n>loginFileMax) then begin;
  fillchar(d,sizeof(d),0);
  exit;
  end;
if (xSeek(loginFileHdr,(n-1)*sizeof(d))<>0) then immErr('error seeking in file!');
if (xBlockRead(loginFileHdr,d,sizeof(d))<>0) then immErr('error reading file!');
End;

Procedure writeEntry(n:LongInt;var d:OneLoginUserDataRecord);
Begin;
if (n=loginFileMax+1) then inc(loginFileMax);
if (n<1) or (n>loginFileMax) then exit;
if (xSeek(loginFileHdr,(n-1)*sizeof(d))<>0) then immErr('error seeking in file!');
if (xBlockWrite(loginFileHdr,d,sizeof(d))<>0) then immErr('error writing file!');
End;

Procedure deleteEntry(n:LongInt);
Var d:OneLoginUserDataRecord;
Begin;
if (n<1) or (n>loginFileMax) then exit;
if (xSeek(loginFileHdr,(loginFileMax-1)*sizeof(d))<>0) then immErr('error seeking in file!');
if (xBlockRead(loginFileHdr,d,sizeof(d))<>0) then immErr('error reading file!');
if (xSeek(loginFileHdr,(n-1)*sizeof(d))<>0) then immErr('error seeking in file!');
if (xBlockWrite(loginFileHdr,d,sizeof(d))<>0) then immErr('error writing file!');
dec(loginFileMax);
if (xSeek(loginFileHdr,loginFileMax*sizeof(d))<>0) then immErr('error seeking in file!');
if (xTruncate(loginFileHdr)<>0) then immErr('error truncating file!');
End;

Procedure entryOpen(a:String);
Begin;
WriteLn('opening '+a+'...');
xCreate(a);
xSetRight(a,0,xRights_OwnRead+xRights_OwnWrite);
if (xOpen(loginFileHdr,a,xGenFilMod_rw)<>0) then immErr('error opening user database!');
loginFileMax:=xFileSize(loginFileHdr) div sizeof(OneLoginUserDataRecord);
End;

Procedure entryClose;
Begin;
if (xClose(loginFileHdr)<>0) then immErr('error closing file!');
End;
