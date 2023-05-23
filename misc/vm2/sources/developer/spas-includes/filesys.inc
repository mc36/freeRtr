Type
  xDirEntryDateTimeRec=record
    year:Word;
    month:Byte;
    day:Byte;
    hour:Byte;
    minute:Byte;
    second:Byte;
    end;
  xDirEntryRec=record
    size:LongInt;
    rights:LongInt;
    owner:LongInt;
    created:xDirEntryDateTimeRec;
    modified:xDirEntryDateTimeRec;
    name:String;
    end;
  xFile=LongInt;
Const
  xFileMode_read=1;
  xFileMode_write=2;
  xFileMode_allowRead=$100;
  xFileMode_allowWrite=$200;
  xFileMode_allowExec=$400;
  xGenFilMod_r=xFileMode_read+xFileMode_allowRead+xFileMode_allowExec;
  xGenFilMod_rw=xFileMode_read+xFileMode_write;
Const
  xRights_OwnRead=$01;
  xRights_OwnWrite=$02;
  xRights_OwnExec=$04;
  xRights_AnyRead=$08;
  xRights_AnyWrite=$10;
  xRights_AnyExec=$20;
  xRights_HasRootPriv=$40;
  xRights_Directory=$80;


Function xOpen(var hnd:xFile;nam:String;mod:LongInt):Word;
Begin;
asm VarAddr src nam;
asm VarAddr trg mod;
asm movR dudud a [trg];
asm sysCall file.Open;
asm VarAddr trg hnd;
asm AddrLod trg [trg];
asm movW dudud [trg] a;
asm VarAddr trg @result;
asm movW duwuw [trg] b;
End;

Function xBlockRead(hnd:xFile;var buf;siz:LongInt):Word;
Begin;
asm VarAddr src hnd;
asm movR dudud a [src];
asm VarAddr src siz;
asm movR dudud c [src];
asm VarAddr trg buf;
asm AddrLod trg [trg];
asm sysCall file.Read;
asm VarAddr trg @result;
asm movW duwuw [trg] b;
End;

Function xBlockWrite(hnd:xFile;var buf;siz:LongInt):Word;
Begin;
asm VarAddr src hnd;
asm movR dudud a [src];
asm VarAddr src siz;
asm movR dudud c [src];
asm VarAddr src buf;
asm AddrLod src [src];
asm sysCall file.Write;
asm VarAddr trg @result;
asm movW duwuw [trg] b;
End;

Function xSeek(hnd:xFile;ps:LongInt):Word;
Begin;
asm VarAddr src hnd;
asm movR dudud a [src];
asm VarAddr src ps;
asm movR dudud c [src];
asm sysCall file.Seek;
asm VarAddr trg @result;
asm movW duwuw [trg] b;
End;

Function xFileSize(hnd:xFile):LongInt;
Begin;
asm VarAddr src hnd;
asm movR dudud a [src];
asm sysCall file.GetSize;
asm comp uwuw b 0;
asm jmpc e @j1;
asm move sdsd c -1;
asm label @j1;
asm VarAddr trg @result;
asm movW dudud [trg] c;
End;

Function xFilePos(hnd:xFile):LongInt;
Begin;
asm VarAddr src hnd;
asm movR dudud a [src];
asm sysCall file.GetPos;
asm comp uwuw b 0;
asm jmpc e @j1;
asm move sdsd c -1;
asm label @j1;
asm VarAddr trg @result;
asm movW dudud [trg] c;
End;

Function xTruncate(hnd:xFile):Word;
Begin;
asm VarAddr src hnd;
asm movR dudud a [src];
asm sysCall file.Truncate;
asm VarAddr trg @result;
asm movW duwuw [trg] b;
End;

Function xClose(hnd:xFile):Word;
Begin;
asm VarAddr src hnd;
asm movR dudud a [src];
asm sysCall file.Close;
asm VarAddr trg @result;
asm movW duwuw [trg] b;
End;

Function xCreate(fn:String):Word;
Begin;
asm VarAddr src fn;
asm sysCall file.Create;
asm VarAddr trg @result;
asm movW duwuw [trg] b;
End;

Function xErase(fn:String):Word;
Begin;
asm VarAddr src fn;
asm sysCall file.Erase;
asm VarAddr trg @result;
asm movW duwuw [trg] b;
End;

Function xGetDir:String;
Begin;
asm VarAddr trg @result;
asm sysCall dir.Current;
End;

Function xChDir(fn:String):Word;
Begin;
asm VarAddr src fn;
asm sysCall dir.Change;
asm VarAddr trg @result;
asm movW duwuw [trg] b;
End;

Function xSetRight(fn:String;owner,right:LongInt):Word;
Begin;
asm VarAddr src fn;
asm VarAddr trg owner;
asm movR dudud b [trg];
asm VarAddr trg right;
asm movR dudud a [trg];
asm sysCall dir.setRights;
asm VarAddr trg @result;
asm movW duwuw [trg] b;
End;

Function xSetDate(fn:String;create,modify:xDirEntryDateTimeRec):Word;
Var
  allDates:record
    create:xDirEntryDateTimeRec;
    modify:xDirEntryDateTimeRec;
    end;
Begin;
move(create,allDates.create,sizeof(create));
move(modify,allDates.modify,sizeof(modify));
asm VarAddr src fn;
asm VarAddr trg allDates;
asm sysCall dir.setDate;
asm VarAddr trg @result;
asm movW duwuw [trg] b;
End;

Function xRename(fn1,fn2:String):Word;
Begin;
asm VarAddr src fn1;
asm VarAddr trg fn2;
asm sysCall dir.ReName;
asm VarAddr trg @result;
asm movW duwuw [trg] b;
End;

Function xMkLink(fn1,fn2:String):Word;
Begin;
asm VarAddr src fn1;
asm VarAddr trg fn2;
asm sysCall dir.makeLink;
asm VarAddr trg @result;
asm movW duwuw [trg] b;
End;

Function xMkDir(fn:String):Word;
Begin;
asm VarAddr src fn;
asm sysCall dir.Create;
asm VarAddr trg @result;
asm movW duwuw [trg] b;
End;

Function xRmDir(fn:String):Word;
Begin;
asm VarAddr src fn;
asm sysCall dir.Erase;
asm VarAddr trg @result;
asm movW duwuw [trg] b;
End;

Function xDirOpen(var hnd:xFile;nam:String):Word;
Begin;
asm VarAddr src nam;
asm sysCall dir.Open;
asm VarAddr trg hnd;
asm AddrLod trg [trg];
asm movW dudud [trg] a;
asm VarAddr trg @result;
asm movW duwuw [trg] b;
End;

Function xDirRead(hnd:xFile;var buf:xDirEntryRec):Word;
Begin;
asm VarAddr src hnd;
asm movR dudud a [src];
asm VarAddr trg buf;
asm AddrLod trg [trg];
asm sysCall dir.Read;
asm VarAddr trg @result;
asm movW duwuw [trg] b;
End;

Function xDirClose(hnd:xFile):Word;
Begin;
asm VarAddr src hnd;
asm movR dudud a [src];
asm sysCall dir.Close;
asm VarAddr trg @result;
asm movW duwuw [trg] b;
End;

Function xDiskInfo(var free,used,bad,blocksize:LongInt):Word;
Begin;
asm sysCall dir.statistic;
asm VarAddr trg free;
asm AddrLod trg [trg];
asm movW dudud [trg] a;
asm VarAddr trg used;
asm AddrLod trg [trg];
asm movW dudud [trg] b;
asm VarAddr trg bad;
asm AddrLod trg [trg];
asm movW dudud [trg] c;
asm VarAddr trg blocksize;
asm AddrLod trg [trg];
asm movW dudud [trg] d;
asm VarAddr trg @result;
asm sub w a a;
asm movW duwuw [trg] a;
End;

Function xExec(fn,par:String;var ret:Word):Word;
Begin;
asm varAddr src fn;
asm varAddr trg par;
asm sysCall console.execWait;
asm VarAddr trg @result;
asm movW duwuw [trg] b;
asm VarAddr trg ret;
asm addrLod trg [trg];
asm movW duwuw [trg] a;
End;

Function xExecNew(fn,par:String):Word;
Begin;
asm varAddr src fn;
asm varAddr trg par;
asm sysCall console.execNew;
asm VarAddr trg @result;
asm movW duwuw [trg] b;
End;

Function xExecBgnd(fn,par:String;var pid:LongInt):Word;
Begin;
asm varAddr src fn;
asm varAddr trg par;
asm sysCall console.execBckgnd;
asm VarAddr trg pid;
asm addrLod trg [trg];
asm movW dudud [trg] a;
asm VarAddr trg @result;
asm movW duwuw [trg] b;
End;

Function xExecInside(fn,par:String;var pid,pipe:LongInt):Word;
Begin;
asm varAddr src fn;
asm varAddr trg par;
asm sysCall console.execInme;
asm VarAddr src pipe;
asm addrLod src [src];
asm movW dudud [src] c;
asm VarAddr trg pid;
asm addrLod trg [trg];
asm movW dudud [trg] a;
asm VarAddr trg @result;
asm movW duwuw [trg] b;
End;
