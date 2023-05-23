Type
  OneMemoryType=array[1..1] of byte;
  OneRecordHeadType=record
    siz:LongInt;
    typ:LongInt;
    nam:string[63];
    end;
const
  strTyp_var=$01;
  strTyp_prc=$02;
  strTyp_par=$04;
  strTyp_glb=$08;
  strTyp_tmp=$80;
  strTyp_all=$ff;
  strTyp_non=$100;
Var
  currMemDat:^OneMemoryType;
  currMemSiz:LongInt;
  currMemTopSize:LongInt;
  currProcOfs:LongInt;


Procedure memResize(i:LongInt);
Var p:pointer;
Begin;
currMemSiz:=i;
if (i<currMemTopSize) then exit;
inc(i,65536);
if (ExtendedMemoryResize(p,i)<currMemSiz) then begin;
  WriteLn('error allocating memory!');
  Halt(9);
  end;
currMemTopSize:=i;
currMemDat:=p^;
End;




Function memoryWrite(var str:oneStringType;ps,typ:LongInt;nam:String):LongInt;
Var
  h:OneRecordHeadType;
  o:LongInt;
Begin;
inc(ps);
o:=stringLength(str);
h.siz:=o;
h.typ:=typ;
h.nam:=nam;
move(h,currMemDat^[ps],sizeof(h));
move(str,currMemDat^[ps+sizeof(h)],o);
move(h,currMemDat^[ps+o+sizeof(h)],sizeof(o));
memoryWrite:=o+sizeof(h)+sizeof(o);
End;



Function memoryRead(var str:oneStringType;ps:LongInt;Var typ:LongInt;var nam:String):LongInt;
Var
  o:LongInt;
  h:OneRecordHeadType;
Begin;
inc(ps);
move(currMemDat^[ps],h,sizeof(h));
o:=h.siz;
move(currMemDat^[ps+sizeof(h)],str,o);
str[o+1]:=0;
typ:=h.typ;
nam:=h.nam;
memoryRead:=o+sizeof(h)+sizeof(o);
End;




Procedure memoryPush(var str:oneStringType;typ:LongInt;nam:String);
Var o,p:LongInt;
Begin;
p:=currMemSiz;
memResize(oneStringMaxSize+128+p);
o:=memoryWrite(str,p,typ,nam);
currMemSiz:=o+p;
End;



Procedure memoryPop(var str:oneStringType;Var typ:LongInt;var nam:String);
Var o,p:LongInt;
Begin;
dec(currMemSiz,sizeof(o));
move(currMemDat^[currMemSiz+1],o,sizeof(o));
dec(currMemSiz,o);
dec(currMemSiz,sizeof(OneRecordHeadType));
memoryRead(str,currMemSiz,typ,nam);
End;



Function memoryFindForeward(nam:String;typ,bg,ed:LongInt):LongInt;
Label f1;
Var
  h:OneRecordHeadType;
  i,o,p:LongInt;
Begin;
memoryFindForeward:=-1;
p:=bg;
o:=0;
f1:
inc(p,o);
if (p>=ed) then exit;
move(currMemDat^[p+1],h,sizeof(h));
o:=sizeof(h)+sizeof(o)+h.siz;
if (h.typ and typ=0) then goto f1;
if (typ and strTyp_non=0) then if (h.nam<>nam) then goto f1;
memoryFindForeward:=p;
End;



Function memoryFindBackward(nam:String;typ,bg,ed:LongInt):LongInt;
Label f1;
Var
  h:OneRecordHeadType;
  i,o,p:LongInt;
Begin;
memoryFindBackward:=-1;
p:=ed;
if (bg<1) then bg:=1;
f1:
if (p<bg) then exit;
dec(p,sizeof(o));
move(currMemDat^[p+1],o,sizeof(o));
dec(p,o);
dec(p,sizeof(h));
move(currMemDat^[p+1],h,sizeof(h));
if (h.typ and typ=0) then goto f1;
if (typ and strTyp_non=0) then if (h.nam<>nam) then goto f1;
memoryFindBackward:=p;
End;



Function memoryFindCurr(nam:String;typ:LongInt):LongInt;
Begin;
memoryFindCurr:=memoryFindBackward(nam,typ,currProcOfs,currMemSiz);
End;
