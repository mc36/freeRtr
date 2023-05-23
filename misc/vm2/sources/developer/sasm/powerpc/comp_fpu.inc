Procedure CompileOneFpuLine;
Var
  Cmd:String;
  p1:OneParameterRec;
  p2:OneParameterRec;
  p3:OneParameterRec;
  p4:OneParameterRec;
  p5:OneParameterRec;

Procedure getParam(var p:OneParameterRec;var b:String);
Var
  a:String;
  i:Longint;
Begin;
b:=xLevesz(b);
i:=Pos(',',b+',');
a:=Copy(b,1,i-1);
b:=Copy(b,i+1,255);
AnalizeParameter(a,p);
End;



Const
  Str_mcrfs='|mcrfs|';
Procedure Inst_mcrfs;
Var i,o,p:LongInt;
Begin;
if (OutOk=True) then Exit;
if (p1.Typ<>5) then Exit;
if (p2.Typ<>5) then Exit;
if (p3.Typ<>0) then Exit;
i:=0;
if (Cmd='mcrfs') then i:=$fc000080;
if (i=0) then exit;
inc(i,p1.Cnd shl 23);
inc(i,p2.Cnd shl 18);
OutStr:=b2h(i shr 24)+SepCh+b2h(i shr 16)+SepCh+b2h(i shr 8)+SepCh+b2h(i);
OutOk:=True;
End;


Const
  Str_mtfsb0='|mtfsb0|mtfsb0.|mtfsb1|mtfsb1.|';
Procedure Inst_mtfsb0;
Var i,o,p:LongInt;
Begin;
if (OutOk=True) then Exit;
if (p1.Typ<>2) then Exit;
if (p2.Typ<>0) then Exit;
i:=0;
if (Cmd='mtfsb0') then i:=$fc00008c;
if (Cmd='mtfsb0.') then i:=$fc00008d;
if (Cmd='mtfsb1') then i:=$fc00004c;
if (Cmd='mtfsb1.') then i:=$fc00004d;
if (i=0) then exit;
o:=BVal('$'+p1.Num);
if (o<0) or (o>31) then exit;
inc(i,o shl 21);
OutStr:=b2h(i shr 24)+SepCh+b2h(i shr 16)+SepCh+b2h(i shr 8)+SepCh+b2h(i);
OutOk:=True;
End;


Const
  Str_mtfsfi='|mtfsfi|mtfsfi.|';
Procedure Inst_mtfsfi;
Var i,o,p:LongInt;
Begin;
if (OutOk=True) then Exit;
if (p1.Typ<>2) then Exit;
if (p2.Typ<>2) then Exit;
if (p3.Typ<>0) then Exit;
i:=0;
if (Cmd='mtfsfi') then i:=$fc00010c;
if (Cmd='mtfsfi.') then i:=$fc00010d;
if (i=0) then exit;
o:=BVal('$'+p1.Num);
if (o<0) or (o>15) then exit;
inc(i,o shl 23);
o:=BVal('$'+p2.Num);
if (o<0) or (o>15) then exit;
inc(i,o shl 12);
OutStr:=b2h(i shr 24)+SepCh+b2h(i shr 16)+SepCh+b2h(i shr 8)+SepCh+b2h(i);
OutOk:=True;
End;


Const
  Str_fabs='|fabs|fabs.|fctiw|fctiw.|fctiwz|fctiwz.|fmr|fmr.|'+
           'fnabs|fnabs.|fneg|fneg.|frsp|frsp.|fres|fres.|'+
           'frsqrte|frsqrte.|fsqrt|fsqrt.|fsqrts|fsqrts.|';
Procedure Inst_fabs;
Var i,o,p:LongInt;
Begin;
if (OutOk=True) then Exit;
if (p1.Typ<>6) then Exit;
if (p2.Typ<>6) then Exit;
if (p3.Typ<>0) then Exit;
i:=0;
if (Cmd='fabs') then i:=$fc000210;
if (Cmd='fabs.') then i:=$fc000211;
if (Cmd='fctiw') then i:=$fc00001c;
if (Cmd='fctiw.') then i:=$fc00001d;
if (Cmd='fctiwz') then i:=$fc00001e;
if (Cmd='fctiwz.') then i:=$fc00001f;
if (Cmd='fmr') then i:=$fc000090;
if (Cmd='fmr.') then i:=$fc000091;
if (Cmd='fnabs') then i:=$fc000110;
if (Cmd='fnabs.') then i:=$fc000111;
if (Cmd='fneg') then i:=$fc000050;
if (Cmd='fneg.') then i:=$fc000051;
if (Cmd='frsp') then i:=$fc000018;
if (Cmd='frsp.') then i:=$fc000019;
if (Cmd='fres') then i:=$ec000030;
if (Cmd='fres.') then i:=$ec000031;
if (Cmd='frsqrte') then i:=$ec000034;
if (Cmd='frsqrte.') then i:=$ec000035;
if (Cmd='fsqrt') then i:=$fc00002c;
if (Cmd='fsqrt.') then i:=$fc00002d;
if (Cmd='fsqrts') then i:=$ec00002c;
if (Cmd='fsqrts.') then i:=$ec00002d;
if (i=0) then exit;
inc(i,p1.Frg shl 21);
inc(i,p2.Frg shl 11);
OutStr:=b2h(i shr 24)+SepCh+b2h(i shr 16)+SepCh+b2h(i shr 8)+SepCh+b2h(i);
OutOk:=True;
End;


Const
  Str_fadd='|fadd|fadd.|fadds|fadds.|fdiv|fdiv.|fdivs|fdivs.|'+
           'fsub|fsub.|fsubs|fsubs.|';
Procedure Inst_fadd;
Var i,o,p:LongInt;
Begin;
if (OutOk=True) then Exit;
if (p1.Typ<>6) then Exit;
if (p2.Typ<>6) then Exit;
if (p3.Typ<>6) then Exit;
if (p4.Typ<>0) then Exit;
i:=0;
if (Cmd='fadd') then i:=$fc00002a;
if (Cmd='fadd.') then i:=$fc00002b;
if (Cmd='fadds') then i:=$ec00002a;
if (Cmd='fadds.') then i:=$ec00002b;
if (Cmd='fdiv') then i:=$fc000024;
if (Cmd='fdiv.') then i:=$fc000025;
if (Cmd='fdivs') then i:=$ec000024;
if (Cmd='fdivs.') then i:=$ec000025;
if (Cmd='fsub') then i:=$fc000028;
if (Cmd='fsub.') then i:=$fc000029;
if (Cmd='fsubs') then i:=$ec000028;
if (Cmd='fsubs.') then i:=$ec000029;
if (i=0) then exit;
inc(i,p1.Frg shl 21);
inc(i,p2.Frg shl 16);
inc(i,p3.Frg shl 11);
OutStr:=b2h(i shr 24)+SepCh+b2h(i shr 16)+SepCh+b2h(i shr 8)+SepCh+b2h(i);
OutOk:=True;
End;


Const
  Str_fcmpo='|fcmpo|fcmpu|';
Procedure Inst_fcmpo;
Var i,o,p:LongInt;
Begin;
if (OutOk=True) then Exit;
if (p1.Typ<>5) then Exit;
if (p2.Typ<>6) then Exit;
if (p3.Typ<>6) then Exit;
if (p4.Typ<>0) then Exit;
i:=0;
if (Cmd='fcmpo') then i:=$fc000040;
if (Cmd='fcmpu') then i:=$fc000000;
if (i=0) then exit;
inc(i,p1.Cnd shl 23);
inc(i,p2.Frg shl 16);
inc(i,p3.Frg shl 11);
OutStr:=b2h(i shr 24)+SepCh+b2h(i shr 16)+SepCh+b2h(i shr 8)+SepCh+b2h(i);
OutOk:=True;
End;


Const
  Str_fmadd='|fmadd|fmadd.|fmadds|fmadds.|fmsub|fmsub.|fmsubs|fmsubs.|'+
            'fnmadd|fnmadd.|fnmadds|fnmadds.|fnmsub|fnmsub.|fnmsubs|fnmsubs.|'+
            'fsel|fsel.|';
Procedure Inst_fmadd;
Var i,o,p:LongInt;
Begin;
if (OutOk=True) then Exit;
if (p1.Typ<>6) then Exit;
if (p2.Typ<>6) then Exit;
if (p3.Typ<>6) then Exit;
if (p4.Typ<>6) then Exit;
if (p5.Typ<>0) then Exit;
i:=0;
if (Cmd='fmadd') then i:=$fc00003a;
if (Cmd='fmadd.') then i:=$fc00003b;
if (Cmd='fmadds') then i:=$ec00003a;
if (Cmd='fmadds.') then i:=$ec00003b;
if (Cmd='fmsub') then i:=$fc000038;
if (Cmd='fmsub.') then i:=$fc000039;
if (Cmd='fmsubs') then i:=$ec000038;
if (Cmd='fmsubs.') then i:=$ec000039;
if (Cmd='fnmadd') then i:=$fc00003e;
if (Cmd='fnmadd.') then i:=$fc00003f;
if (Cmd='fnmadds') then i:=$ec00003e;
if (Cmd='fnmadds.') then i:=$ec00003f;
if (Cmd='fnmsub') then i:=$fc00003c;
if (Cmd='fnmsub.') then i:=$fc00003d;
if (Cmd='fnmsubs') then i:=$ec00003c;
if (Cmd='fnmsubs.') then i:=$ec00003d;
if (Cmd='fsel') then i:=$ec00002e;
if (Cmd='fsel.') then i:=$ec00002f;
if (i=0) then exit;
inc(i,p1.Frg shl 21);
inc(i,p2.Frg shl 16);
inc(i,p3.Frg shl 6);
inc(i,p4.Frg shl 11);
OutStr:=b2h(i shr 24)+SepCh+b2h(i shr 16)+SepCh+b2h(i shr 8)+SepCh+b2h(i);
OutOk:=True;
End;


Const
  Str_fmul='|fmul|fmul.|fmuls|fmuls.|';
Procedure Inst_fmul;
Var i,o,p:LongInt;
Begin;
if (OutOk=True) then Exit;
if (p1.Typ<>6) then Exit;
if (p2.Typ<>6) then Exit;
if (p3.Typ<>6) then Exit;
if (p4.Typ<>0) then Exit;
i:=0;
if (Cmd='fmul') then i:=$fc000032;
if (Cmd='fmul.') then i:=$fc000033;
if (Cmd='fmuls') then i:=$ec000032;
if (Cmd='fmuls.') then i:=$ec000033;
if (i=0) then exit;
inc(i,p1.Frg shl 21);
inc(i,p2.Frg shl 16);
inc(i,p3.Frg shl 6);
OutStr:=b2h(i shr 24)+SepCh+b2h(i shr 16)+SepCh+b2h(i shr 8)+SepCh+b2h(i);
OutOk:=True;
End;


Const
  Str_lfd='|lfd|lfdu|lfs|lfsu|stfd|stfdu|stfs|stfsu|';
Procedure Inst_lfd;
Var i,o,p:LongInt;
Begin;
if (OutOk=True) then Exit;
if (p1.Typ<>6) then Exit;
if (p2.Typ<>3) then Exit;
if (p3.Typ<>0) then Exit;
i:=0;
if (Cmd='lfd') then i:=$c800;
if (Cmd='lfdu') then i:=$cc00;
if (Cmd='lfs') then i:=$c000;
if (Cmd='lfsu') then i:=$c400;
if (Cmd='stfd') then i:=$d800;
if (Cmd='stfdu') then i:=$dc00;
if (Cmd='stfs') then i:=$d000;
if (Cmd='stfsu') then i:=$d400;
if (i=0) then exit;
inc(i,p1.Frg shl 5);
inc(i,p2.Mem.Reg);
OutStr:=b2h(i shr 8)+SepCh+b2h(i)+SepCh+ConvertAllMemoryDatas(p2,2);
OutOk:=True;
End;


Const
  Str_lfdux='|lfdux|lfdx|lfsux|lfsx|stfdux|stfdx|stfsux|stfsx|';
Procedure Inst_lfdux;
Var i,o,p:LongInt;
Begin;
if (OutOk=True) then Exit;
if (p1.Typ<>6) then Exit;
if (p2.Typ<>1) then Exit;
if (p3.Typ<>1) then Exit;
if (p4.Typ<>0) then Exit;
i:=0;
if (Cmd='lfdux') then i:=$7c0004ee;
if (Cmd='lfdx') then i:=$7c0004ae;
if (Cmd='lfsux') then i:=$7c00046e;
if (Cmd='lfsx') then i:=$7c00042e;
if (Cmd='stfdux') then i:=$7c0005ee;
if (Cmd='stfdx') then i:=$7c0005ae;
if (Cmd='stfsux') then i:=$7c00056e;
if (Cmd='stfsx') then i:=$7c00052e;
if (i=0) then exit;
inc(i,p1.Frg shl 21);
inc(i,p2.Reg shl 16);
inc(i,p3.Reg shl 11);
OutStr:=b2h(i shr 24)+SepCh+b2h(i shr 16)+SepCh+b2h(i shr 8)+SepCh+b2h(i);
OutOk:=True;
End;


Const
  Str_mffs='|mffs|mffs.|';
Procedure Inst_mffs;
Var i,o,p:LongInt;
Begin;
if (OutOk=True) then Exit;
if (p1.Typ<>6) then Exit;
if (p2.Typ<>0) then Exit;
i:=0;
if (Cmd='mffs') then i:=$fc00048e;
if (Cmd='mffs.') then i:=$fc00048f;
if (i=0) then exit;
inc(i,p1.Frg shl 21);
OutStr:=b2h(i shr 24)+SepCh+b2h(i shr 16)+SepCh+b2h(i shr 8)+SepCh+b2h(i);
OutOk:=True;
End;


Const
  Str_mtfsf='|mtfsf|mtfsf.|';
Procedure Inst_mtfsf;
Var i,o,p:LongInt;
Begin;
if (OutOk=True) then Exit;
if (p1.Typ<>2) then Exit;
if (p2.Typ<>6) then Exit;
if (p3.Typ<>0) then Exit;
i:=0;
if (Cmd='mtfsf') then i:=$fc00058e;
if (Cmd='mtfsf.') then i:=$fc00058f;
if (i=0) then exit;
inc(i,p2.Frg shl 11);
o:=BVal('$'+p1.Num);
if (o<0) or (o>255) then exit;
inc(i,o shl 17);
OutStr:=b2h(i shr 24)+SepCh+b2h(i shr 16)+SepCh+b2h(i shr 8)+SepCh+b2h(i);
OutOk:=True;
End;


Const
  Str_stfiwx='|stfiwx|';
Procedure Inst_stfiwx;
Var i,o,p:LongInt;
Begin;
if (OutOk=True) then Exit;
if (p1.Typ<>6) then Exit;
if (p2.Typ<>1) then Exit;
if (p3.Typ<>1) then Exit;
i:=0;
if (Cmd='stfiwx') then i:=$7c0007ae;
if (i=0) then exit;
inc(i,p1.Frg shl 21);
inc(i,p2.Reg shl 16);
inc(i,p3.Reg shl 11);
OutStr:=b2h(i shr 24)+SepCh+b2h(i shr 16)+SepCh+b2h(i shr 8)+SepCh+b2h(i);
OutOk:=True;
End;








Var
  a,b:String;
  i,o:LongInt;
Begin;
if (OutOk=True) then Exit;
OutStr:='';
b:=InStr;
b:=xLevesz(b);
if (Copy(b,Length(b),1)=',') then Exit;
if (b='') then Exit;
i:=Pos(' ',b+' ');
Cmd:=Copy(b,1,i-1);
Delete(b,1,i);b:=xLevesz(b);
getParam(p1,b);
getParam(p2,b);
getParam(p3,b);
getParam(p4,b);
AnalizeParameter(b,p5);
OutStr:='er';
if (Cmd='') then Exit;

if BenneVan(Str_mcrfs,Cmd) then Inst_mcrfs;
if BenneVan(Str_mtfsb0,Cmd) then Inst_mtfsb0;
if BenneVan(Str_mtfsfi,Cmd) then Inst_mtfsfi;
if BenneVan(Str_fabs,Cmd) then Inst_fabs;
if BenneVan(Str_fadd,Cmd) then Inst_fadd;
if BenneVan(Str_fcmpo,Cmd) then Inst_fcmpo;
if BenneVan(Str_fmadd,Cmd) then Inst_fmadd;
if BenneVan(Str_fmul,Cmd) then Inst_fmul;
if BenneVan(Str_lfd,Cmd) then Inst_lfd;
if BenneVan(Str_lfdux,Cmd) then Inst_lfdux;
if BenneVan(Str_mffs,Cmd) then Inst_mffs;
if BenneVan(Str_mtfsf,Cmd) then Inst_mtfsf;
if BenneVan(Str_stfiwx,Cmd) then Inst_stfiwx;

if (OutOk=False) then OutStr:='';
End;
