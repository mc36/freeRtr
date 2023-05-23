Const DriveInternalNum=$80;
Var
  DriveInternalCyl:LongInt;
  DriveInternalHed:LongInt;
  DriveInternalSec:LongInt;

Function DriveOpen(nam:String;num:longint):Word;
Type
  OnePartiData=record
    boot:byte;
    beg1:byte;
    beg2:word;
    osid:byte;
    end1:byte;
    end2:word;
    beg:longint;
    siz:longint;
    end;
  OnePartiSec=record
    r1:array[1..512-4*sizeof(OnePartiData)-2] of byte;
    d:array[1..4] of OnePartiData;
    r2:word;
    end;
Var
  dat:OnePartiSec;
  datp:pointer;
  r1,r2:Word;
Begin;
DriveOpen:=15;
datp:=@dat;
asm
  mov ah,08h
  mov dl,DriveInternalNum
  int 13h
  jnc @j1
  sub cx,cx
  sub dx,dx
  @j1:
  mov [r1],cx
  mov [r2],dx
  end;
if (r1=0) and (r2=0) then exit;
DriveInternalHed:=(r2 shr 8)+1;
DriveInternalSec:=r1 and $3f;
r2:=(r1 shr 6) and 3;
DriveInternalCyl:=(r2 shl 8)+(r1 shr 8)+2;
asm
  les bx,[datp]
  mov cx,1
  mov dx,DriveInternalNum
  mov ax,0201h
  int 13h
  end;
r2:=0;
for r1:=1 to 4 do if (dat.d[r1].osid=$b0) then r2:=r1;
if (r2=0) then exit;
DriveSize:=dat.d[r2].siz;
DriveBegin:=dat.d[r2].beg;
DriveOpen:=0;
End;

Function DriveClose:Word;
Begin;
DriveClose:=0;
End;

procedure convert(num:longint;var r1,r2:word);
begin;
inc(num,DriveBegin);
r2:=longint(num div (DriveInternalHed*DriveInternalSec));
r1:=(r2 shl 8)+((r2 and $300) shr 2);
num:=longint(num mod (DriveInternalHed*DriveInternalSec));
r1:=r1 or (((num mod DriveInternalSec)+1) and $3f);
r2:=((num div DriveInternalSec) shl 8) or DriveInternalNum;
end;


Function DriveRead(sec:longint;var buf):Word;
Var r1,r2:word;bufp:pointer;
Begin;
bufp:=@buf;
convert(sec,r1,r2);
asm
  mov cx,[r1]
  mov dx,[r2]
  les bx,[bufp]
  mov ax,0201h
  int 13h
  end;
DriveRead:=0;
End;

Function DriveWrite(sec:longint;var buf):Word;
Var r1,r2:word;bufp:pointer;
Begin;
if DriveReadOnly then begin; DriveWrite:=4;exit; end;
bufp:=@buf;
convert(sec,r1,r2);
asm
  mov cx,[r1]
  mov dx,[r2]
  les bx,[bufp]
  mov ax,0301h
  int 13h
  end;
DriveWrite:=0;
End;
