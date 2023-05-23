Procedure CacheInit;
Begin;
End;

Function CacheFlush:Word;
Begin;
CacheFlush:=0;
End;

Function CacheRead(sec:longint;var buf):Word;
Begin;
CacheRead:=DriveRead(sec,buf);
End;

Function CacheWrite(sec:longint;var buf):Word;
Begin;
CacheWrite:=DriveWrite(sec,buf);
End;
