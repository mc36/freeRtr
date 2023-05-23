Var ioaccess:LongInt;

Function readPort(p,s:LongInt):LongInt;
Begin;
inc(ioaccess);
readPort:=$ffffffff;
End;

Procedure writePort(p,s,d:LongInt);
Begin;
inc(ioaccess);
End;
