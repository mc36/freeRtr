Type onePacketHeader=record
  id:LongInt;                 {STF#1}
  len:Word;                   {length of data}
  typ:Word;                   {type of header}
  end;
Const packetMagicCookie=$53544601;
