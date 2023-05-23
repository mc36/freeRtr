Const
  vendorIDnat1=#$90#$cb#$80#$91#$3e#$bb#$69#$6e#$08#$63#$81#$b5#$ec#$42#$7b#$1f; {draft2}
  vendorIDnat2=#$7d#$94#$19#$a6#$53#$10#$ca#$6f#$2c#$17#$9d#$92#$15#$52#$9d#$56; {draft3}
  vendorIDnat3=#$4a#$13#$1c#$81#$07#$03#$58#$45#$5c#$57#$28#$f2#$0e#$95#$45#$2f; {rfc}
Type
  OneISAKMPheaderRecord=record
    initCok:array[1..8] of byte;                {initiator cookie}
    respCok:array[1..8] of byte;                {responder cookie}
    nxtPay:Byte;                                {next payload type}
    vers:Byte;                                  {version information}
    exTyp:Byte;                                 {exchange type}
    flags:Byte;                                 {flags value}
    msgID:Longint;                              {message id}
    len:LongInt;                                {length of payload}
    end;
  OneIPSECheaderRecord=record
    spi:LongInt;                                {spi value}
    seq:LongInt;                                {sequence number}
                                {iv}
                                {payload}
                                {ah}
    end;
  OneGenericMextHeaderRecord=record
    nxtPay:Byte;                                {next payload type}
    res1:Byte;                                  {reserved}
    len:Word;                                   {length of payload}
                                {data}
    end;
  OneAttributeHeaderRecord=record
    typ:Word;                                   {attribute type}
    len:Word;                                   {attribute length}
                                {data}
    end;
  OneSecurityAssociationRecord=record
    nxtPay:Byte;                                {next payload type}
    res1:Byte;                                  {reserved}
    len:Word;                                   {length of payload}
    doi:LongInt;                                {domain of interpretation}
    situ:LongInt;                               {situation index}
                                {situation data}
    end;
  OneProposalHeaderRecord=record
    nxtPay:Byte;                                {next payload type}
    res1:Byte;                                  {reserved}
    len:Word;                                   {length of payload}
    propN:Byte;                                 {proposal #}
    prot:Byte;                                  {protocol id}
    spiLen:Byte;                                {spi size}
    trans:Byte;                                 {# of transforms}
                                {spi data}
    end;
  OneTransformHeaderRecord=record
    nxtPay:Byte;                                {next payload type}
    res1:Byte;                                  {reserved}
    len:Word;                                   {length of payload}
    trnsN:Byte;                                 {transform #}
    trnsI:Byte;                                 {transform id}
    res2:Word;                                  {reserved}
                                {sa attrs}
    end;
  OneNonceHeaderRecord=record
    nxtPay:Byte;                                {next payload type}
    res1:Byte;                                  {reserved}
    len:Word;                                   {length of payload}
                                {nonce data}
    end;
  OneKeyExchangeHeaderRecord=record
    nxtPay:Byte;                                {next payload type}
    res1:Byte;                                  {reserved}
    len:Word;                                   {length of payload}
                                {key xchg data}
    end;
  OneIdentificateHeaderRecord=record
    nxtPay:Byte;                                {next payload type}
    res1:Byte;                                  {reserved}
    len:Word;                                   {length of payload}
    typ:Byte;                                   {id type}
    prot:Byte;                                  {protocol}
    port:Word;                                  {port}
                                {id data}
    end;
  OneHashingStateHeaderRecord=record
    nxtPay:Byte;                                {next payload type}
    res1:Byte;                                  {reserved}
    len:Word;                                   {length of payload}
                                {hash data}
    end;
  OneNotifyHeaderRecord=record
    nxtPay:Byte;                                {next payload type}
    res1:Byte;                                  {reserved}
    len:Word;                                   {length of payload}
    doi:LongInt;                                {domain of interpretation}
    prtId:Byte;                                 {protocol id}
    spiLen:Byte;                                {spi size}
    notId:Word;                                 {notify type}
                                {spi data}
    end;
  OneVendorIdHeaderRecord=record
    nxtPay:Byte;                                {next payload type}
    res1:Byte;                                  {reserved}
    len:Word;                                   {length of payload}
                                {data}
    end;
  OneNatDetectHeaderRecord=record
    nxtPay:Byte;                                {next payload type}
    res1:Byte;                                  {reserved}
    len:Word;                                   {length of payload}
                                {data}
    end;
