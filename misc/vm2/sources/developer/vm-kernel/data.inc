Type
  byteArrayRecord=array[0..1] of byte;
  longArrayRecord=array[0..1] of longInt;
  oneHandlerRecord=array[0..511] of byte;
  oneListingRecord=record
    beginning:LongInt;          {dword offset}
    number:LongInt;             {number of entries}
    maximum:LongInt;            {maximum entries allowed}
    sorted:LongInt;             {sorted or not flag}
    end;
  oneFileRecord=record
    id:LongInt;                 {id of handle}
    ownerPID:LongInt;           {pid of user process}
    drivePID:LongInt;           {pid of drive process}
    driveLtr:LongInt;           {letter of drive}
    right:LongInt;              {used/allowed rights}
    inode:LongInt;              {inode number}
    handle:oneHandlerRecord;    {file handle}
    end;
  onePipelineRecord=record
    id:LongInt;                 {pipeline id}
    size:LongInt;               {size of record}
    sizHF:LongInt;              {size of buffer}
    block:LongInt;              {block mode; 1=yes, 0=no}
    sizCA:LongInt;              {size of c->a buffer}
    sizAC:LongInt;              {size of a->c buffer}
    pidC:LongInt;               {caller's pid}
    pidA:LongInt;               {answerer's pid}
    buf:array[0..3] of byte;    {buffer}
    end;
  oneDriveRecord=record
    drivePid:LongInt;           {pid of drive process}
    driveBuf:LongInt;           {buffer in drive}
    busy:LongInt;               {busy/used flag}
    userPid:LongInt;            {pid of user process}
    userBuf:LongInt;            {buffer in user}
    userSiz:LongInt;            {size of buffer}
    fileID:LongInt;             {file handle id}
    command:LongInt;            {command to issue}
    end;
  oneProcessRecord=record
    pid:LongInt;                {process id}
    regs:array[0..7] of LongInt;{registers}
    flags:LongInt;              {1=above, 2=below, 4=equal}
    regIP:LongInt;              {instruction pointer}
    regSP:LongInt;              {stack pointer}
    ppid:LongInt;               {parent process id}
    cuid:LongInt;               {current user id}
    ouid:LongInt;               {original user id}
    right:LongInt;              {rights}
    codeSiz:LongInt;            {size of code}
    stackEnd:LongInt;           {size of stack}
    heapEnd:LongInt;            {size of heap}
    heapBeg:LongInt;            {beginning of heap}
    heapFree:LongInt;           {offset in heap memory}
    heapCurr:LongInt;           {currently invoking process}
    procNum:LongInt;            {number of procedures}
    procTab:LongInt;            {offset of procedure table}
    filNam:String;              {process pathname}
    params:String;              {parameter string}
    curDir:String;              {current directory}
    status:LongInt;             {status: 1-run, 2-load, 3-fileio, 4-execwait}
    oldStat:LongInt;            {status before status change}
    execWait:LongInt;           {waiting for this process id}
    consProc:LongInt;           {console owner process id}
    consPipe:LongInt;           {console pipeline id}
    consSizX:LongInt;           {size of console}
    consSizY:LongInt;           {size of console}
    lstnPipe:LongInt;           {listening pipeline id}
    workSince:LongInt;          {ticks since last control giveaway}
    activeRun:LongInt;          {times actively started}
    timesRun:LongInt;           {times process started}
    timesBusy:LongInt;          {times process was busy}
    relequish:LongInt;          {times process relequished}
    extBeg:LongInt;             {first byte of extended memory}
    blockOfs:LongInt;           {dword offset of block list}
    blockFwd:longInt;           {used blocks forward}
    blockBck:longInt;           {used blocks backward}
    blkD:array[0..0] of LongInt;{block list}
    end;
  oneExecutableRecord=record
    magic:LongInt;              {magic value}
    codeS:LongInt;              {size of code}
    stackS:LongInt;             {size of stack}
    heapS:LongInt;              {size of heap}
    procN:LongInt;              {number of procedures}
    entry:LongInt;              {entry point}
    end;
  oneCommunicationRecord=record
    cmd:LongInt;                {command}
    uid:LongInt;                {user id}
    rgt:LongInt;                {rights}
    siz:LongInt;                {size of data}
    dir:String;                 {working directory}
    fn1:String;                 {name of file}
    fn2:String;                 {target name}
    hdr:oneHandlerRecord;       {file handler}
    dat:array[0..0] of LongInt; {buffer}
    end;
Const
  reg_a=1;
  reg_b=2;
  reg_c=3;
  reg_d=4;
  reg_src=5;
  reg_trg=6;
  memoryPageBits=10;
  memoryPageByte=1 shl memoryPageBits;
  memoryPageMask=memoryPageByte-1;
  memoryPageNegM=-memoryPageByte;
  kernelProcNum=-1;
  kernelFakeNum=$deadbabe;
  processBlockMax=65536;
  executableMagic=$30314d56;
  extraRightsMask=$40;
  dirRightMask=$80;
Var
  totalMemoryByte:^ByteArrayRecord;
  totalMemoryLong:^longArrayRecord;
  totalMemorySize:LongInt;
  totalMemoryPage:LongInt;
  totalMemoryPhys:LongInt;
  kernelBigBuf:LongInt;
  listingNextID:LongInt;
  listingProc:oneListingRecord;
  listingPipe:oneListingRecord;
  listingFile:oneListingRecord;
  listingDrvr:array[0..255] of oneDriveRecord;
  crc32table:Array [0..255] of LongInt;
  kernelLog1,kernelLog2:String;
  kernelInf1,kernelInf2:String;
  timesRelequished:LongInt;
  timesFullRounds:LongInt;
  timesHalfRounds:LongInt;
  timesActiveStarts:LongInt;
  defaultScreenX,defaultScreenY:LongInt;
  kernelConsoleRaw:LongInt;
  kernelConsoleData:LongInt;
  kernelConsoleCtrl:LongInt;
  kernelConsoleSkip:LongInt;
