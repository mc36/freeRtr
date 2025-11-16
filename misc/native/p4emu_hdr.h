struct ifaceStat_entry {
    long byteRx;
    long packRx;
    long byteTx;
    long packTx;
    long byteDr;
    long packDr;
    long byteMpls;
    long packMpls;
    long byteVlan;
    long packVlan;
    long byteIpv4;
    long packIpv4;
    long byteIpv6;
    long packIpv6;
    long bytePppoe;
    long packPppoe;
    long byteBridge;
    long packBridge;
    long bytePolka;
    long packPolka;
    long byteNsh;
    long packNsh;
};

struct packetContext {
    int port;
    struct ifaceStat_entry *stat;
    int hash;
    int sgt;
    unsigned char *bufB3;
    unsigned char *bufB2;
    unsigned char *bufB1;
    unsigned char *bufC;
    unsigned char *bufD;
    unsigned char *bufH;
    void *encr; // EVP_CIPHER_CTX
    void *dgst; // EVP_MD_CTX
};

#define maxPorts 128
#define preBuff 512
#define minBuff 128
#define maxBuff 1024
#define totBuff 69632
#define maxKeys 384

extern int commandSock;
extern int dataPorts;
extern int cpuPort;
extern char *ifaceName[maxPorts];
extern struct ifaceStat_entry *ifaceStat[maxPorts];

void err(char*buf);
void sendPack(unsigned char *bufD, int bufS, int port);
void setMtu(int port, int mtu);
void setState(int port, int sta);
int getState(int port);
void getStats(int port, unsigned char*buf, unsigned char*pre, int*len);

extern void initIface(int port, char *name);
extern int initTables();
extern int hashDataPacket(unsigned char *bufP);
extern void processDataPacket(struct packetContext *ctx, int bufS, int prt);
extern int doOneCommand(struct packetContext *ctx, unsigned char* buf);
extern int initContext(struct packetContext *ctx);
extern void doNegotiate(char*name);
extern void doSockLoop();
extern void doStatLoop();
extern void doMainLoop();
