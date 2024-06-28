#ifndef HAVE_NOCRYPTO

#include <openssl/conf.h>
#if OPENSSL_VERSION_NUMBER >= 0x30000000L
#include <openssl/provider.h>
#endif
#include <openssl/evp.h>
#include <openssl/rand.h>

#else

typedef char EVP_CIPHER;
typedef char EVP_MD_CTX;
typedef char EVP_CIPHER_CTX;
typedef char EVP_MD;
typedef char EVP_PKEY;
#define EVP_CIPHER_CTX_new() NULL
#define EVP_MD_CTX_new() NULL

#endif

#define maxPorts 128
#define preBuff 512
#define minBuff 128
#define maxBuff 1024

extern int dataPorts;
extern int cpuPort;
extern char *ifaceName[maxPorts];

void err(char*buf);
void sendPack(unsigned char *bufD, int bufS, int port);
void setMtu(int port, int mtu);
void setState(int port, int sta);
int getState(int port);
void getStats(int port, unsigned char*buf, unsigned char*pre, int*len);

extern void initIface(int port, char *name);
extern int initTables();
extern int hashDataPacket(unsigned char *bufP);
extern void processDataPacket(unsigned char *bufA, unsigned char *bufB, unsigned char *bufC, unsigned char *bufD, int bufS, int port, int prt, EVP_CIPHER_CTX *encrCtx, EVP_MD_CTX *hashCtx);
extern void processCpuPack(unsigned char *bufA, unsigned char *bufB, unsigned char *bufC, unsigned char* bufD, int bufS, EVP_CIPHER_CTX *encrCtx, EVP_MD_CTX *hashCtx);
extern int doOneCommand(unsigned char* buf, EVP_CIPHER_CTX *encrCtx, EVP_MD_CTX *hashCtx);
extern void doStatRound(FILE *commands, int round);
extern int doConsoleCommand(unsigned char*buf);
extern char* getCapas();

#define platformBase "p4emu/"
