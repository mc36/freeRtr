int punts = 0;

int dataPorts;
int cpuPort;
char *ifaceName[maxPorts];
long int byteRx[maxPorts];
long int packRx[maxPorts];
long int byteTx[maxPorts];
long int packTx[maxPorts];
long int byteDr[maxPorts];
long int packDr[maxPorts];
long int byteMpls[maxPorts];
long int packMpls[maxPorts];
long int byteVlan[maxPorts];
long int packVlan[maxPorts];
long int byteIpv4[maxPorts];
long int packIpv4[maxPorts];
long int byteIpv6[maxPorts];
long int packIpv6[maxPorts];
long int bytePppoe[maxPorts];
long int packPppoe[maxPorts];
long int byteBridge[maxPorts];
long int packBridge[maxPorts];
#ifdef HAVE_POLKA
long int bytePolka[maxPorts];
long int packPolka[maxPorts];
long int byteMpolka[maxPorts];
long int packMpolka[maxPorts];
#endif
long int byteNsh[maxPorts];
long int packNsh[maxPorts];




#ifdef HAVE_POLKA
struct polkaIdx_entry {
    int index;
    int nexthop;
    long pack;
    long byte;
};
#endif


#ifdef HAVE_POLKA
struct polkaPoly_entry {
    int port;
    int tab[256];
    long pack;
    long byte;
};

struct table_head polkaPoly_table;

struct table_head mpolkaPoly_table;



void crc16mktab(int *tab, int poly) {
    for (int o=0; o < 256; o++) {
        int v = o << 8;
        for (int i = 0; i < 8; ++i) {
            v <<= 1;
            if ((v & 0x10000) != 0) {
                v ^= poly;
            }
        }
        tab[o] = v & 0xffff;
    }
}

#define crc16calc(tmp, tab, buf, ofs, len)                                      \
    tmp = 0;                                                                    \
    for (i = 0; i < len; i++) {                                                 \
        tmp = ((tmp << 8) & 0xffff) ^ tab[(tmp >> 8) ^ (buf[ofs + i] & 0xff)];  \
    }
#endif


struct nsh_entry {
    int sp;
    int si;
    int command;    // 1=swap, 2=vrf
    int port;
    int vrf;
    int trg;
    unsigned char macs[12];
    long pack;
    long byte;
};

struct table_head nsh_table;



struct mpls_entry {
    int label;
    int command;    // 1=vrf, 2=pop, 3=swap, 4=xconn, 5=vpls, 6=punt, 7=dup, 8=bier
    int nexthop;
    int port;
    int bridge;
    int vrf;
    int ver;
    int swap;
    int bier[8];
    struct table_head flood;
    long pack;
    long byte;
};

struct table_head mpls_table;



#define mpls_init table_addinited(&mpls_table, &mpls_ntry, &mpls_ntry.flood, sizeof(struct flood_entry), 3);

struct port2vrf_entry {
    int port;
    int command;    // 1=vrf, 2=bridge, 3=xconn, 4=loconn
    int vrf;
    int bridge;
    int nexthop;
    int label1;
    int label2;
    int tcpmss4;
    int tcpmss6;
    int verify4;
    int verify6;
    int pmtud4;
    int pmtud6;
    int nflw4;
    int nflw6;
    int pttl4;
    int pttl6;
    int mpls;
    int nsh;
    int monTarget;
    int monSample;
    int monTruncate;
    int monPackets;
    int sgtSet;
    int sgtTag;
    int mcscEthtyp;
    unsigned char mcscEncrKeyDat[256];
    unsigned char mcscHashKeyDat[256];
    int mcscEncrKeyLen;
    int mcscHashKeyLen;
    int mcscEncrBlkLen;
    int mcscHashBlkLen;
    int mcscNeedMacs;
    int mcscNeedAead;
    int mcscSeqTx;
    int mcscSeqRx;
    const EVP_CIPHER *mcscEncrAlg;
    const EVP_MD *mcscHashAlg;
    EVP_PKEY *mcscHashPkey;
    long mcscPackRx;
    long mcscByteRx;
    long mcscPackOk;
    long mcscByteOk;
    long mcscPackTx;
    long mcscByteTx;
};

struct table_head port2vrf_table;


struct port2vrf_entry* port2vrf_init(struct port2vrf_entry *ntry) {
    int index = table_find(&port2vrf_table, ntry);
    if (index >= 0) return table_get(&port2vrf_table, index);
    table_add(&port2vrf_table, ntry);
    index = table_find(&port2vrf_table, ntry);
    ntry = table_get(&port2vrf_table, index);
    ntry->monTarget = -1;
    ntry->sgtSet = -1;
    return ntry;
}


struct vrf2rib_entry {
    int vrf;
    struct tree_head rou;
    struct table_head trns;
    struct table_head tun;
    struct table_head mcst;
    struct table_head pbr;
#ifdef HAVE_POLKA
    struct table_head plk;
#endif
    long pack;
    long byte;
};


struct table_head vrf2rib4_table;

struct table_head vrf2rib6_table;




struct route4_entry {
    int mask;
    int addr[1];
    int command;    // 1=route, 2=punt, 3=mpls1, 4=mpls2, 5=srv6, 6=mysrv4, 7=mysrv6, 8=brsrv, 9=polka, 10=mpolka, 11=drop
    int nexthop;
    int label1;
    int label2;
    int srv1;
    int srv2;
    int srv3;
    int srv4;
#ifdef HAVE_POLKA
    unsigned char polka[16];
#endif
    long packTx;
    long byteTx;
    long packRx;
    long byteRx;
};


struct route6_entry {
    int mask;
    int addr[4];
    int command;    // 1=route, 2=punt, 3=mpls1, 4=mpls2, 5=srv6, 6=mysrv4, 7=mysrv6, 8=brsrv, 9=polka, 10=mpolka, 11=drop
    int nexthop;
    int label1;
    int label2;
    int srv1;
    int srv2;
    int srv3;
    int srv4;
#ifdef HAVE_POLKA
    unsigned char polka[16];
#endif
    long packTx;
    long byteTx;
    long packRx;
    long byteRx;
};


struct neigh_entry {
    int id;
    int vrf;
    int command;    // 1=rawip, 2=pppoe, 3=gre4, 4=gre6, 5=l2tp4, 6=l2tp6, 7=ipip4, 8=ipip6, 9=esp4, 10=esp6, 11=ovpn4, 12=ovpn6, 13=wg4. 14=wg6, 15=amt4, 16=amt6, 17=gtp4, 18=gtp6, 19=l3tp4, 20=l3tp6, 21=tmux4, 22=tmux6
    int port;
    int aclport;
    int session;
    unsigned char macs[12];
    int sip1;
    int sip2;
    int sip3;
    int sip4;
    int dip1;
    int dip2;
    int dip3;
    int dip4;
    int sprt;
    int dprt;
    int tid;
    int spi;
    int frag;
    unsigned char encrKeyDat[256];
    unsigned char hashKeyDat[256];
    int encrKeyLen;
    int hashKeyLen;
    int encrBlkLen;
    int hashBlkLen;
    int seq;
    const EVP_CIPHER *encrAlg;
    const EVP_MD *hashAlg;
    EVP_PKEY *hashPkey;
    long pack;
    long byte;
};

struct table_head neigh_table;


struct bridge_entry {
    int id;
    int mac2;
    int mac1;
    int command;    // 1=port, 2=vpls, 3=route, 4=vxlan4, 5=vxlan6, 6=pckoudp4, 7=pckoudp6, 8=srv4, 9=srv6, 10=etherip4, 11=etherip6
    int port;
    int nexthop;
    int label1;
    int label2;
    int srcAddr1;
    int srcAddr2;
    int srcAddr3;
    int srcAddr4;
    int trgAddr1;
    int trgAddr2;
    int trgAddr3;
    int trgAddr4;
    int srcPort;
    int trgPort;
    int instance;
    long packRx;
    long byteRx;
    long packTx;
    long byteTx;
};

struct table_head bridge_table;


struct vlanin_entry {
    int vlan;
    int port;
    int id;
    long pack;
    long byte;
};

struct table_head vlanin_table;


struct vlanout_entry {
    int id;
    int vlan;
    int port;
    long pack;
    long byte;
};

struct table_head vlanout_table;



struct acls_entry {
    int dir; // 1=inacl, 2=outacl, 3=nat, 4=copp, 5=pbr, 6=inqos, 7=outqos, 8=flwspc
    int port;
    struct table_head aces;
    struct table_head *insp;
};

struct table_head acls4_table;

struct table_head acls6_table;


struct acl4_entry {
    int pri;
    int act;
    long pack;
    long byte;
    int cmd;
    int vrf;
    int nexthop;
    int label;
    int srcAddr;
    int srcMask;
    int trgAddr;
    int trgMask;
    int protV;
    int protM;
    int srcPortV;
    int srcPortM;
    int trgPortV;
    int trgPortM;
    int tosV;
    int tosM;
    int flowV;
    int flowM;
    int sgtV;
    int sgtM;
};


int acl4_matcher(void *ptr1, void *ptr2) {
    struct acl4_entry *ntry1 = ptr1;
    struct acl4_entry *ntry2 = ptr2;
    if ((ntry1->protV & ntry2->protM) != ntry2->protV) return 1;
    if ((ntry1->srcPortV & ntry2->srcPortM) != ntry2->srcPortV) return 1;
    if ((ntry1->trgPortV & ntry2->trgPortM) != ntry2->trgPortV) return 1;
    if ((ntry1->tosV & ntry2->tosM) != ntry2->tosV) return 1;
    if ((ntry1->flowV & ntry2->flowM) != ntry2->flowV) return 1;
    if ((ntry1->sgtV & ntry2->sgtM) != ntry2->sgtV) return 1;
    if ((ntry1->srcAddr & ntry2->srcMask) != ntry2->srcAddr) return 1;
    if ((ntry1->trgAddr & ntry2->trgMask) != ntry2->trgAddr) return 1;
    return 0;
}


struct acl6_entry {
    int pri;
    int act;
    long pack;
    long byte;
    int cmd;
    int vrf;
    int nexthop;
    int label;
    int srcAddr1;
    int srcAddr2;
    int srcAddr3;
    int srcAddr4;
    int srcMask1;
    int srcMask2;
    int srcMask3;
    int srcMask4;
    int trgAddr1;
    int trgAddr2;
    int trgAddr3;
    int trgAddr4;
    int trgMask1;
    int trgMask2;
    int trgMask3;
    int trgMask4;
    int protV;
    int protM;
    int srcPortV;
    int srcPortM;
    int trgPortV;
    int trgPortM;
    int tosV;
    int tosM;
    int flowV;
    int flowM;
    int sgtV;
    int sgtM;
};


int acl6_matcher(void *ptr1, void *ptr2) {
    struct acl6_entry *ntry1 = ptr1;
    struct acl6_entry *ntry2 = ptr2;
    if ((ntry1->protV & ntry2->protM) != ntry2->protV) return 1;
    if ((ntry1->srcPortV & ntry2->srcPortM) != ntry2->srcPortV) return 1;
    if ((ntry1->trgPortV & ntry2->trgPortM) != ntry2->trgPortV) return 1;
    if ((ntry1->tosV & ntry2->tosM) != ntry2->tosV) return 1;
    if ((ntry1->flowV & ntry2->flowM) != ntry2->flowV) return 1;
    if ((ntry1->sgtV & ntry2->sgtM) != ntry2->sgtV) return 1;
    if ((ntry1->srcAddr1 & ntry2->srcMask1) != ntry2->srcAddr1) return 1;
    if ((ntry1->srcAddr2 & ntry2->srcMask2) != ntry2->srcAddr2) return 1;
    if ((ntry1->srcAddr3 & ntry2->srcMask3) != ntry2->srcAddr3) return 1;
    if ((ntry1->srcAddr4 & ntry2->srcMask4) != ntry2->srcAddr4) return 1;
    if ((ntry1->trgAddr1 & ntry2->trgMask1) != ntry2->trgAddr1) return 1;
    if ((ntry1->trgAddr2 & ntry2->trgMask2) != ntry2->trgAddr2) return 1;
    if ((ntry1->trgAddr3 & ntry2->trgMask3) != ntry2->trgAddr3) return 1;
    if ((ntry1->trgAddr4 & ntry2->trgMask4) != ntry2->trgAddr4) return 1;
    return 0;
}


struct aclH_entry {
    int pri;
    int act;        // 0=permit, 1=deny, 2=punt
    long pack;
    long byte;
    int cmd;        // 1=normal, 2=setvrf, 3=sethop, 4=setlab
    int vrf;
    int nexthop;
    int label;
};

struct aclH_entry* search_ace(struct table_head *tab, void *ntry, int matcher(void *, void *),int siz) {
    for (int i=0; i<tab->size; i++) {
        struct aclH_entry *res = table_get(tab, i);
        if (matcher(ntry, res) != 0) continue;
        res->pack++;
        res->byte += siz;
        return res;
    }
    return NULL;
}

int apply_acl(struct table_head *tab, void *ntry, int matcher(void *, void *),int siz) {
    struct aclH_entry *res = search_ace(tab, ntry, matcher, siz);
    if (res == NULL) return 1;
    return res->act;
}

struct acls_entry* acls_init(struct table_head *tab, struct acls_entry *ntry, int reclen1, int reclen2, int acer, int insper) {
    int index = table_find(tab, ntry);
    int oidx = index;
    if (index < 0) {
        table_add(tab, ntry);
        index = table_find(tab, ntry);
    }
    struct acls_entry *res = table_get(tab, index);
    if ((ntry->dir < 3) && (oidx < 0)) {
        ntry->dir = 3 - ntry->dir;
        index = table_find(tab, ntry);
        if (index >= 0) {
            struct acls_entry *oth = table_get(tab, index);
            res->insp = oth->insp;
        }
        ntry->dir = 3 - ntry->dir;
    }
    struct table_head *tab3 = &res->aces;
    if (tab3->reclen != reclen1) table_init(tab3, reclen1, acer);
    if (res->insp == NULL) {
        res->insp = malloc(sizeof(struct table_head));
        if (res->insp == NULL) err("error allocating memory");
        table_init(res->insp, reclen2, insper);
    }
    return res;
}

#define acls_init4 acls_init(&acls4_table, &acls_ntry, sizeof(struct acl4_entry), sizeof(struct insp4_entry), 1, 5);
#define acls_init6 acls_init(&acls6_table, &acls_ntry, sizeof(struct acl6_entry), sizeof(struct insp6_entry), 1, 11);



struct insp4_entry {
    int srcPort;
    int trgPort;
    int srcAddr;
    int trgAddr;
    int prot;
    long packRx;
    long byteRx;
    long packTx;
    long byteTx;
};

struct nat4_entry {
    int oSrcPort;
    int oTrgPort;
    int oSrcAddr;
    int oTrgAddr;
    int prot;
    int nSrcAddr;
    int nTrgAddr;
    int nSrcPort;
    int nTrgPort;
    int sum3;
    int sum4;
    long pack;
    long byte;
};



struct insp6_entry {
    int srcPort;
    int trgPort;
    int srcAddr4;
    int trgAddr4;
    int srcAddr3;
    int trgAddr3;
    int srcAddr2;
    int srcAddr1;
    int trgAddr2;
    int trgAddr1;
    int prot;
    long packRx;
    long byteRx;
    long packTx;
    long byteTx;
};

struct nat6_entry {
    int oSrcPort;
    int oTrgPort;
    int oSrcAddr4;
    int oTrgAddr4;
    int oSrcAddr3;
    int oTrgAddr3;
    int oSrcAddr2;
    int oTrgAddr2;
    int oSrcAddr1;
    int oTrgAddr1;
    int prot;
    int nSrcAddr1;
    int nSrcAddr2;
    int nSrcAddr3;
    int nSrcAddr4;
    int nTrgAddr1;
    int nTrgAddr2;
    int nTrgAddr3;
    int nTrgAddr4;
    int nSrcPort;
    int nTrgPort;
    int sum3;
    int sum4;
    long pack;
    long byte;
};


struct bundle_entry {
    int id;
    int command;    // 1=port, 2=hairpin
    int out[16];
    long pack;
    long byte;
};

struct table_head bundle_table;


struct pppoe_entry {
    int port;
    int session;
    int aclport;
    int reasmS;
    unsigned char *reasmB;
    long pack;
    long byte;
};

struct table_head pppoe_table;


struct tun4_entry {
    int srcPort;
    int trgPort;
    int srcAddr;
    int trgAddr;
    int prot;
    int command;    // 1=gre, 2=l2tp, 3=vxlan, 4=ip4ip, 5=ip6ip, 6=pckoudp, 7=esp, 8=openvpn, 9=wireguard, 10=amt, 11=gtp, 12=l3tp, 13=tmux, 14=etherip
    int aclport;
    int spi;
    int reasmS;
    unsigned char *reasmB;
    unsigned char encrKeyDat[256];
    unsigned char hashKeyDat[256];
    int encrKeyLen;
    int hashKeyLen;
    int encrBlkLen;
    int hashBlkLen;
    int seq;
    const EVP_CIPHER *encrAlg;
    const EVP_MD *hashAlg;
    EVP_PKEY *hashPkey;
    long pack;
    long byte;
};


struct tun6_entry {
    int srcPort;
    int trgPort;
    int srcAddr4;
    int trgAddr4;
    int srcAddr3;
    int trgAddr3;
    int srcAddr2;
    int trgAddr2;
    int srcAddr1;
    int trgAddr1;
    int prot;
    int command;    // 1=gre, 2=l2tp, 3=vxlan, 4=ip4ip, 5=ip6ip, 6=pckoudp, 7=esp, 8=openvpn, 9=wireguard, 10=amt, 11=gtp, 12=l3tp, 13=tmux, 14=etherip
    int aclport;
    int spi;
    int reasmS;
    unsigned char *reasmB;
    unsigned char encrKeyDat[256];
    unsigned char hashKeyDat[256];
    int encrKeyLen;
    int hashKeyLen;
    int encrBlkLen;
    int hashBlkLen;
    int seq;
    const EVP_CIPHER *encrAlg;
    const EVP_MD *hashAlg;
    EVP_PKEY *hashPkey;
    long pack;
    long byte;
};


struct policer_entry {
    int vrf;
    int dir; // 1=in, 2=out, 3=flwspc4, 4=flwspc6
    int meter;
    long allow;
    long avail;
};

struct table_head policer_table;


struct flood_entry {
    int command; // 1=iface, 2=mpls, 3=biermsk, 4=bierset
    int trg;
    int id;
    int lab;
    int src;
    unsigned char macs[12];
    int bier[8];
};


struct mroute4_entry {
    int grp;
    int src;
    int ingr;
    int local;
    struct table_head flood;
    long pack;
    long byte;
};



struct mroute6_entry {
    int grp1;
    int grp2;
    int grp3;
    int grp4;
    int src1;
    int src2;
    int src3;
    int src4;
    int ingr;
    int local;
    struct table_head flood;
    long pack;
    long byte;
};



#define mcast_init4 table_addinited(&vrf2rib_res->mcst, &mroute4_ntry, &mroute4_ntry.flood, sizeof(struct flood_entry), 3);
#define mcast_init6 table_addinited(&vrf2rib_res->mcst, &mroute6_ntry, &mroute6_ntry.flood, sizeof(struct flood_entry), 3);


struct vrf2rib_entry* vrf2rib_init(struct table_head *tab, struct vrf2rib_entry *ntry, int reclen1, int reclen2, int reclen3, int reclen4, int reclen5, int natter, int tunner, int mcaster) {
    int index = table_find(tab, ntry);
    if (index < 0) {
        table_add(tab, ntry);
        index = table_find(tab, ntry);
    }
    struct vrf2rib_entry* res = table_get(tab, index);
    struct tree_head *tab2 = &res->rou;
    if (tab2->reclen != reclen1) tree_init(tab2, reclen1);
    struct table_head *tab3 = &res->trns;
    if (tab3->reclen != reclen2) table_init(tab3, reclen2, natter);
    tab3 = &res->tun;
    if (tab3->reclen != reclen3) table_init(tab3, reclen3, tunner);
    tab3 = &res->mcst;
    if (tab3->reclen != reclen4) table_init(tab3, reclen4, mcaster);
    tab3 = &res->pbr;
    if (tab3->reclen != reclen5) table_init(tab3, reclen5, mcaster);
#ifdef HAVE_POLKA
    tab3 = &res->plk;
    reclen5 = sizeof(struct polkaIdx_entry);
    if (tab3->reclen != reclen5) table_init(tab3, reclen5, 1);
#endif
    return res;
}

#define vrf2rib_init4 vrf2rib_init(&vrf2rib4_table, &vrf2rib_ntry, sizeof(struct route4_entry), sizeof(struct nat4_entry), sizeof(struct tun4_entry), sizeof(struct mroute4_entry), sizeof(struct acl4_entry), 5, 5, 2)
#define vrf2rib_init6 vrf2rib_init(&vrf2rib6_table, &vrf2rib_ntry, sizeof(struct route6_entry), sizeof(struct nat6_entry), sizeof(struct tun6_entry), sizeof(struct mroute6_entry), sizeof(struct acl6_entry), 11, 11, 8)







void initIface(int port, char *name) {
    ifaceName[port] = malloc(strlen(name)+1);
    if (ifaceName[port] == NULL) err("error allocating memory");
    strcpy(ifaceName[port], name);
    byteRx[port] = 0;
    packRx[port] = 0;
    byteTx[port] = 0;
    packTx[port] = 0;
    byteDr[port] = 0;
    packDr[port] = 0;
    byteMpls[port] = 0;
    packMpls[port] = 0;
    byteVlan[port] = 0;
    packVlan[port] = 0;
    byteIpv4[port] = 0;
    packIpv4[port] = 0;
    byteIpv6[port] = 0;
    packIpv6[port] = 0;
    bytePppoe[port] = 0;
    packPppoe[port] = 0;
    byteBridge[port] = 0;
    packBridge[port] = 0;
#ifdef HAVE_POLKA
    bytePolka[port] = 0;
    packPolka[port] = 0;
    byteMpolka[port] = 0;
    packMpolka[port] = 0;
#endif
    byteNsh[port] = 0;
    packNsh[port] = 0;
}


int initTables() {
#ifdef HAVE_POLKA
    table_init(&polkaPoly_table, sizeof(struct polkaPoly_entry), 1);
    table_init(&mpolkaPoly_table, sizeof(struct polkaPoly_entry), 1);
#endif
    table_init(&nsh_table, sizeof(struct nsh_entry), 2);
    table_init(&mpls_table, sizeof(struct mpls_entry), 1);
    table_init(&port2vrf_table, sizeof(struct port2vrf_entry), 1);
    table_init(&vrf2rib4_table, sizeof(struct vrf2rib_entry), 1);
    table_init(&vrf2rib6_table, sizeof(struct vrf2rib_entry), 1);
    table_init(&neigh_table, sizeof(struct neigh_entry), 1);
    table_init(&vlanin_table, sizeof(struct vlanin_entry), 2);
    table_init(&vlanout_table, sizeof(struct vlanout_entry), 1);
    table_init(&bridge_table, sizeof(struct bridge_entry), 3);
    table_init(&acls4_table, sizeof(struct acls_entry), 2);
    table_init(&acls6_table, sizeof(struct acls_entry), 2);
    table_init(&bundle_table, sizeof(struct bundle_entry), 1);
    table_init(&pppoe_table, sizeof(struct pppoe_entry), 2);
    table_init(&policer_table, sizeof(struct policer_entry), 3);
#ifndef HAVE_NOCRYPTO
    printf("openssl version: %s\n", OpenSSL_version(OPENSSL_VERSION));
#if OPENSSL_VERSION_NUMBER >= 0x30000000L
    if (OSSL_PROVIDER_load(NULL, "legacy") == NULL) return 1;
    if (OSSL_PROVIDER_load(NULL, "default") == NULL) return 1;
#endif
    OpenSSL_add_all_ciphers();
    OpenSSL_add_all_digests();
    OpenSSL_add_all_algorithms();
    RAND_poll();
#endif
    return 0;
}
