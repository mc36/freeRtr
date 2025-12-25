#define maxPorts 128

struct port_res {
    __u32 idx;
    __u64 byte;
    __u64 pack;
};

struct vrfp_res {
    __u32 cmd; // 1=route, 2=bridge, 3=xconn, 4=loconnifc, 5=loconnnei, 6=nshconn
    __u32 vrf;
    __u32 bridge;
    __u32 nexthop;
    __u32 label1;
    __u32 label2;
    __u32 pttl4;
    __u32 pttl6;
    __u32 mpls;
    __u32 nsh;
    __s32 sgtSet;
    __u32 sgtTag;
    __u32 verify4;
    __u32 verify6;
    __u32 pmtud4;
    __u32 pmtud6;
    __u64 packRx;
    __u64 byteRx;
    __u64 packTx;
    __u64 byteTx;
};

#define routes_bits (sizeof(__u32) * 8)

struct route4_key {
    __u32 bits;
    __u32 vrf;
    __u8 addr[4];
};

struct route6_key {
    __u32 bits;
    __u32 vrf;
    __u8 addr[16];
};

struct routes_res {
    __u32 cmd; // 1=route, 2=cpu, 3=mpls1, 4=mpls2, 5=drop, 6=polka
    __u32 nexthop;
    __u32 label1;
    __u32 label2;
    __u8 polka[16];
    __u64 pack;
    __u64 byte;
};

struct neigh_res {
    __u32 cmd; // 1=rawip, 2=pppoe, 3=gre, 4=l2tp, 5=l3tp, 6=gtp, 7=pwhe, 8=labels
    __u32 cmd2; // 1=ipv4, 2=ipv6
    __u8 macs[12];
    __u8 mac2[12];
    __u32 port;
    __u32 aclport;
    __u32 sess;
    __u8 srcAddr[16];
    __u8 trgAddr[16];
    __u32 srcPort;
    __u32 trgPort;
    __u64 pack;
    __u64 byte;
};

struct label_res {
    __u32 cmd; // 1=route, 2=pop, 3=swap, 4=xconn, 5=vpls, 6=push, 7=punt, 8=pwhe
    __u32 ver;
    __u32 nexthop;
    __u32 vrf;
    __u32 swap;
    __u32 push;
    __u32 port;
    __u64 pack;
    __u64 byte;
};

struct bundle_res {
    __u32 cmd; // 1=bundle, 2=hairpin
    __u32 out[16];
    __u64 pack;
    __u64 byte;
};

struct vlan_key {
    __u32 port;
    __u32 vlan;
};

struct vlan_res {
    __u32 port;
    __u32 port2;
    __u32 vlan;
    __u32 vlan2;
    __u64 pack;
    __u64 byte;
};

struct pppoe_key {
    __u32 port;
    __u32 sess;
};

struct bridge_key {
    __u32 id;
    __u8 mac[8];
};

struct bridge_res {
    __u32 cmd; // 1=port, 2=vpls, 3=route, 4=pckoudp, 5=vxlan
    __u32 cmd2; // 1=ipv4, 2=ipv6
    __u32 port;
    __u32 nexthop;
    __u32 label1;
    __u32 label2;
    __u8 srcAddr[16];
    __u8 trgAddr[16];
    __u32 srcPort;
    __u32 trgPort;
    __u64 packRx;
    __u64 byteRx;
    __u64 packTx;
    __u64 byteTx;
};

struct tunnel4_key {
    __u32 vrf;
    __u8 srcAddr[4];
    __u8 trgAddr[4];
    __u32 prot;
    __u32 srcPort;
    __u32 trgPort;
};

struct tunnel6_key {
    __u32 vrf;
    __u8 srcAddr[16];
    __u8 trgAddr[16];
    __u32 prot;
    __u32 srcPort;
    __u32 trgPort;
};

struct tunnel_res {
    __u32 cmd; // 1=gre, 2=l2tp, 3=l3tp, 4=gtp, 5=pckoudp, 6=vxlan
    __u32 aclport;
    __u64 pack;
    __u64 byte;
};

struct nsh_key {
    __u32 sp;
    __u32 si;
};

struct nsh_res {
    __u32 cmd; //  1=fwd, 2=vrf, 3=nei
    int port;
    int vrf;
    int trg;
    unsigned char macs[12];
    long pack;
    long byte;
};

struct polpol_res {
    int tab[256];
    long pack;
    long byte;
};

struct polidx_key {
    int vrf;
    int idx;
};
