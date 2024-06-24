#define maxPorts 128

struct port_res {
    __u32 idx;
    __u64 byte;
    __u64 pack;
};

struct vrfp_res {
    __u32 cmd; // 1=route, 2=bridge, 3=xconn, 4=loconnifc, 5=loconnnei
    __u32 vrf;
    __u32 brdg;
    __u32 hop;
    __u32 label1;
    __u32 label2;
    __u32 pttl4;
    __u32 pttl6;
    __u32 mpls;
    __s32 sgtSet;
    __u32 sgtTag;
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
    __u32 cmd; // 1=route, 2=cpu, 3=mpls1, 4=mpls2, 5=drop
    __u32 hop;
    __u32 label1;
    __u32 label2;
    __u64 pack;
    __u64 byte;
};

struct neigh_res {
    __u32 cmd; // 1=rawip, 2=pppoe, 3=gre4, 4=gre6
    __u8 macs[12];
    __u32 port;
    __u32 aclport;
    __u32 sess;
    __u8 srcAddr[16];
    __u8 trgAddr[16];
    __u32 prot;
    __u32 srcPort;
    __u32 trgPort;
    __u64 pack;
    __u64 byte;
};

struct label_res {
    __u32 cmd; // 1=route, 2=pop, 3=swap, 4=xconn, 5=vpls
    __u32 ver;
    __u32 hop;
    __u32 vrf;
    __u32 swap;
    __u32 brdg;
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
    __u32 vlan;
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
    __u32 cmd; // 1=port, 2=vpls
    __u32 port;
    __u32 hop;
    __u32 label1;
    __u32 label2;
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
    __u32 cmd; // 1=gre
    __u32 aclport;
    __u64 pack;
    __u64 byte;
};
