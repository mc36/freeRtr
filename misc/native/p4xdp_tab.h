#define maxPorts 128

struct port_res {
    int idx;
    long byte;
    long pack;
};

struct vrfp_res {
    int cmd; // 1=route, 2=bridge, 3=xconn
    int vrf;
    int hop;
    int label1;
    int label2;
    long pack;
    long byte;
};

#define routes_bits (sizeof(__u32) * 8)

struct route4_key {
    __u32 bits;
    __u32 vrf;
    unsigned char addr[4];
};

struct route6_key {
    __u32 bits;
    __u32 vrf;
    unsigned char addr[16];
};

struct routes_res {
    int cmd; // 1=route, 2=cpu, 3=mpls1, 4=mpls2
    int hop;
    int label1;
    int label2;
    long pack;
    long byte;
};

struct neigh_res {
    int cmd; // 1=rawip, 2=pppoe
    unsigned char dmac[6];
    unsigned char smac[6];
    int port;
    int sess;
    long pack;
    long byte;
};

struct label_res {
    int cmd; // 1=route, 2=pop, 3=swap, 4=xconn
    int ver;
    int hop;
    int vrf;
    int swap;
    int port;
    long pack;
    long byte;
};

struct bundle_res {
    int out[16];
    long pack;
    long byte;
};

struct vlan_key {
    int port;
    int vlan;
};

struct vlan_res {
    int port;
    int vlan;
    long pack;
    long byte;
};

struct pppoe_key {
    int port;
    int sess;
};
