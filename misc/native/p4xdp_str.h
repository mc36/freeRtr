#define MAX_PORTS 128
#define MAX_ROUTES4 2048
#define MAX_ROUTES6 2048
#define MAX_NEIGHS 512
#define MAX_LABELS 1024

struct port_entry {
    int idx;
    long bytes;
    long packs;
};

struct vrfp_entry {
    int cmd; // 1=route
    int vrf;
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
};

struct neigh_res {
    unsigned char dmac[6];
    unsigned char smac[6];
    int port;
};

struct label_res {
    int cmd; // 1=route, 2=pop, 3=swap
    int ver;
    int hop;
    int vrf;
    int swap;
};
