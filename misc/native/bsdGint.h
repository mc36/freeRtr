struct bpf_hdr {
    long bt_sec;
    long bt_frac;
    int bh_caplen;
    int bh_datalen;
    short bh_hdrlen;
};
#define BIOCSBLEN       _IOWR('B', 102, u_int)
#define BIOCSETIF       _IOW('B', 108, struct ifreq)
#define BIOCIMMEDIATE   _IOW('B', 112, u_int)
#define BIOCPROMISC     _IO('B', 105)
#define BPF_D_IN        0
