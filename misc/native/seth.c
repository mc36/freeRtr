#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <linux/if.h>
#include <linux/if_link.h>
#include <linux/ethtool.h>
#include <linux/sockios.h>
#include <sys/ioctl.h>



void err(char*buf) {
    printf("%s\n", buf);
    _exit(1);
}

void str2mac(unsigned char *dst, char *src) {
    sscanf(src, "%hhx:%hhx:%hhx:%hhx:%hhx:%hhx", &dst[0], &dst[1], &dst[2], &dst[3], &dst[4], &dst[5]);
}

void mac2str(unsigned char *src, unsigned char *dst) {
    snprintf((char*)dst, 128, "%02x:%02x:%02x:%02x:%02x:%02x", src[0], src[1], src[2], src[3], src[4], src[5]);
}


struct one_feature {
    char *nam;
    int get;
    int set;
};

struct one_flag {
    char *nam;
    int val;
};

struct one_feature all_features[] = {
    {"rx", ETHTOOL_GRXCSUM, ETHTOOL_SRXCSUM },
    {"tx", ETHTOOL_GTXCSUM, ETHTOOL_STXCSUM },
    {"sg", ETHTOOL_GSG, ETHTOOL_SSG },
    {"tso", ETHTOOL_GTSO, ETHTOOL_STSO },
    {"ufo", ETHTOOL_GUFO, ETHTOOL_SUFO },
    {"gso", ETHTOOL_GGSO, ETHTOOL_SGSO },
    {"gro", ETHTOOL_GGRO, ETHTOOL_SGRO },
};

struct one_flag all_flags[] = {
    {"up", IFF_UP },
    {"run", IFF_RUNNING },
    {"bcast", IFF_BROADCAST },
    {"mcast", IFF_MULTICAST },
    {"allmc", IFF_ALLMULTI },
    {"prmsc", IFF_PROMISC },
};




int main(int argc, char *argv[]) {
    int fd;
    struct ifreq ifr;
    struct ethtool_value eval;
    unsigned char buf[1024];

    if (argc < 2) err("using: seth <iface> [mtu] [mac]");

    printf("setting up %s\n", argv[1]);

    fd = socket(AF_NETLINK, SOCK_RAW, NETLINK_GENERIC);
    if (fd < 0) err("cannot get socket");

    memset(&ifr, 0, sizeof(ifr));
    strcpy(ifr.ifr_name, argv[1]);
    ifr.ifr_data = &eval;

    printf("features:");
    for (int i = 0; i < sizeof(all_features) / sizeof(struct one_feature); i++) {
        eval.cmd = all_features[i].set;
        eval.data = 0;
        ioctl(fd, SIOCETHTOOL, &ifr);
        eval.cmd = all_features[i].get;
        if (ioctl(fd, SIOCETHTOOL, &ifr) < 0) continue;
        printf(" %s=%i", all_features[i].nam, eval.data);
    }

    ioctl(fd, SIOCGIFFLAGS, &ifr);
    for (int i = 0; i < sizeof(all_flags) / sizeof(struct one_flag); i++) {
        ifr.ifr_flags |= all_flags[i].val;
    }
    ioctl(fd, SIOCSIFFLAGS, &ifr);
    ioctl(fd, SIOCGIFFLAGS, &ifr);
    printf("\nflags:");
    for (int i = 0; i < sizeof(all_flags) / sizeof(struct one_flag); i++) {
        int o = ifr.ifr_flags & all_flags[i].val;
        if (o != 0) o = 1;
        printf(" %s=%i", all_flags[i].nam, o);
    }

    if (argc > 2) {
        ifr.ifr_mtu = atoi(argv[2]);
        ioctl(fd, SIOCSIFMTU, &ifr);
    }
    ioctl(fd, SIOCGIFMTU, &ifr);
    printf("\nmtu=%i", ifr.ifr_mtu);

    if (argc > 3) {
        str2mac(buf, argv[3]);
        memcpy(&ifr.ifr_hwaddr.sa_data, &buf, 6);
        ifr.ifr_hwaddr.sa_family = AF_LOCAL;
        ioctl(fd, SIOCSIFHWADDR, &ifr);
    }

    ioctl(fd, SIOCGIFHWADDR, &ifr);
    mac2str((unsigned char*)&ifr.ifr_hwaddr.sa_data, buf);
    printf(" mac=%s\n", (char*)&buf);

}
