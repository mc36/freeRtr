#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <time.h>
#include <linux/if.h>
#include <linux/if_link.h>
#include <linux/rtnetlink.h>
#include <libmnl/libmnl.h>


void str2mac(char *dst, char *src) {
    sscanf(src, "%hhx:%hhx:%hhx:%hhx:%hhx:%hhx", &dst[0], &dst[1], &dst[2], &dst[3], &dst[4], &dst[5]);
}

void err(char*buf) {
    printf("%s\n", buf);
    _exit(1);
}


int main(int argc, char *argv[]) {
    struct mnl_socket *nl;
    char buf1[MNL_SOCKET_BUFFER_SIZE];
    char buf2[MNL_SOCKET_BUFFER_SIZE];
    struct nlmsghdr *nlh1;
    struct nlmsghdr *nlh2;
    struct nlattr *nla;
    struct ifinfomsg *ifm;
    int ret;
    unsigned int seq, portid;

    if (argc < 3) err("using: veth <iface1> <iface2> [mtu1] [mtu2] [addr1] [addr2]");

    printf("creating veth pair %s - %s\n", argv[1], argv[2]);

    nl = mnl_socket_open(NETLINK_ROUTE);
    if (nl == NULL) err("error opening socket");
    if (mnl_socket_bind(nl, 0, MNL_SOCKET_AUTOPID) < 0) err("error binding socket");
    portid = mnl_socket_get_portid(nl);

    nlh2 = mnl_nlmsg_put_header(buf2);
    nla = mnl_attr_nest_start(nlh2, 1);
    mnl_nlmsg_put_extra_header(nlh2, sizeof(*ifm));
    mnl_attr_put_str(nlh2, IFLA_IFNAME, argv[2]);
    mnl_attr_nest_end(nlh2, nla);

    seq = time(NULL);
    nlh1 = mnl_nlmsg_put_header(buf1);
    nlh1->nlmsg_type = RTM_NEWLINK;
    nlh1->nlmsg_flags = NLM_F_REQUEST | NLM_F_CREATE | NLM_F_EXCL | NLM_F_ACK;
    nlh1->nlmsg_seq = seq;
    mnl_nlmsg_put_extra_header(nlh1, sizeof(*ifm));
    mnl_attr_put_str(nlh1, IFLA_IFNAME, argv[1]);

    nla = mnl_attr_nest_start(nlh1, IFLA_LINKINFO);
    mnl_attr_put_str(nlh1, IFLA_INFO_KIND, "veth");
    mnl_attr_put(nlh1, IFLA_INFO_DATA, mnl_nlmsg_get_payload_len(nlh2), mnl_nlmsg_get_payload(nlh2));
    mnl_attr_nest_end(nlh1, nla);

    if (mnl_socket_sendto(nl, nlh1, nlh1->nlmsg_len) < 0) err("error sending");
    ret = mnl_socket_recvfrom(nl, buf1, sizeof(buf1));
    if (ret == -1) err("error receiving");
    if (mnl_cb_run(buf1, ret, seq, portid, NULL, NULL) == -1) err("error running");

    seq++;
    nlh1 = mnl_nlmsg_put_header(buf1);
    nlh1->nlmsg_type = RTM_NEWLINK;
    nlh1->nlmsg_flags = NLM_F_REQUEST | NLM_F_ACK;
    nlh1->nlmsg_seq = seq;
    ifm = mnl_nlmsg_put_extra_header(nlh1, sizeof(*ifm));
    ifm->ifi_change = IFF_UP;
    ifm->ifi_flags = IFF_UP;
    mnl_attr_put_str(nlh1, IFLA_IFNAME, argv[1]);
    if (argc > 3) mnl_attr_put_u32(nlh1, IFLA_MTU, atoi(argv[3]));
    if (argc > 5) {
        str2mac(&buf2[0], argv[5]);
        mnl_attr_put(nlh1, IFLA_ADDRESS, 6, &buf2);
    }

    if (mnl_socket_sendto(nl, nlh1, nlh1->nlmsg_len) < 0) err("error sending");
    ret = mnl_socket_recvfrom(nl, buf1, sizeof(buf1));
    if (ret == -1) err("error receiving");
    if (mnl_cb_run(buf1, ret, seq, portid, NULL, NULL) == -1) err("error running");

    seq++;
    nlh1 = mnl_nlmsg_put_header(buf1);
    nlh1->nlmsg_type = RTM_NEWLINK;
    nlh1->nlmsg_flags = NLM_F_REQUEST | NLM_F_ACK;
    nlh1->nlmsg_seq = seq;
    ifm = mnl_nlmsg_put_extra_header(nlh1, sizeof(*ifm));
    ifm->ifi_change = IFF_UP;
    ifm->ifi_flags = IFF_UP;
    mnl_attr_put_str(nlh1, IFLA_IFNAME, argv[2]);
    if (argc > 4) mnl_attr_put_u32(nlh1, IFLA_MTU, atoi(argv[4]));
    if (argc > 6) {
        str2mac(&buf2[0], argv[6]);
        mnl_attr_put(nlh1, IFLA_ADDRESS, 6, &buf2);
    }

    if (mnl_socket_sendto(nl, nlh1, nlh1->nlmsg_len) < 0) err("error sending");
    ret = mnl_socket_recvfrom(nl, buf1, sizeof(buf1));
    if (ret == -1) err("error receiving");
    if (mnl_cb_run(buf1, ret, seq, portid, NULL, NULL) == -1) err("error running");

    printf("done\n");

    mnl_socket_close(nl);
    return 0;
}
