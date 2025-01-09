int printCmds = 0;

void str2mac(unsigned char *dst, char *src) {
    sscanf(src, "%hhx:%hhx:%hhx:%hhx:%hhx:%hhx", &dst[0], &dst[1], &dst[2], &dst[3], &dst[4], &dst[5]);
}


void doStatRound(FILE *commands, int round) {
    if ((round % 10) != 0) return;
    __u32 i = -1;
    __u32 o = 0;
    for (;;) {
        if (bpf_map_get_next_key(tx_ports_fd, &i, &o) != 0) break;
        i = o;
        fprintf(commands, "state %i 1\r\n", i);
    }
    fflush(commands);
}


void addattr_l(struct nlmsghdr *n, int t, unsigned char *d, int s) {
    int l = RTA_LENGTH(s);
    struct rtattr *rta;
    rta = ((struct rtattr *) (((void *) n) + NLMSG_ALIGN(n->nlmsg_len)));
    rta->rta_type = t;
    rta->rta_len = l;
    memcpy(RTA_DATA(rta), d, s);
    n->nlmsg_len = NLMSG_ALIGN(n->nlmsg_len) + RTA_ALIGN(l);
}


int doOneCommand(unsigned char* buf) {
    unsigned char buf2[1024];
    char* arg[128];
    int cnt;
    cnt = 0;
    arg[0] = (char*)&buf[0];
    __u32 i = 0;
    __u32 o = 0;
    for (;;) {
        if (cnt >= 128) break;
        switch (buf[i]) {
        case 0:
        case 10:
        case 13:
            o = 1;
            break;
        case ' ':
        case '/':
        case '_':
            buf[i] = 0;
            cnt++;
            arg[cnt] = (char*)&buf[i + 1];
            break;
        }
        if (o > 0) break;
        i++;
    }
    o = i;
    cnt++;
    buf[o] = 0;
    for (int i=cnt; i < 128; i++) arg[i]=(char*)&buf[o];
    if (printCmds != 0) {
        printf("rx: ");
        for (int i=0; i < cnt; i++) printf("'%s' ",arg[i]);
        printf("\n");
    }
    int del = strcmp(arg[1], "del");
    if (del != 0) del = 1;
    unsigned char nlb[MNL_SOCKET_BUFFER_SIZE];
    struct nlmsghdr *nlh;
    nlh = mnl_nlmsg_put_header(nlb);
    ssize_t sizt;
    mnlSequence++;
    if (strcmp(arg[0], "myaddr4") == 0) {
        int p = atoi(arg[4]);
        if (p < 0) return 0;
        inet_pton(AF_INET, arg[2], buf2);
        if (del == 0) {
            nlh->nlmsg_type = RTM_DELADDR;
            nlh->nlmsg_flags = NLM_F_REQUEST | NLM_F_ACK;
        } else {
            nlh->nlmsg_type = RTM_NEWADDR;
            nlh->nlmsg_flags = NLM_F_REQUEST | NLM_F_CREATE | NLM_F_REPLACE | NLM_F_ACK;
        }
        nlh->nlmsg_seq = mnlSequence;
        struct ifaddrmsg *ifm = mnl_nlmsg_put_extra_header(nlh, sizeof(struct ifaddrmsg));
        ifm->ifa_family = AF_INET;
        ifm->ifa_prefixlen = atoi(arg[3]);
        ifm->ifa_flags = IFA_F_PERMANENT;
        ifm->ifa_scope = RT_SCOPE_UNIVERSE;
        ifm->ifa_index = ifaceId[p];
        mnl_attr_put(nlh, IFA_LOCAL, sizeof(struct in_addr), &buf2);
        mnl_attr_put(nlh, IFA_ADDRESS, sizeof(struct in_addr), &buf2);
        if (mnl_socket_sendto(mnlSocket, nlh, nlh->nlmsg_len) < 0) warn("error sending");
        sizt = mnl_socket_recvfrom(mnlSocket, nlb, sizeof(nlb));
        if (sizt < 0) warn("error receiving");
        if (mnl_cb_run(nlb, sizt, mnlSequence, mnlPortid, NULL, NULL) < 0) warn("error running");
        return 0;
    }
    if (strcmp(arg[0], "myaddr6") == 0) {
        int p = atoi(arg[4]);
        if (p < 0) return 0;
        inet_pton(AF_INET6, arg[2], buf2);
        if (del == 0) {
            nlh->nlmsg_type = RTM_DELADDR;
            nlh->nlmsg_flags = NLM_F_REQUEST | NLM_F_ACK;
        } else {
            nlh->nlmsg_type = RTM_NEWADDR;
            nlh->nlmsg_flags = NLM_F_REQUEST | NLM_F_CREATE | NLM_F_REPLACE | NLM_F_ACK;
        }
        nlh->nlmsg_seq = mnlSequence;
        struct ifaddrmsg *ifm = mnl_nlmsg_put_extra_header(nlh, sizeof(struct ifaddrmsg));
        ifm->ifa_family = AF_INET6;
        ifm->ifa_prefixlen = atoi(arg[3]);
        ifm->ifa_flags = IFA_F_PERMANENT;
        ifm->ifa_scope = RT_SCOPE_UNIVERSE;
        ifm->ifa_index = ifaceId[p];
        mnl_attr_put(nlh, IFA_ADDRESS, sizeof(struct in6_addr), &buf2);
        if (mnl_socket_sendto(mnlSocket, nlh, nlh->nlmsg_len) < 0) warn("error sending");
        sizt = mnl_socket_recvfrom(mnlSocket, nlb, sizeof(nlb));
        if (sizt < 0) warn("error receiving");
        if (mnl_cb_run(nlb, sizt, mnlSequence, mnlPortid, NULL, NULL) < 0) warn("error running");
        return 0;
    }
    if (strcmp(arg[0], "neigh4") == 0) {
        int p = atoi(arg[7]);
        inet_pton(AF_INET, arg[3], buf2);
        if (del == 0) {
            nlh->nlmsg_type = RTM_DELNEIGH;
            nlh->nlmsg_flags = NLM_F_REQUEST | NLM_F_ACK;
        } else {
            nlh->nlmsg_type = RTM_NEWNEIGH;
            nlh->nlmsg_flags = NLM_F_REQUEST | NLM_F_CREATE | NLM_F_REPLACE | NLM_F_ACK;
        }
        nlh->nlmsg_seq = mnlSequence;
        struct ndmsg *ndm = mnl_nlmsg_put_extra_header(nlh, sizeof(struct ndmsg));
        ndm->ndm_family = AF_INET;
        ndm->ndm_state = NUD_PERMANENT;
        ndm->ndm_ifindex = ifaceId[p];
        addattr_l(nlh, NDA_DST, buf2, sizeof(struct in_addr));
        str2mac(buf2, arg[4]);
        addattr_l(nlh, NDA_LLADDR, buf2, 6);
        if (mnl_socket_sendto(mnlSocket, nlh, nlh->nlmsg_len) < 0) warn("error sending");
        sizt = mnl_socket_recvfrom(mnlSocket, nlb, sizeof(nlb));
        if (sizt < 0) warn("error receiving");
        if (mnl_cb_run(nlb, sizt, mnlSequence, mnlPortid, NULL, NULL) < 0) warn("error running");
        return 0;
    }
    if (strcmp(arg[0], "neigh6") == 0) {
        int p = atoi(arg[7]);
        inet_pton(AF_INET6, arg[3], buf2);
        if (del == 0) {
            nlh->nlmsg_type = RTM_DELNEIGH;
            nlh->nlmsg_flags = NLM_F_REQUEST | NLM_F_ACK;
        } else {
            nlh->nlmsg_type = RTM_NEWNEIGH;
            nlh->nlmsg_flags = NLM_F_REQUEST | NLM_F_CREATE | NLM_F_REPLACE | NLM_F_ACK;
        }
        nlh->nlmsg_seq = mnlSequence;
        struct ndmsg *ndm = mnl_nlmsg_put_extra_header(nlh, sizeof(struct ndmsg));
        ndm->ndm_family = AF_INET6;
        ndm->ndm_state = NUD_PERMANENT;
        ndm->ndm_ifindex = ifaceId[p];
        addattr_l(nlh, NDA_DST, buf2, sizeof(struct in6_addr));
        str2mac(buf2, arg[4]);
        addattr_l(nlh, NDA_LLADDR, buf2, 6);
        if (mnl_socket_sendto(mnlSocket, nlh, nlh->nlmsg_len) < 0) warn("error sending");
        sizt = mnl_socket_recvfrom(mnlSocket, nlb, sizeof(nlb));
        if (sizt < 0) warn("error receiving");
        if (mnl_cb_run(nlb, sizt, mnlSequence, mnlPortid, NULL, NULL) < 0) warn("error running");
        return 0;
    }
    if (strcmp(arg[0], "route4") == 0) {
        int p = atoi(arg[7]);
        inet_pton(AF_INET, arg[2], buf2);
        if (del == 0) {
            nlh->nlmsg_type = RTM_DELROUTE;
            nlh->nlmsg_flags = NLM_F_REQUEST | NLM_F_ACK;
        } else {
            nlh->nlmsg_type = RTM_NEWROUTE;
            nlh->nlmsg_flags = NLM_F_REQUEST | NLM_F_CREATE | NLM_F_REPLACE | NLM_F_ACK;
        }
        nlh->nlmsg_seq = mnlSequence;
        struct rtmsg *rtm = mnl_nlmsg_put_extra_header(nlh, sizeof(struct rtmsg));
        rtm->rtm_family = AF_INET;
        rtm->rtm_dst_len = atoi(arg[3]);
        rtm->rtm_src_len = 0;
        rtm->rtm_tos = 0;
        rtm->rtm_protocol = RTPROT_STATIC;
        rtm->rtm_table = RT_TABLE_MAIN;
        rtm->rtm_type = RTN_UNICAST;
        rtm->rtm_scope = RT_SCOPE_UNIVERSE;
        rtm->rtm_flags = 0;
        mnl_attr_put(nlh, RTA_DST, sizeof(struct in_addr), &buf2);
        mnl_attr_put_u32(nlh, RTA_OIF, ifaceId[p]);
        inet_pton(AF_INET, arg[5], buf2);
        mnl_attr_put(nlh, RTA_GATEWAY, sizeof(struct in_addr), &buf2);
        if (mnl_socket_sendto(mnlSocket, nlh, nlh->nlmsg_len) < 0) warn("error sending");
        sizt = mnl_socket_recvfrom(mnlSocket, nlb, sizeof(nlb));
        if (sizt < 0) warn("error receiving");
        if (mnl_cb_run(nlb, sizt, mnlSequence, mnlPortid, NULL, NULL) < 0) warn("error running");
        return 0;
    }
    if (strcmp(arg[0], "route6") == 0) {
        int p = atoi(arg[7]);
        inet_pton(AF_INET6, arg[2], buf2);
        if (del == 0) {
            nlh->nlmsg_type = RTM_DELROUTE;
            nlh->nlmsg_flags = NLM_F_REQUEST | NLM_F_ACK;
        } else {
            nlh->nlmsg_type = RTM_NEWROUTE;
            nlh->nlmsg_flags = NLM_F_REQUEST | NLM_F_CREATE | NLM_F_REPLACE | NLM_F_ACK;
        }
        nlh->nlmsg_seq = mnlSequence;
        struct rtmsg *rtm = mnl_nlmsg_put_extra_header(nlh, sizeof(struct rtmsg));
        rtm->rtm_family = AF_INET6;
        rtm->rtm_dst_len = atoi(arg[3]);
        rtm->rtm_src_len = 0;
        rtm->rtm_tos = 0;
        rtm->rtm_protocol = RTPROT_STATIC;
        rtm->rtm_table = RT_TABLE_MAIN;
        rtm->rtm_type = RTN_UNICAST;
        rtm->rtm_scope = RT_SCOPE_UNIVERSE;
        rtm->rtm_flags = 0;
        mnl_attr_put(nlh, RTA_DST, sizeof(struct in6_addr), &buf2);
        mnl_attr_put_u32(nlh, RTA_OIF, ifaceId[p]);
        inet_pton(AF_INET6, arg[5], buf2);
        mnl_attr_put(nlh, RTA_GATEWAY, sizeof(struct in6_addr), &buf2);
        if (mnl_socket_sendto(mnlSocket, nlh, nlh->nlmsg_len) < 0) warn("error sending");
        sizt = mnl_socket_recvfrom(mnlSocket, nlb, sizeof(nlb));
        if (sizt < 0) warn("error receiving");
        if (mnl_cb_run(nlb, sizt, mnlSequence, mnlPortid, NULL, NULL) < 0) warn("error running");
        return 0;
    }
    return 0;
}
