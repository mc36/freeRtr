printf("opening interface %s\n", ifaceName[o]);
if ((ifaceSock[o] = socket(PF_PACKET, SOCK_RAW, htons(ETH_P_ALL))) < 0) err("unable to open socket");
struct ifreq ifr;
memset(&ifr, 0, sizeof (ifr));
strcpy(ifr.ifr_name, ifaceName[o]);
if (ioctl(ifaceSock[o], SIOCGIFINDEX, &ifr) < 0) {
    if (o < (dataPorts-1)) err("unable to get ifcidx");
    dataPorts--;
    break;
}
ifaceIndex[o] = ifr.ifr_ifindex;
memset(&addrIfc[o], 0, sizeof (addrIfc[o]));
addrIfc[o].sll_family = AF_PACKET;
addrIfc[o].sll_ifindex = ifaceIndex[o];
addrIfc[o].sll_protocol = htons(ETH_P_ALL);
if (bind(ifaceSock[o], (struct sockaddr *) &addrIfc[o], sizeof (addrIfc[o])) < 0) err("failed to bind socket");
addrIfc[o].sll_pkttype = PACKET_OUTGOING;
struct packet_mreq pmr;
memset(&pmr, 0, sizeof (pmr));
pmr.mr_ifindex = ifaceIndex[o];
pmr.mr_type = PACKET_MR_PROMISC;
if (setsockopt(ifaceSock[o], SOL_PACKET, PACKET_ADD_MEMBERSHIP, &pmr, sizeof (pmr)) < 0) err("failed to set promisc");
int val = 1;
if (setsockopt(ifaceSock[o], SOL_PACKET, PACKET_AUXDATA, &val, sizeof(val)) < 0) err("failed to set auxdata");
int sockOpt = 524288;
setsockopt(sockets[i], SOL_SOCKET, SO_RCVBUF, &sockOpt, sizeof(sockOpt));
setsockopt(sockets[i], SOL_SOCKET, SO_SNDBUF, &sockOpt, sizeof(sockOpt));
ifaceId[o] = o;
