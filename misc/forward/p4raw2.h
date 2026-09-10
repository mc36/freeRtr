struct ifreq ifr;
memset(&ifr, 0, sizeof (ifr));
strcpy(ifr.ifr_name, ifaceName[port]);
if (ioctl(ifaceSock[port], SIOCGIFFLAGS, &ifr) < 0) return 0;
int needed = IFF_RUNNING | IFF_UP;
if ((ifr.ifr_flags & needed) == needed) return 1;
return 0;
