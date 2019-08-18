package cfg;

import addr.addrIP;
import addr.addrPrefix;
import ip.ipRtrAdv;
import ip.ipRtrRed;
import ip.ipRtr;
import ip.ipRtrAgr;
import ip.ipRtrInt;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import rtr.rtrBabel;
import rtr.rtrBgp;
import rtr.rtrDownload;
import rtr.rtrEigrp;
import rtr.rtrFlowspec;
import rtr.rtrIsis;
import rtr.rtrLogger;
import rtr.rtrMsdp;
import rtr.rtrOspf4;
import rtr.rtrOspf6;
import rtr.rtrPvrp;
import rtr.rtrLsrp;
import rtr.rtrOlsr;
import rtr.rtrRip4;
import rtr.rtrRip6;
import rtr.rtrUni2flow;
import rtr.rtrUni2multi;
import rtr.rtrDeaggr;
import rtr.rtrMobile;
import tab.tabGen;
import tab.tabRouteEntry;
import user.userFilter;
import user.userHelping;
import util.bits;
import util.cmds;

/**
 * router configuration
 *
 * @author matecsaba
 */
public class cfgRtr implements Comparator<cfgRtr>, cfgGeneric {

    /**
     * type of routing process
     */
    public tabRouteEntry.routeType type;

    /**
     * number of routing process
     */
    public int number;

    /**
     * vrf of this router
     */
    public cfgVrf vrf;

    /**
     * rip handler
     */
    public rtrRip4 rip4;

    /**
     * rip handler
     */
    public rtrRip6 rip6;

    /**
     * babel handler
     */
    public rtrBabel babel;

    /**
     * olsr handler
     */
    public rtrOlsr olsr;

    /**
     * ospf handler
     */
    public rtrOspf4 ospf4;

    /**
     * ospf handler
     */
    public rtrOspf6 ospf6;

    /**
     * isis handler
     */
    public rtrIsis isis;

    /**
     * pvrp handler
     */
    public rtrPvrp pvrp;

    /**
     * lsrp handler
     */
    public rtrLsrp lsrp;

    /**
     * eigrp handler
     */
    public rtrEigrp eigrp;

    /**
     * bgp handler
     */
    public rtrBgp bgp;

    /**
     * msdp handler
     */
    public rtrMsdp msdp;

    /**
     * flowspec handler
     */
    public rtrFlowspec flwspc;

    /**
     * unicast to multicast handler
     */
    public rtrUni2multi uni2multi;

    /**
     * unicast to flowspec handler
     */
    public rtrUni2flow uni2flow;

    /**
     * logger handler
     */
    public rtrLogger logger;

    /**
     * downloader handler
     */
    public rtrDownload download;

    /**
     * deaggregate handler
     */
    public rtrDeaggr deaggr;

    /**
     * mobile handler
     */
    public rtrMobile mobile;

    /**
     * state of this process
     */
    public boolean running;

    /**
     * defaults text
     */
    public final static String defaultL[] = {
        // router *
        "router .*! no automesh",
        // router pvrp
        "router pvrp[4|6] .*! no labels",
        // router lsrp
        "router lsrp[4|6] .*! distance 70",
        "router lsrp[4|6] .*! no default-originate",
        "router lsrp[4|6] .*! no prefix-list",
        "router lsrp[4|6] .*! no route-map",
        "router lsrp[4|6] .*! no route-policy",
        "router lsrp[4|6] .*! no segrout",
        "router lsrp[4|6] .*! no bier",
        "router lsrp[4|6] .*! refresh 2400000",
        "router lsrp[4|6] .*! lifetime 3600000",
        // router eigrp
        "router eigrp[4|6] .*! kvals 1 0 1 0 0",
        "router eigrp[4|6] .*! stub",
        // router ospf
        "router ospf[4|6] .*! distance 110 110 110",
        "router ospf[4|6] .*! no segrout",
        "router ospf[4|6] .*! no bier",
        "router ospf[4|6] .*! no area .* stub",
        "router ospf[4|6] .*! no area .* nssa",
        "router ospf[4|6] .*! no area .* traffeng",
        "router ospf[4|6] .*! no area .* segrout",
        "router ospf[4|6] .*! no area .* bier",
        "router ospf[4|6] .*! no area .* hostname",
        "router ospf[4|6] .*! no area .* default-originate",
        "router ospf[4|6] .*! no area .* prefix-list-from",
        "router ospf[4|6] .*! no area .* prefix-list-into",
        "router ospf[4|6] .*! no area .* route-map-from",
        "router ospf[4|6] .*! no area .* route-map-into",
        "router ospf[4|6] .*! no area .* route-policy-from",
        "router ospf[4|6] .*! no area .* route-policy-into",
        // router isis
        "router isis[4|6] .*! max-area-addrs 3",
        "router isis[4|6] .*! distance 115 115",
        "router isis[4|6] .*! metric-wide",
        "router isis[4|6] .*! no multi-topology",
        "router isis[4|6] .*! no segrout",
        "router isis[4|6] .*! no bier",
        "router isis[4|6] .*! level2 clear-attached",
        "router isis[4|6] .*! no level2 allow-attached",
        "router isis[4|6] .*! no level1 clear-attached",
        "router isis[4|6] .*! level1 allow-attached",
        "router isis[4|6] .*! no level[1|2] set-overload",
        "router isis[4|6] .*! no level[1|2] set-attached",
        "router isis[4|6] .*! no level[1|2] traffeng",
        "router isis[4|6] .*! no level[1|2] segrout",
        "router isis[4|6] .*! no level[1|2] bier",
        "router isis[4|6] .*! level[1|2] hostname",
        "router isis[4|6] .*! level[1|2] inter-level",
        "router isis[4|6] .*! no level[1|2] default-originate",
        "router isis[4|6] .*! level[1|2] lsp-mtu 1024",
        "router isis[4|6] .*! level[1|2] lsp-refresh 400000",
        "router isis[4|6] .*! level[1|2] lsp-lifetime 1200000",
        "router isis[4|6] .*! no level[1|2] prefix-list-from",
        "router isis[4|6] .*! no level[1|2] prefix-list-into",
        "router isis[4|6] .*! no level[1|2] route-map-from",
        "router isis[4|6] .*! no level[1|2] route-map-into",
        "router isis[4|6] .*! no level[1|2] route-policy-from",
        "router isis[4|6] .*! no level[1|2] route-policy-into",
        // router bgp
        "router bgp[4|6].*! distance 20 200 200",
        "router bgp[4|6].*! scantime 1000",
        "router bgp[4|6].*! scandelay 1000",
        "router bgp[4|6].*! incremental 1000",
        "router bgp[4|6].*! no conquer",
        "router bgp[4|6].*! no flapstat",
        "router bgp[4|6].*! no flowspec",
        "router bgp[4|6].*! no segrout",
        "router bgp[4|6].*! no bier",
        "router bgp[4|6].*! graceful-restart 60000",
        "router bgp[4|6].*! no listen",
        "router bgp[4|6].*! no nexthop route-map",
        "router bgp[4|6].*! no nexthop route-policy",
        "router bgp[4|6].*! no nexthop prefix-list",
        "router bgp[4|6].*! no neighbor .* template",
        "router bgp[4|6].*! neighbor .* dmz-link-bw -1",
        "router bgp[4|6].*! neighbor .* timer 60000 180000",
        "router bgp[4|6].*! neighbor .* connection-mode both",
        "router bgp[4|6].*! neighbor .* buffer-size 65536",
        "router bgp[4|6].*! neighbor .* ttl-security -1",
        "router bgp[4|6].*! neighbor .* additional-path-rx",
        "router bgp[4|6].*! neighbor .* additional-path-tx",
        "router bgp[4|6].*! no neighbor .* password",
        "router bgp[4|6].*! no neighbor .* shutdown",
        "router bgp[4|6].*! no neighbor .* update-source",
        "router bgp[4|6].*! no neighbor .* compression",
        "router bgp[4|6].*! no neighbor .* bfd",
        "router bgp[4|6].*! no neighbor .* fall-over",
        "router bgp[4|6].*! no neighbor .* hostname",
        "router bgp[4|6].*! no neighbor .* unidirection",
        "router bgp[4|6].*! neighbor .* graceful-restart",
        "router bgp[4|6].*! neighbor .* capability-negotiation",
        "router bgp[4|6].*! no neighbor .* aigp",
        "router bgp[4|6].*! no neighbor .* traffeng",
        "router bgp[4|6].*! no neighbor .* pmsitun",
        "router bgp[4|6].*! no neighbor .* tunenc",
        "router bgp[4|6].*! no neighbor .* attribset",
        "router bgp[4|6].*! no neighbor .* segrout",
        "router bgp[4|6].*! no neighbor .* bier",
        "router bgp[4|6].*! no neighbor .* egress-engineering",
        "router bgp[4|6].*! no neighbor .* monitor",
        "router bgp[4|6].*! no neighbor .* dump",
        "router bgp[4|6].*! no neighbor .* default-originate",
        "router bgp[4|6].*! no neighbor .* internal-vpn-client",
        "router bgp[4|6].*! no neighbor .* allow-as-in",
        "router bgp[4|6].*! neighbor .* allow-as-out",
        "router bgp[4|6].*! no neighbor .* enforce-first-as",
        "router bgp[4|6].*! no neighbor .* route-server-client",
        "router bgp[4|6].*! no neighbor .* soft-reconfiguration",
        "router bgp[4|6].*! no neighbor .* remove-private-as-out",
        "router bgp[4|6].*! no neighbor .* remove-private-as-in",
        "router bgp[4|6].*! no neighbor .* override-peer-as-out",
        "router bgp[4|6].*! no neighbor .* override-peer-as-in",
        "router bgp[4|6].*! no neighbor .* route-reflector-client",
        "router bgp[4|6].*! no neighbor .* confederation-peer",
        "router bgp[4|6].*! no neighbor .* next-hop-unchanged",
        "router bgp[4|6].*! no neighbor .* next-hop-self",
        "router bgp[4|6].*! no neighbor .* next-hop-peer",
        "router bgp[4|6].*! no neighbor .* send-community",
        "router bgp[4|6].*! no neighbor .* maximum-prefix",
        "router bgp[4|6].*! no neighbor .* prefix-list-in",
        "router bgp[4|6].*! no neighbor .* prefix-list-out",
        "router bgp[4|6].*! no neighbor .* route-map-in",
        "router bgp[4|6].*! no neighbor .* route-map-out",
        "router bgp[4|6].*! no neighbor .* route-map-vin",
        "router bgp[4|6].*! no neighbor .* route-map-vout",
        "router bgp[4|6].*! no neighbor .* route-policy-in",
        "router bgp[4|6].*! no neighbor .* route-policy-out",
        "router bgp[4|6].*! no neighbor .* route-policy-vin",
        "router bgp[4|6].*! no neighbor .* route-policy-vout",
        "router bgp[4|6].*! no template .* template",
        "router bgp[4|6].*! template .* dmz-link-bw -1",
        "router bgp[4|6].*! template .* timer 60000 180000",
        "router bgp[4|6].*! template .* connection-mode both",
        "router bgp[4|6].*! template .* buffer-size 65536",
        "router bgp[4|6].*! template .* ttl-security -1",
        "router bgp[4|6].*! template .* additional-path-rx",
        "router bgp[4|6].*! template .* additional-path-tx",
        "router bgp[4|6].*! no template .* password",
        "router bgp[4|6].*! no template .* shutdown",
        "router bgp[4|6].*! no template .* update-source",
        "router bgp[4|6].*! no template .* compression",
        "router bgp[4|6].*! no template .* bfd",
        "router bgp[4|6].*! no template .* fall-over",
        "router bgp[4|6].*! no template .* hostname",
        "router bgp[4|6].*! no template .* unidirection",
        "router bgp[4|6].*! template .* graceful-restart",
        "router bgp[4|6].*! template .* capability-negotiation",
        "router bgp[4|6].*! no template .* aigp",
        "router bgp[4|6].*! no template .* traffeng",
        "router bgp[4|6].*! no template .* pmsitun",
        "router bgp[4|6].*! no template .* tunenc",
        "router bgp[4|6].*! no template .* attribset",
        "router bgp[4|6].*! no template .* segrout",
        "router bgp[4|6].*! no template .* bier",
        "router bgp[4|6].*! no template .* egress-engineering",
        "router bgp[4|6].*! no template .* monitor",
        "router bgp[4|6].*! no template .* dump",
        "router bgp[4|6].*! no template .* default-originate",
        "router bgp[4|6].*! no template .* internal-vpn-client",
        "router bgp[4|6].*! no template .* allow-as-in",
        "router bgp[4|6].*! template .* allow-as-out",
        "router bgp[4|6].*! no template .* enforce-first-as",
        "router bgp[4|6].*! no template .* route-server-client",
        "router bgp[4|6].*! no template .* soft-reconfiguration",
        "router bgp[4|6].*! no template .* remove-private-as-out",
        "router bgp[4|6].*! no template .* remove-private-as-in",
        "router bgp[4|6].*! no template .* override-peer-as-out",
        "router bgp[4|6].*! no template .* override-peer-as-in",
        "router bgp[4|6].*! no template .* route-reflector-client",
        "router bgp[4|6].*! no template .* confederation-peer",
        "router bgp[4|6].*! no template .* next-hop-unchanged",
        "router bgp[4|6].*! no template .* next-hop-self",
        "router bgp[4|6].*! no template .* next-hop-peer",
        "router bgp[4|6].*! no template .* send-community",
        "router bgp[4|6].*! no template .* maximum-prefix",
        "router bgp[4|6].*! no template .* prefix-list-in",
        "router bgp[4|6].*! no template .* prefix-list-out",
        "router bgp[4|6].*! no template .* route-map-in",
        "router bgp[4|6].*! no template .* route-map-out",
        "router bgp[4|6].*! no template .* route-map-vin",
        "router bgp[4|6].*! no template .* route-map-vout",
        "router bgp[4|6].*! no template .* route-policy-in",
        "router bgp[4|6].*! no template .* route-policy-out",
        "router bgp[4|6].*! no template .* route-policy-vin",
        "router bgp[4|6].*! no template .* route-policy-vout",
        "router bgp[4|6].*! afi-vrf .* distance 200",
        "router bgp[4|6].*! no afi-vrf .* flowspec",
        "router bgp[4|6].*! no afi-vrf .* mvpn",
        "router bgp[4|6].*! afi-ovrf .* distance 200",
        "router bgp[4|6].*! no afi-ovrf .* flowspec",
        "router bgp[4|6].*! no afi-ovrf .* mvpn",
        "router bgp[4|6].*! afi-vpls .* ve-id 0 0",
        // router msdp
        "router msdp[4|6] .*! neighbor .* timer 30000 75000 60000 120000",
        "router msdp[4|6] .*! no neighbor .* shutdown",
        "router msdp[4|6] .*! no neighbor .* update-source",
        "router msdp[4|6] .*! no neighbor .* password",
        "router msdp[4|6] .*! no neighbor .* bfd",
        // router flowspec
        "router flowspec[4|6] .*! distance 254",
        // router uni2multi
        "router uni2multi[4|6] .*! distance 254",
        // router uni2flow
        "router uni2flow[4|6] .*! distance 15",
        "router uni2flow[4|6] .*! direction target",
        "router uni2flow[4|6] .*! as 0",
        "router uni2flow[4|6] .*! rate 0",
        // router download
        "router download[4|6] .*! no hidden",
        "router download[4|6] .*! no log",
        "router download[4|6] .*! url ",
        "router download[4|6] .*! no range",
        "router download[4|6] .*! delay 0",
        "router download[4|6] .*! time 0",
        // router mobile
        "router mobile[4|6] .*! distance 254",
        // router logger
        "router logger[4|6] .*! no flapstat",
        "router logger[4|6] .*! no logging",};

    /**
     * defaults filter
     */
    public static tabGen<userFilter> defaultF;

    /**
     * convert name to number
     *
     * @param a name to convert
     * @return value, negative on error
     */
    public static tabRouteEntry.routeType name2num(String a) {
        if (a.equals("rip4")) {
            return tabRouteEntry.routeType.rip4;
        }
        if (a.equals("rip6")) {
            return tabRouteEntry.routeType.rip6;
        }
        if (a.equals("babel4")) {
            return tabRouteEntry.routeType.babel4;
        }
        if (a.equals("babel6")) {
            return tabRouteEntry.routeType.babel6;
        }
        if (a.equals("olsr4")) {
            return tabRouteEntry.routeType.olsr4;
        }
        if (a.equals("olsr6")) {
            return tabRouteEntry.routeType.olsr6;
        }
        if (a.equals("ospf4")) {
            return tabRouteEntry.routeType.ospf4;
        }
        if (a.equals("ospf6")) {
            return tabRouteEntry.routeType.ospf6;
        }
        if (a.equals("isis4")) {
            return tabRouteEntry.routeType.isis4;
        }
        if (a.equals("isis6")) {
            return tabRouteEntry.routeType.isis6;
        }
        if (a.equals("pvrp4")) {
            return tabRouteEntry.routeType.pvrp4;
        }
        if (a.equals("pvrp6")) {
            return tabRouteEntry.routeType.pvrp6;
        }
        if (a.equals("lsrp4")) {
            return tabRouteEntry.routeType.lsrp4;
        }
        if (a.equals("lsrp6")) {
            return tabRouteEntry.routeType.lsrp6;
        }
        if (a.equals("eigrp4")) {
            return tabRouteEntry.routeType.eigrp4;
        }
        if (a.equals("eigrp6")) {
            return tabRouteEntry.routeType.eigrp6;
        }
        if (a.equals("bgp4")) {
            return tabRouteEntry.routeType.bgp4;
        }
        if (a.equals("bgp6")) {
            return tabRouteEntry.routeType.bgp6;
        }
        if (a.equals("static")) {
            return tabRouteEntry.routeType.staticRoute;
        }
        if (a.equals("connected")) {
            return tabRouteEntry.routeType.conn;
        }
        if (a.equals("msdp4")) {
            return tabRouteEntry.routeType.msdp4;
        }
        if (a.equals("msdp6")) {
            return tabRouteEntry.routeType.msdp6;
        }
        if (a.equals("flowspec4")) {
            return tabRouteEntry.routeType.flwspc4;
        }
        if (a.equals("flowspec6")) {
            return tabRouteEntry.routeType.flwspc6;
        }
        if (a.equals("uni2multi4")) {
            return tabRouteEntry.routeType.uni2multi4;
        }
        if (a.equals("uni2multi6")) {
            return tabRouteEntry.routeType.uni2multi6;
        }
        if (a.equals("uni2flow4")) {
            return tabRouteEntry.routeType.uni2flow4;
        }
        if (a.equals("uni2flow6")) {
            return tabRouteEntry.routeType.uni2flow6;
        }
        if (a.equals("logger4")) {
            return tabRouteEntry.routeType.logger4;
        }
        if (a.equals("logger6")) {
            return tabRouteEntry.routeType.logger6;
        }
        if (a.equals("download4")) {
            return tabRouteEntry.routeType.download4;
        }
        if (a.equals("download6")) {
            return tabRouteEntry.routeType.download6;
        }
        if (a.equals("deaggr4")) {
            return tabRouteEntry.routeType.deaggr4;
        }
        if (a.equals("deaggr6")) {
            return tabRouteEntry.routeType.deaggr6;
        }
        if (a.equals("mobile4")) {
            return tabRouteEntry.routeType.mobile4;
        }
        if (a.equals("mobile6")) {
            return tabRouteEntry.routeType.mobile6;
        }
        return null;
    }

    /**
     * convert number to name
     *
     * @param i number to convert
     * @return name of protocol
     */
    public static String num2name(tabRouteEntry.routeType i) {
        switch (i) {
            case rip4:
                return "rip4";
            case rip6:
                return "rip6";
            case babel4:
                return "babel4";
            case babel6:
                return "babel6";
            case olsr4:
                return "olsr4";
            case olsr6:
                return "olsr6";
            case ospf4:
                return "ospf4";
            case ospf6:
                return "ospf6";
            case isis4:
                return "isis4";
            case isis6:
                return "isis6";
            case pvrp4:
                return "pvrp4";
            case pvrp6:
                return "pvrp6";
            case lsrp4:
                return "lsrp4";
            case lsrp6:
                return "lsrp6";
            case eigrp4:
                return "eigrp4";
            case eigrp6:
                return "eigrp6";
            case bgp4:
                return "bgp4";
            case bgp6:
                return "bgp6";
            case msdp4:
                return "msdp4";
            case msdp6:
                return "msdp6";
            case flwspc4:
                return "flowspec4";
            case flwspc6:
                return "flowspec6";
            case uni2multi4:
                return "uni2multi4";
            case uni2multi6:
                return "uni2multi6";
            case uni2flow4:
                return "uni2flow4";
            case uni2flow6:
                return "uni2flow6";
            case logger4:
                return "logger4";
            case logger6:
                return "logger6";
            case download4:
                return "download4";
            case download6:
                return "download6";
            case deaggr4:
                return "deaggr4";
            case deaggr6:
                return "deaggr6";
            case mobile4:
                return "mobile4";
            case mobile6:
                return "mobile6";
            case staticRoute:
                return "static";
            case conn:
                return "connected";
            default:
                return "";
        }
    }

    /**
     * test if need process number
     *
     * @param i router type
     * @return true if needed, false if not
     */
    public static boolean num2proc(tabRouteEntry.routeType i) {
        switch (i) {
            case staticRoute:
            case conn:
                return false;
            default:
                return true;
        }
    }

    /**
     * get redistribution config
     *
     * @param lst list to update
     * @param beg beginning
     * @param rtr router to use
     */
    public static void getShRedist(List<String> lst, String beg, ipRtr rtr) {
        for (int i = 0; i < rtr.routerRedisting.size(); i++) {
            ipRtrRed ntry = rtr.routerRedisting.get(i);
            if (ntry == null) {
                continue;
            }
            lst.add(beg + "redistribute " + redist2str(ntry));
        }
        for (int i = 0; i < rtr.routerAdverting.size(); i++) {
            ipRtrAdv ntry = rtr.routerAdverting.get(i);
            if (ntry == null) {
                continue;
            }
            lst.add(beg + "advertise " + advert2str(ntry));
        }
        for (int i = 0; i < rtr.routerAdvInter.size(); i++) {
            ipRtrInt ntry = rtr.routerAdvInter.get(i);
            if (ntry == null) {
                continue;
            }
            lst.add(beg + "justadvert " + advint2str(ntry));
        }
        for (int i = 0; i < rtr.routerAggregating.size(); i++) {
            ipRtrAgr ntry = rtr.routerAggregating.get(i);
            if (ntry == null) {
                continue;
            }
            lst.add(beg + "aggregate " + aggreg2str(ntry));
        }
    }

    /**
     * do redistribution config
     *
     * @param rtr router to update
     * @param neg negated
     * @param a command word
     * @param cmd command to parse
     * @return false on success, true on error
     */
    public static boolean doCfgRedist(ipRtr rtr, boolean neg, String a, cmds cmd) {
        if (a.equals("redistribute")) {
            ipRtrRed ntry = str2redist(cmd);
            if (ntry == null) {
                return true;
            }
            if (neg) {
                rtr.routerRedisting.del(ntry);
            } else {
                rtr.routerRedisting.put(ntry);
            }
            rtr.routerCreateComputed();
            return false;
        }
        if (a.equals("justadvert")) {
            ipRtrInt ntry = str2advint(cmd);
            if (ntry == null) {
                return true;
            }
            try {
                if (neg) {
                    rtr.routerAdvInter.del(ntry);
                } else {
                    rtr.routerAdvInter.put(ntry);
                }
            } catch (Exception e) {
                cmd.error("no such iface");
            }
            rtr.routerCreateComputed();
            return false;
        }
        if (a.equals("advertise")) {
            ipRtrAdv ntry = str2advert(cmd);
            if (ntry == null) {
                return true;
            }
            if (neg) {
                rtr.routerAdverting.del(ntry);
            } else {
                rtr.routerAdverting.put(ntry);
            }
            rtr.routerCreateComputed();
            return false;
        }
        if (a.equals("aggregate")) {
            ipRtrAgr ntry = str2aggreg(cmd);
            if (ntry == null) {
                return true;
            }
            if (neg) {
                rtr.routerAggregating.del(ntry);
            } else {
                rtr.routerAggregating.put(ntry);
            }
            rtr.routerCreateComputed();
            return false;
        }
        return true;
    }

    /**
     * convert redistribution to string
     *
     * @param red redistribution
     * @return string
     */
    public static String redist2str(ipRtrRed red) {
        String a = num2name(red.typ);
        if (num2proc(red.typ)) {
            a += " " + red.num;
        }
        if (red.prflst != null) {
            a += " prefix-list " + red.prflst.listName;
        }
        if (red.roumap != null) {
            a += " route-map " + red.roumap.listName;
        }
        if (red.rouplc != null) {
            a += " route-policy " + red.rouplc.listName;
        }
        return a;
    }

    /**
     * convert advertisement to string
     *
     * @param adv advertisement
     * @return string
     */
    public static String advert2str(ipRtrAdv adv) {
        String a = "" + addrPrefix.ip2str(adv.prefix);
        if (adv.roumap != null) {
            a += " route-map " + adv.roumap.listName;
        }
        if (adv.rouplc != null) {
            a += " route-policy " + adv.rouplc.listName;
        }
        return a;
    }

    /**
     * convert aggregating to string
     *
     * @param agr aggregating
     * @return string
     */
    public static String aggreg2str(ipRtrAgr agr) {
        String a = "" + addrPrefix.ip2str(agr.prefix);
        if (agr.prflst != null) {
            a += " prefix-list " + agr.prflst.listName;
        }
        if (agr.roumap != null) {
            a += " route-map " + agr.roumap.listName;
        }
        if (agr.rouplc != null) {
            a += " route-policy " + agr.rouplc.listName;
        }
        if (agr.aspath) {
            a += " as-set";
        }
        if (agr.summary) {
            a += " summary-only";
        }
        return a;
    }

    /**
     * convert interface to string
     *
     * @param ifc interface
     * @return string
     */
    public static String advint2str(ipRtrInt ifc) {
        String a = "" + ifc.iface.name;
        if (ifc.roumap != null) {
            a += " route-map " + ifc.roumap.listName;
        }
        if (ifc.rouplc != null) {
            a += " route-policy " + ifc.rouplc.listName;
        }
        return a;
    }

    /**
     * parse up redistribution
     *
     * @param cmd command to read
     * @return redistribution
     */
    public static ipRtrRed str2redist(cmds cmd) {
        tabRouteEntry.routeType i = name2num(cmd.word());
        if (i == null) {
            return null;
        }
        int o = -1;
        if (num2proc(i)) {
            o = bits.str2num(cmd.word());
        }
        ipRtrRed red = new ipRtrRed(i, o);
        for (;;) {
            String s = cmd.word();
            if (s.length() < 1) {
                break;
            }
            if (s.equals("prefix-list")) {
                cfgPrfxlst ntry = cfgAll.prfxFind(cmd.word(), false);
                if (ntry == null) {
                    return null;
                }
                red.prflst = ntry.prflst;
                continue;
            }
            if (s.equals("route-map")) {
                cfgRoump ntry = cfgAll.rtmpFind(cmd.word(), false);
                if (ntry == null) {
                    return null;
                }
                red.roumap = ntry.roumap;
                continue;
            }
            if (s.equals("route-policy")) {
                cfgRouplc ntry = cfgAll.rtplFind(cmd.word(), false);
                if (ntry == null) {
                    return null;
                }
                red.rouplc = ntry.rouplc;
                continue;
            }
            return null;
        }
        return red;
    }

    /**
     * parse up advertisement
     *
     * @param cmd command to read
     * @return advertisement
     */
    public static ipRtrAdv str2advert(cmds cmd) {
        addrPrefix<addrIP> prf = addrPrefix.str2ip(cmd.word());
        if (prf == null) {
            return null;
        }
        ipRtrAdv adv = new ipRtrAdv(prf);
        for (;;) {
            String s = cmd.word();
            if (s.length() < 1) {
                break;
            }
            if (s.equals("route-map")) {
                cfgRoump ntry = cfgAll.rtmpFind(cmd.word(), false);
                if (ntry == null) {
                    return null;
                }
                adv.roumap = ntry.roumap;
                continue;
            }
            if (s.equals("route-policy")) {
                cfgRouplc ntry = cfgAll.rtplFind(cmd.word(), false);
                if (ntry == null) {
                    return null;
                }
                adv.rouplc = ntry.rouplc;
                continue;
            }
            return null;
        }
        return adv;
    }

    /**
     * parse up aggregation
     *
     * @param cmd command to read
     * @return aggregation
     */
    public static ipRtrAgr str2aggreg(cmds cmd) {
        addrPrefix<addrIP> prf = addrPrefix.str2ip(cmd.word());
        if (prf == null) {
            return null;
        }
        ipRtrAgr agr = new ipRtrAgr(prf);
        for (;;) {
            String s = cmd.word();
            if (s.length() < 1) {
                break;
            }
            if (s.equals("prefix-list")) {
                cfgPrfxlst ntry = cfgAll.prfxFind(cmd.word(), false);
                if (ntry == null) {
                    return null;
                }
                agr.prflst = ntry.prflst;
                continue;
            }
            if (s.equals("route-map")) {
                cfgRoump ntry = cfgAll.rtmpFind(cmd.word(), false);
                if (ntry == null) {
                    return null;
                }
                agr.roumap = ntry.roumap;
                continue;
            }
            if (s.equals("route-policy")) {
                cfgRouplc ntry = cfgAll.rtplFind(cmd.word(), false);
                if (ntry == null) {
                    return null;
                }
                agr.rouplc = ntry.rouplc;
                continue;
            }
            if (s.equals("as-set")) {
                agr.aspath = true;
                continue;
            }
            if (s.equals("summary-only")) {
                agr.summary = true;
                continue;
            }
            return null;
        }
        return agr;
    }

    /**
     * parse up interface
     *
     * @param cmd command to read
     * @return advertisement
     */
    public static ipRtrInt str2advint(cmds cmd) {
        cfgIfc iface = cfgAll.ifcFind(cmd.word(), false);
        ipRtrInt adv = new ipRtrInt(iface);
        for (;;) {
            String s = cmd.word();
            if (s.length() < 1) {
                break;
            }
            if (s.equals("route-map")) {
                cfgRoump ntry = cfgAll.rtmpFind(cmd.word(), false);
                if (ntry == null) {
                    return null;
                }
                adv.roumap = ntry.roumap;
                continue;
            }
            if (s.equals("route-policy")) {
                cfgRouplc ntry = cfgAll.rtplFind(cmd.word(), false);
                if (ntry == null) {
                    return null;
                }
                adv.rouplc = ntry.rouplc;
                continue;
            }
            return null;
        }
        return adv;
    }

    public int compare(cfgRtr o1, cfgRtr o2) {
        int i = o1.type.compareTo(o2.type);
        if (i != 0) {
            return i;
        }
        if (o1.number < o2.number) {
            return -1;
        }
        if (o1.number > o2.number) {
            return +1;
        }
        return 0;
    }

    public String toString() {
        return "rtr " + number;
    }

    /**
     * create new bridge instance
     *
     * @param typ type of routing process
     * @param num number of bridge
     */
    public cfgRtr(tabRouteEntry.routeType typ, int num) {
        type = typ;
        number = num;
    }

    /**
     * destroy this router
     */
    public synchronized void closeUp() {
        running = false;
        if (rip4 != null) {
            rip4.routerCloseNow();
            rip4 = null;
        }
        if (rip6 != null) {
            rip6.routerCloseNow();
            rip6 = null;
        }
        if (babel != null) {
            babel.routerCloseNow();
            babel = null;
        }
        if (olsr != null) {
            olsr.routerCloseNow();
            olsr = null;
        }
        if (ospf4 != null) {
            ospf4.routerCloseNow();
            ospf4 = null;
        }
        if (ospf6 != null) {
            ospf6.routerCloseNow();
            ospf6 = null;
        }
        if (isis != null) {
            isis.routerCloseNow();
            isis = null;
        }
        if (pvrp != null) {
            pvrp.routerCloseNow();
            pvrp = null;
        }
        if (lsrp != null) {
            lsrp.routerCloseNow();
            lsrp = null;
        }
        if (eigrp != null) {
            eigrp.routerCloseNow();
            eigrp = null;
        }
        if (bgp != null) {
            bgp.routerCloseNow();
            bgp = null;
        }
        if (msdp != null) {
            msdp.routerCloseNow();
            msdp = null;
        }
        if (flwspc != null) {
            flwspc.routerCloseNow();
            flwspc = null;
        }
        if (uni2multi != null) {
            uni2multi.routerCloseNow();
            uni2multi = null;
        }
        if (uni2flow != null) {
            uni2flow.routerCloseNow();
            uni2flow = null;
        }
        if (logger != null) {
            logger.routerCloseNow();
            logger = null;
        }
        if (download != null) {
            download.routerCloseNow();
            download = null;
        }
        if (deaggr != null) {
            deaggr.routerCloseNow();
            deaggr = null;
        }
        if (mobile != null) {
            mobile.routerCloseNow();
            mobile = null;
        }
    }

    /**
     * get routing process
     *
     * @return routing process
     */
    public synchronized ipRtr getRouter() {
        switch (type) {
            case rip4:
                return rip4;
            case rip6:
                return rip6;
            case babel4:
            case babel6:
                return babel;
            case olsr4:
            case olsr6:
                return olsr;
            case ospf4:
                return ospf4;
            case ospf6:
                return ospf6;
            case isis4:
            case isis6:
                return isis;
            case pvrp4:
            case pvrp6:
                return pvrp;
            case lsrp4:
            case lsrp6:
                return lsrp;
            case eigrp4:
            case eigrp6:
                return eigrp;
            case bgp4:
            case bgp6:
                return bgp;
            case msdp4:
            case msdp6:
                return msdp;
            case flwspc4:
            case flwspc6:
                return flwspc;
            case uni2multi4:
            case uni2multi6:
                return uni2multi;
            case uni2flow4:
            case uni2flow6:
                return uni2flow;
            case logger4:
            case logger6:
                return logger;
            case download4:
            case download6:
                return download;
            case deaggr4:
            case deaggr6:
                return deaggr;
            case mobile4:
            case mobile6:
                return mobile;
            default:
                return null;
        }
    }

    /**
     * setup this router
     *
     * @return true on error, false if successful
     */
    public synchronized boolean setup2run() {
        if (vrf == null) {
            return true;
        }
        running = true;
        switch (type) {
            case rip4:
                rip4 = new rtrRip4(vrf.fwd4, vrf.udp4, number);
                break;
            case rip6:
                rip6 = new rtrRip6(vrf.fwd6, vrf.udp6, number);
                break;
            case babel4:
                babel = new rtrBabel(vrf.fwd4, vrf.udp4, number);
                break;
            case babel6:
                babel = new rtrBabel(vrf.fwd6, vrf.udp6, number);
                break;
            case olsr4:
                olsr = new rtrOlsr(vrf.fwd4, vrf.udp4, number);
                break;
            case olsr6:
                olsr = new rtrOlsr(vrf.fwd6, vrf.udp6, number);
                break;
            case ospf4:
                ospf4 = new rtrOspf4(vrf.fwd4, number);
                break;
            case ospf6:
                ospf6 = new rtrOspf6(vrf.fwd6, number);
                break;
            case isis4:
                isis = new rtrIsis(vrf.fwd4, number);
                break;
            case isis6:
                isis = new rtrIsis(vrf.fwd6, number);
                break;
            case pvrp4:
                pvrp = new rtrPvrp(vrf.fwd4, vrf.udp4, vrf.tcp4, number);
                break;
            case pvrp6:
                pvrp = new rtrPvrp(vrf.fwd6, vrf.udp6, vrf.tcp6, number);
                break;
            case lsrp4:
                lsrp = new rtrLsrp(vrf.fwd4, vrf.udp4, vrf.tcp4, number);
                break;
            case lsrp6:
                lsrp = new rtrLsrp(vrf.fwd6, vrf.udp6, vrf.tcp6, number);
                break;
            case eigrp4:
                eigrp = new rtrEigrp(vrf.fwd4, number);
                break;
            case eigrp6:
                eigrp = new rtrEigrp(vrf.fwd6, number);
                break;
            case bgp4:
                bgp = new rtrBgp(vrf.fwd4, vrf, vrf.tcp4, number);
                break;
            case bgp6:
                bgp = new rtrBgp(vrf.fwd6, vrf, vrf.tcp6, number);
                break;
            case msdp4:
                msdp = new rtrMsdp(vrf.fwd4, vrf.tcp4, number);
                break;
            case msdp6:
                msdp = new rtrMsdp(vrf.fwd6, vrf.tcp6, number);
                break;
            case flwspc4:
                flwspc = new rtrFlowspec(vrf.fwd4, number);
                break;
            case flwspc6:
                flwspc = new rtrFlowspec(vrf.fwd6, number);
                break;
            case uni2multi4:
                uni2multi = new rtrUni2multi(vrf.fwd4, number);
                break;
            case uni2multi6:
                uni2multi = new rtrUni2multi(vrf.fwd6, number);
                break;
            case uni2flow4:
                uni2flow = new rtrUni2flow(vrf.fwd4, number);
                break;
            case uni2flow6:
                uni2flow = new rtrUni2flow(vrf.fwd6, number);
                break;
            case logger4:
                logger = new rtrLogger(vrf.fwd4, number);
                break;
            case logger6:
                logger = new rtrLogger(vrf.fwd6, number);
                break;
            case download4:
                download = new rtrDownload(vrf.fwd4, number);
                break;
            case download6:
                download = new rtrDownload(vrf.fwd6, number);
                break;
            case deaggr4:
                deaggr = new rtrDeaggr(vrf.fwd4, number);
                break;
            case deaggr6:
                deaggr = new rtrDeaggr(vrf.fwd6, number);
                break;
            case mobile4:
                mobile = new rtrMobile(vrf.fwd4, number);
                break;
            case mobile6:
                mobile = new rtrMobile(vrf.fwd6, number);
                break;
            default:
                return true;
        }
        return false;
    }

    private synchronized List<String> getShRun(int mode, boolean filter) {
        boolean need2nd;
        switch (type) {
            case bgp4:
            case bgp6:
            case msdp4:
            case msdp6:
                need2nd = true;
                break;
            default:
                need2nd = false;
                break;
        }
        List<String> l = new ArrayList<String>();
        switch (mode) {
            case 1:
                break;
            case 2:
                if (need2nd) {
                    return l;
                }
                break;
            case 3:
                if (!need2nd) {
                    return l;
                }
                break;
            default:
                return l;
        }
        l.add("router " + num2name(type) + " " + number);
        if (vrf == null) {
            l.add(cmds.tabulator + "no vrf");
        } else {
            l.add(cmds.tabulator + "vrf " + vrf.name);
        }
        ipRtr rtr = getRouter();
        if (rtr != null) {
            rtr.routerGetConfig(l, cmds.tabulator, filter);
            getShRedist(l, cmds.tabulator, rtr);
            if (rtr.routerAutoMesh == null) {
                l.add(cmds.tabulator + "no automesh");
            } else {
                l.add(cmds.tabulator + "automesh " + rtr.routerAutoMesh.listName);
            }
        }
        l.add(cmds.tabulator + cmds.finish);
        l.add(cmds.comment);
        if (!filter) {
            return l;
        }
        return userFilter.filterText(l, defaultF);
    }

    public List<String> getShRun(boolean filter) {
        return getShRun(2, filter);
    }

    /**
     * get configuration
     *
     * @param filter filter defaults
     * @return configuration
     */
    public List<String> getShRun2(boolean filter) {
        return getShRun(3, filter);
    }

    /**
     * get help string
     *
     * @param l list to update
     * @param p number start
     * @param e ending
     */
    public static void getRouterList(userHelping l, int p, String e) {
        l.add((p + 2) + " " + (p + 3) + "     bgp4                  border gateway protocol" + e);
        l.add((p + 2) + " " + (p + 3) + "     bgp6                  border gateway protocol" + e);
        l.add((p + 2) + " " + (p + 3) + "     ospf4                 open shortest path first" + e);
        l.add((p + 2) + " " + (p + 3) + "     ospf6                 open shortest path first" + e);
        l.add((p + 2) + " " + (p + 3) + "     isis4                 intermediate system intermediate system" + e);
        l.add((p + 2) + " " + (p + 3) + "     isis6                 intermediate system intermediate system" + e);
        l.add((p + 2) + " " + (p + 3) + "     pvrp4                 path vector routing protocol" + e);
        l.add((p + 2) + " " + (p + 3) + "     pvrp6                 path vector routing protocol" + e);
        l.add((p + 2) + " " + (p + 3) + "     lsrp4                 link state routing protocol" + e);
        l.add((p + 2) + " " + (p + 3) + "     lsrp6                 link state routing protocol" + e);
        l.add((p + 2) + " " + (p + 3) + "     eigrp4                enhanced interior gateway routing protocol" + e);
        l.add((p + 2) + " " + (p + 3) + "     eigrp6                enhanced interior gateway routing protocol" + e);
        l.add((p + 2) + " " + (p + 3) + "     rip4                  routing information protocol" + e);
        l.add((p + 2) + " " + (p + 3) + "     rip6                  routing information protocol" + e);
        l.add((p + 2) + " " + (p + 3) + "     babel4                babel routing protocol" + e);
        l.add((p + 2) + " " + (p + 3) + "     babel6                babel routing protocol" + e);
        l.add((p + 2) + " " + (p + 3) + "     olsr4                 optimized link state routing protocol" + e);
        l.add((p + 2) + " " + (p + 3) + "     olsr6                 optimized link state routing protocol" + e);
        l.add((p + 2) + " " + (p + 3) + "     msdp4                 multicast source discovery protocol" + e);
        l.add((p + 2) + " " + (p + 3) + "     msdp6                 multicast source discovery protocol" + e);
        l.add((p + 2) + " " + (p + 3) + "     flowspec4             flowspec to flowspec rewriter" + e);
        l.add((p + 2) + " " + (p + 3) + "     flowspec6             flowspec to flowspec rewriter" + e);
        l.add((p + 2) + " " + (p + 3) + "     uni2multi4            unicast to multicast converter" + e);
        l.add((p + 2) + " " + (p + 3) + "     uni2multi6            unicast to multicast converter" + e);
        l.add((p + 2) + " " + (p + 3) + "     uni2flow4             unicast to flowspec converter" + e);
        l.add((p + 2) + " " + (p + 3) + "     uni2flow6             unicast to flowspec converter" + e);
        l.add((p + 2) + " " + (p + 3) + "     logger4               route logger" + e);
        l.add((p + 2) + " " + (p + 3) + "     logger6               route logger" + e);
        l.add((p + 2) + " " + (p + 3) + "     download4             route download" + e);
        l.add((p + 2) + " " + (p + 3) + "     download6             route download" + e);
        l.add((p + 2) + " " + (p + 3) + "     deaggr4               deaggregate creator" + e);
        l.add((p + 2) + " " + (p + 3) + "     deaggr6               deaggregate creator" + e);
        l.add((p + 2) + " " + (p + 3) + "     mobile4               mobile route creator" + e);
        l.add((p + 2) + " " + (p + 3) + "     mobile6               mobile route creator" + e);
    }

    /**
     * get help string
     *
     * @param l list to update
     * @param p number start
     */
    public static void getRedistHelp(userHelping l, int p) {
        l.add((p + 1) + " " + (p + 2) + "   redistribute            redistribute prefixes from other protocols");
        l.add((p + 2) + " " + (p + 4) + ",.   connected             connected routes");
        l.add((p + 2) + " " + (p + 4) + ",.   static                static routes");
        getRouterList(l, p, " routes");
        l.add((p + 3) + " " + (p + 4) + ",.     <num>               process id");
        l.add((p + 4) + " " + (p + 5) + "         route-map         process prefixes on importing");
        l.add((p + 5) + " " + (p + 4) + ",.         <name>          name of route map");
        l.add((p + 4) + " " + (p + 5) + "         route-policy      process prefixes on importing");
        l.add((p + 5) + " " + (p + 4) + ",.         <name>          name of route policy");
        l.add((p + 4) + " " + (p + 5) + "         prefix-list       filter prefixes on importing");
        l.add((p + 5) + " " + (p + 4) + ",.         <name>          name of prefix list");
        l.add((p + 1) + " " + (p + 2) + "   advertise               advertise one prefix");
        l.add((p + 2) + " " + (p + 3) + ",.   <pref>                prefix");
        l.add((p + 3) + " " + (p + 4) + "       route-map           set properties of advertisement");
        l.add((p + 4) + " .         <name>            name of route map");
        l.add((p + 3) + " " + (p + 4) + "       route-policy        set properties of advertisement");
        l.add((p + 4) + " .         <name>            name of route policy");
        l.add((p + 1) + " " + (p + 2) + "   justadvert              advertise interface");
        l.add((p + 2) + " " + (p + 3) + ",.   <name>                name of interface");
        l.add((p + 3) + " " + (p + 4) + "       route-map           set properties of advertisement");
        l.add((p + 4) + " .         <name>            name of route map");
        l.add((p + 3) + " " + (p + 4) + "       route-policy        set properties of advertisement");
        l.add((p + 4) + " .         <name>            name of route policy");
        l.add((p + 1) + " " + (p + 2) + "   aggregate               aggregate more prefixes");
        l.add((p + 2) + " " + (p + 3) + ",.   <pref>                prefix");
        l.add((p + 3) + " " + (p + 4) + "       route-map           set properties of advertisement");
        l.add((p + 4) + " " + (p + 3) + ",.       <name>            name of route map");
        l.add((p + 3) + " " + (p + 4) + "       route-policy        set properties of advertisement");
        l.add((p + 4) + " " + (p + 3) + ",.       <name>            name of route policy");
        l.add((p + 3) + " " + (p + 4) + "       prefix-list         filter prefixes for aggregation");
        l.add((p + 4) + " " + (p + 3) + ",.       <name>            name of prefix list");
        l.add((p + 3) + " " + (p + 3) + ",.     as-set              generate as path information");
        l.add((p + 3) + " " + (p + 3) + ",.     summary-only        filter more specific prefixes");
    }

    public userHelping getHelp() {
        userHelping l = userHelping.getGenCfg();
        l.add("1 2   vrf                     specify vrf to use");
        l.add("2 .     <name>                name of table");
        l.add("1 2   automesh                specify auto mesh te tunnels");
        l.add("2 .     <name>                name of prefix list");
        getRedistHelp(l, 0);
        ipRtr rtr = getRouter();
        if (rtr != null) {
            rtr.routerGetHelp(l);
        }
        return l;
    }

    public synchronized void doCfgStr(cmds cmd) {
        ipRtr rtr = getRouter();
        if (rtr != null) {
            if (!rtr.routerConfigure(cmd.copyBytes(false))) {
                return;
            }
        }
        String a = cmd.word();
        boolean neg = a.equals("no");
        if (neg) {
            a = cmd.word();
        }
        if (a.equals("vrf")) {
            if (neg) {
                closeUp();
                vrf = null;
                return;
            }
            if (vrf != null) {
                cmd.error("already initialized");
                return;
            }
            a = cmd.word();
            cfgVrf v = cfgAll.vrfFind(a, false);
            if (v == null) {
                cmd.error("no such vrf");
                return;
            }
            vrf = v;
            setup2run();
            return;
        }
        if (rtr == null) {
            cmd.error("not initialized");
            return;
        }
        if (a.equals("automesh")) {
            if (neg) {
                rtr.routerAutoMesh = null;
                return;
            }
            cfgPrfxlst prf = cfgAll.prfxFind(cmd.word(), false);
            if (prf == null) {
                cmd.error("no such prefixlist");
                return;
            }
            rtr.routerAutoMesh = prf.prflst;
            return;
        }
        if (doCfgRedist(rtr, neg, a, cmd)) {
            cmd.badCmd();
        }
    }

    public String getPrompt() {
        return "rtr";
    }

}
