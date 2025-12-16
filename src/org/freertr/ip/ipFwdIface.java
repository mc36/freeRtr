package org.freertr.ip;

import java.util.ArrayList;
import java.util.List;
import org.freertr.addr.addrIP;
import org.freertr.addr.addrIPv4;
import org.freertr.addr.addrIPv6;
import org.freertr.addr.addrMac;
import org.freertr.addr.addrPrefix;
import org.freertr.auth.authLocal;
import org.freertr.cfg.cfgAceslst;
import org.freertr.cfg.cfgAll;
import org.freertr.cfg.cfgIfc;
import org.freertr.cfg.cfgPrfxlst;
import org.freertr.cfg.cfgRoump;
import org.freertr.cfg.cfgRouplc;
import org.freertr.cfg.cfgRtr;
import org.freertr.pack.packHolder;
import org.freertr.prt.prtTcp;
import org.freertr.prt.prtUdp;
import org.freertr.rtr.rtrBfdClnt;
import org.freertr.rtr.rtrBfdIface;
import org.freertr.rtr.rtrBfdNeigh;
import org.freertr.rtr.rtrHsrpIface;
import org.freertr.rtr.rtrLdpNeigh;
import org.freertr.rtr.rtrNshIface;
import org.freertr.rtr.rtrPimIface;
import org.freertr.rtr.rtrPtpIface;
import org.freertr.rtr.rtrSrhIface;
import org.freertr.rtr.rtrVrrpIface;
import org.freertr.tab.tabAceslstN;
import org.freertr.tab.tabGen;
import org.freertr.tab.tabLabel;
import org.freertr.tab.tabListing;
import org.freertr.tab.tabPbrN;
import org.freertr.tab.tabPrfxlstN;
import org.freertr.tab.tabRateLimit;
import org.freertr.tab.tabRouteAttr;
import org.freertr.tab.tabRouteEntry;
import org.freertr.tab.tabRouteIface;
import org.freertr.tab.tabRtrmapN;
import org.freertr.tab.tabRtrplcN;
import org.freertr.tab.tabSession;
import org.freertr.user.userFormat;
import org.freertr.user.userHelp;
import org.freertr.util.bits;
import org.freertr.util.cmds;
import org.freertr.util.counter;

/**
 * stores one ip interface
 *
 * @author matecsaba
 */
public class ipFwdIface extends tabRouteIface {

    /**
     * set true to enable link local on this interface
     */
    public boolean linkLocal = false;

    /**
     * set true to send unreachables from this interface
     */
    public boolean unreachEna = true;

    /**
     * source of unreachables, null if this interface
     */
    public ipFwdIface unreachSrc = null;

    /**
     * set true to send multicast as broadcast
     */
    public boolean mcastAsBcast = false;

    /**
     * set true to send multicast as unicast
     */
    public boolean mcastAsUcast = false;

    /**
     * set true to disable flowspec
     */
    public boolean disableFlowspec = false;

    /**
     * set true to disable dapp
     */
    public boolean disableDapp = false;

    /**
     * set true to block host to host on communication
     */
    public boolean blockHost2host = true;

    /**
     * always mpls propagate ip ttl
     */
    public boolean mplsPropTtlAlways = false;

    /**
     * allow mpls propagate ip ttl
     */
    public boolean mplsPropTtlAllow = true;

    /**
     * check against just safe protocol numbers
     */
    public boolean protocolSecurity = false;

    /**
     * process netflow on receiving
     */
    public boolean netflowRx = false;

    /**
     * process netflow on sending
     */
    public boolean netflowTx = false;

    /**
     * set true to verify that source reachable
     */
    public boolean verifySource = false;

    /**
     * set true to perform verify that reachable via this iface
     */
    public boolean verifyStricht = false;

    /**
     * set true if have to answer outside requests
     */
    public boolean answerDefReqs = false;

    /**
     * set true if have to answer local requests
     */
    public boolean answerNetReqs = false;

    /**
     * set true if have to filter answers
     */
    public tabListing<tabAceslstN<addrIP>, addrIP> answerFilter;

    /**
     * inspection information
     */
    public tabSession inspect = null;

    /**
     * interface handler
     */
    public final ipIfc lower;

    /**
     * ip address
     */
    public addrIP addr;

    /**
     * netmask
     */
    public int mask;

    /**
     * prefix of network
     */
    public addrPrefix<addrIP> network;

    /**
     * ready for use, true if yes, false if no
     */
    public boolean ready = true;

    /**
     * install connected route
     */
    public boolean gateCon = true;

    /**
     * install local route
     */
    public boolean gateLoc = true;

    /**
     * install remote route
     */
    public boolean gateRem = true;

    /**
     * process every packet
     */
    public boolean gatePrc = false;

    /**
     * install connected with this distance
     */
    public int gateDstC = 0;

    /**
     * install local with this distance
     */
    public int gateDstL = 0;

    /**
     * install remote with this distance
     */
    public int gateDstR = 0;

    /**
     * install prefixes with this distance
     */
    public int gateDstP = 0;

    /**
     * install labeled route, 0=unlabeled, 1=implicit, 2=explicit
     */
    public int gateLab = 0;

    /**
     * gateway
     */
    public addrIP gateAddr;

    /**
     * prefixes through gateway
     */
    public tabListing<tabPrfxlstN, addrIP> gatePrfx;

    /**
     * routemap through gateway
     */
    public tabListing<tabRtrmapN, addrIP> gateRtmp;

    /**
     * routepolicy through gateway
     */
    public tabListing<tabRtrplcN, addrIP> gateRplc;

    /**
     * fragmentation payload size
     */
    public int fragments;

    /**
     * reported pmtud payload size
     */
    public int pmtudIn;

    /**
     * reported pmtud payload size
     */
    public int pmtudOut;

    /**
     * reassembly buffer
     */
    public List<packHolder> reasmBuf;

    /**
     * reassembly position
     */
    public int reasmNxt;

    /**
     * ingress acl
     */
    public tabListing<tabAceslstN<addrIP>, addrIP> filterIn;

    /**
     * egress acl
     */
    public tabListing<tabAceslstN<addrIP>, addrIP> filterOut;

    /**
     * common ingress acl
     */
    public tabListing<tabAceslstN<addrIP>, addrIP> cfilterIn;

    /**
     * common egress acl
     */
    public tabListing<tabAceslstN<addrIP>, addrIP> cfilterOut;

    /**
     * the pbr entries
     */
    public final tabListing<tabPbrN, addrIP> pbrCfg = new tabListing<tabPbrN, addrIP>();

    /**
     * ingress tcp mss
     */
    public int tcpMssIn;

    /**
     * egress tcp mss
     */
    public int tcpMssOut;

    /**
     * bfd config
     */
    public rtrBfdIface bfdCfg;

    /**
     * ptp config
     */
    public rtrPtpIface ptpCfg;

    /**
     * counter of interface
     */
    public final counter cntr = new counter();

    /**
     * fragment counter of interface
     */
    public final counter frgCnt = new counter();

    /**
     * time when multicast flood expires
     */
    public long expires;

    /**
     * mtu on interface
     */
    public int mtu;

    /**
     * bandwidth on interface
     */
    public long bandwidth;

    /**
     * srh interface handler
     */
    public rtrSrhIface srhCfg;

    /**
     * nsh interface handler
     */
    public rtrNshIface nshCfg;

    /**
     * multicast source override in
     */
    public addrIP mcastSrcIn;

    /**
     * multicast source override out
     */
    public addrIP mcastSrcOut;

    /**
     * minimal multicast ttl
     */
    public int mcastTtl;

    /**
     * pim interface handler
     */
    public rtrPimIface pimCfg;

    /**
     * multicast host handler
     */
    public ipMhostIface mhostCfg;

    /**
     * multicast to ldp handler
     */
    public ipMldpIface mldpCfg;

    /**
     * hsrp interface handler
     */
    public rtrHsrpIface hsrpCfg;

    /**
     * vrrp interface handler
     */
    public rtrVrrpIface vrrpCfg;

    /**
     * next hop watch interface handler
     */
    public ipHostWatch hostWatch;

    /**
     * dlep configuration
     */
    public ipDlepIface dlepCfg;

    /**
     * address of remote
     */
    public addrIP hostRemote;

    /**
     * autoroute type
     */
    public tabRouteAttr.routeType autRouTyp;

    /**
     * autoroute proto
     */
    public int autRouPrt;

    /**
     * autoroute router
     */
    public addrIP autRouRtr;

    /**
     * autoroute nexthop
     */
    public addrIP autRouHop;

    /**
     * autoroute recursive
     */
    public boolean autRouRec;

    /**
     * autoroute multicast
     */
    public boolean autRouMcst;

    /**
     * autoroute multicast
     */
    public boolean autRouUnic;

    /**
     * autoroute excluded
     */
    public boolean autRouExcld;

    /**
     * autoroute prefix list
     */
    public tabListing<tabPrfxlstN, addrIP> autRouPfxlst;

    /**
     * autoroute route map
     */
    public tabListing<tabRtrmapN, addrIP> autRouRoumap;

    /**
     * autoroute route policy
     */
    public tabListing<tabRtrplcN, addrIP> autRouRoupol;

    /**
     * other interface handler
     */
    public ipFwdIface otherHandler;

    /**
     * secondary addresses
     */
    private final tabGen<ipFwdIfaceAddr> adrs = new tabGen<ipFwdIfaceAddr>();

    /**
     * secondary networks
     */
    private final tabGen<ipFwdIfacePref> nets = new tabGen<ipFwdIfacePref>();

    /**
     * ldp passwords
     */
    private final tabGen<ipFwdIfaceLdpas> ldpas = new tabGen<ipFwdIfaceLdpas>();

    /**
     * static mpls bindings
     */
    public final tabGen<ipFwdIfaceBind> mplStat = new tabGen<ipFwdIfaceBind>();

    /**
     * static mpls peer
     */
    public addrIP mplPeer;

    /**
     * static mpls hop
     */
    public addrIP mplHop;

    /**
     * create new instance
     *
     * @param num number assigned to it
     * @param hnd packet handler
     */
    public ipFwdIface(int num, ipIfc hnd) {
        ifwNum = num;
        lower = hnd;
    }

    /**
     * convert to string
     *
     * @return string
     */
    public String toString() {
        return "" + lower;
    }

    /**
     * get unreachable source
     *
     * @return address, null if disabled
     */
    public addrIP getUnreachAddr() {
        if (!unreachEna) {
            return null;
        }
        if (unreachSrc == null) {
            return addr;
        }
        return unreachSrc.addr;
    }

    /**
     * get show text
     *
     * @return show
     */
    public userFormat getShow() {
        userFormat l = new userFormat("|", "category|value");
        l.add("name|" + lower);
        l.add("ready|" + ready);
        l.add("num|" + ifwNum);
        l.add("typ|" + ifwTyp);
        l.add("mtu|" + mtu);
        l.add("bw|" + bits.bandwidth(bandwidth));
        l.add("addr|" + addr);
        l.add("mask|" + mask);
        l.add("gate|" + gateAddr);
        l.add("net|" + network);
        l.add("ll|" + lower.getLinkLocalAddr());
        for (int i = 0; i < adrs.size(); i++) {
            ipFwdIfaceAddr adr = adrs.get(i);
            if (adr == null) {
                continue;
            }
            l.add("additional|" + adr.ip + "|" + adr.mac + "|" + adr.cfg);
        }
        for (int i = 0; i < nets.size(); i++) {
            ipFwdIfacePref adr = nets.get(i);
            if (adr == null) {
                continue;
            }
            l.add("additional|" + adr.adr + "|" + adr.msk);
        }
        l.add("cntr|" + cntr.getShStat());
        l.add("lastio|" + cntr.getShTraff());
        counter cnt = lower.getCounter();
        l.add("lowcntr|" + cnt.getShStat());
        l.add("lowlast|" + cnt.getShTraff());
        l.add("frgcntr|" + frgCnt.getShStat());
        l.add("frglast|" + frgCnt.getShTraff());
        return l;
    }

    /**
     * get help text
     *
     * @param l storage
     */
    public static void getHelp(userHelp l) {
        l.add(null, false, 2, new int[]{-1}, "enable", "link local address routing");
        l.add(null, false, 2, new int[]{3}, "address", "set the ip address of an interface");
        l.add(null, false, 3, new int[]{4}, "dynamic", "dynamic address");
        l.add(null, false, 3, new int[]{4}, "<addr>", "address of interface");
        l.add(null, false, 4, new int[]{-1}, "dynamic", "dynamic netmask");
        l.add(null, false, 4, new int[]{-1}, "<mask>", "subnet mask of address");
        l.add(null, false, 2, new int[]{3}, "secondary-network", "set up an additional ip network");
        l.add(null, false, 3, new int[]{4}, "<addr>", "address of interface");
        l.add(null, false, 4, new int[]{-1}, "<mask>", "subnet mask of address");
        l.add(null, false, 2, new int[]{3}, "secondary-address", "set up an additional ip address");
        l.add(null, false, 3, new int[]{-1}, "<addr>", "address of interface");
        l.add(null, false, 2, new int[]{3}, "reassembly", "set up a reassembly buffer");
        l.add(null, false, 3, new int[]{-1}, "<num>", "number of packets");
        l.add(null, false, 2, new int[]{3}, "fragmentation", "set up fragmentation");
        l.add(null, false, 3, new int[]{-1}, "<num>", "maximum payload size");
        l.add(null, false, 2, new int[]{3}, "pmtud-in", "set up pmtud responder");
        l.add(null, false, 3, new int[]{-1}, "<num>", "maximum payload size");
        l.add(null, false, 2, new int[]{3}, "pmtud-out", "set up pmtud responder");
        l.add(null, false, 3, new int[]{-1}, "<num>", "maximum payload size");
        l.add(null, false, 2, new int[]{-1}, "netflow-rx", "netflow received packets");
        l.add(null, false, 2, new int[]{-1}, "netflow-tx", "netflow transmitted packets");
        l.add(null, false, 2, new int[]{-1}, "propagate-ttl-always", "enable ttl propagation to mpls");
        l.add(null, false, 2, new int[]{-1}, "propagate-ttl-allow", "allow ttl propagation to mpls");
        l.add(null, false, 2, new int[]{-1}, "unreachables", "enable sending icmp unreachable messages");
        l.add(null, false, 2, new int[]{3}, "unreach-source", "set unreachable source");
        l.add(null, false, 3, new int[]{-1}, "<name:ifc>", "name of interface");
        l.add(null, false, 2, new int[]{3}, "redirection", "send packets out on different interface");
        l.add(null, false, 3, new int[]{-1}, "<name:ifc>", "name of interface");
        l.add(null, false, 2, new int[]{-1}, "resend-packet", "enable sending packet out on same interface");
        l.add(null, false, 2, new int[]{-1}, "flowspec-disable", "disable flowspec processing");
        l.add(null, false, 2, new int[]{-1}, "dapp-disable", "disable dapp processing");
        l.add(null, false, 2, new int[]{3}, "verify-source", "enable per packet validation");
        l.add(null, false, 3, new int[]{-1}, "any", "source is reachable via any interface");
        l.add(null, false, 3, new int[]{-1}, "rx", "source is reachable via this interface");
        l.add(null, false, 3, new int[]{-1}, "none", "disable per packet source checking");
        l.add(null, false, 2, new int[]{-1}, "gateway-connected", "install connected route");
        l.add(null, false, 2, new int[]{-1}, "gateway-local", "install local route");
        l.add(null, false, 2, new int[]{-1}, "gateway-remote", "install remote route");
        l.add(null, false, 2, new int[]{-1}, "gateway-process", "every packet is local");
        l.add(null, false, 2, new int[]{3}, "gateway-distance", "install with this distance");
        l.add(null, false, 3, new int[]{4}, "<num>", "connected distance");
        l.add(null, false, 4, new int[]{5}, "<num>", "local distance");
        l.add(null, false, 5, new int[]{6}, "<num>", "remote distance");
        l.add(null, false, 6, new int[]{-1}, "<num>", "prefixes distance");
        l.add(null, false, 2, new int[]{3}, "gateway-labeled", "install labeled route");
        l.add(null, false, 3, new int[]{-1}, "unlabeled", "no label");
        l.add(null, false, 3, new int[]{-1}, "implicit-null", "implicit null");
        l.add(null, false, 3, new int[]{-1}, "explicit-null", "explicit null");
        l.add(null, false, 2, new int[]{3}, "gateway-prefix", "prefix list to install throught gateway");
        l.add(null, false, 3, new int[]{-1}, "<name:pl>", "name of prefix list");
        l.add(null, false, 2, new int[]{3}, "gateway-map", "route map to set throught gateway");
        l.add(null, false, 3, new int[]{-1}, "<name:rm>", "name of route map");
        l.add(null, false, 2, new int[]{3}, "gateway-policy", "route policy to set throught gateway");
        l.add(null, false, 3, new int[]{-1}, "<name:rpl>", "name of route policy");
        l.add(null, false, 2, new int[]{3}, "access-group-in", "access list to apply to ingress packets");
        l.add(null, false, 3, new int[]{-1}, "<name:acl>", "name of access list");
        l.add(null, false, 2, new int[]{3}, "access-group-out", "access list to apply to egress packets");
        l.add(null, false, 3, new int[]{-1}, "<name:acl>", "name of access list");
        l.add(null, false, 2, new int[]{3}, "access-group-common-in", "common access list to apply to ingress packets");
        l.add(null, false, 3, new int[]{-1}, "<name:acl>", "name of access list");
        l.add(null, false, 2, new int[]{3}, "access-group-common-out", "common access list to apply to egress packets");
        l.add(null, false, 3, new int[]{-1}, "<name:acl>", "name of access list");
        l.add(null, false, 2, new int[]{3, -1}, "inspect", "enable packet inspection");
        l.add(null, false, 3, new int[]{4}, "timeout", "set timeout");
        l.add(null, false, 4, new int[]{3, -1}, "<num>", "timeout in ms");
        l.add(null, false, 3, new int[]{4}, "sessions", "set session limit");
        l.add(null, false, 4, new int[]{3, -1}, "<num>", "number of sessions");
        l.add(null, false, 3, new int[]{4}, "sample", "set sample value");
        l.add(null, false, 4, new int[]{3, -1}, "<num>", "one of every n packet");
        l.add(null, false, 3, new int[]{4}, "rate", "specify translation rate");
        l.add(null, false, 4, new int[]{3, -1}, "<nam:pm>", "name of policy map");
        l.add(null, false, 3, new int[]{3, -1}, "mac", "with mac addresses");
        l.add(null, false, 3, new int[]{3, -1}, "before", "log on session start");
        l.add(null, false, 3, new int[]{3, -1}, "after", "log on session stop");
        l.add(null, false, 3, new int[]{3, -1}, "dropped", "log dropped sessions");
        l.add(null, false, 3, new int[]{3, -1}, "drop-rx", "drop sessions ingress");
        l.add(null, false, 3, new int[]{3, -1}, "drop-tx", "drop sessions egress");
        l.add(null, false, 3, new int[]{3, -1}, "drop-frg", "drop fragmented packets");
        l.add(null, false, 3, new int[]{3, -1}, "allow-routing", "allow control multicast traffic");
        l.add(null, false, 3, new int[]{3, -1}, "allow-sending", "allow self originated traffic");
        l.add(null, false, 3, new int[]{3, -1}, "allow-linklocal", "allow link local traffic");
        l.add(null, false, 3, new int[]{3, -1}, "allow-multicast", "allow multicast traffic");
        l.add(null, false, 3, new int[]{3, -1}, "allow-broadcast", "allow broadcast traffic");
        l.add(null, false, 3, new int[]{4}, "allow-list", "allow specific traffic");
        l.add(null, false, 4, new int[]{3, -1}, "<name:acl>", "name of access list");
        l.add(null, false, 3, new int[]{4}, "allow-url", "allow specific traffic");
        l.add(null, false, 4, new int[]{3, -1}, "<num>", "translation rule");
        l.add(null, false, 3, new int[]{4}, "member", "member of inspection");
        l.add(null, false, 4, new int[]{5}, "<name:ses>", "name of session group");
        l.add(null, false, 5, new int[]{3, -1}, "<str>", "local identifier in the group");
        l.add(null, false, 2, new int[]{3}, "bfd", "enable bidirectional forwarding detection");
        l.add(null, false, 3, new int[]{4}, "<num>", "tx interval in ms");
        l.add(null, false, 4, new int[]{5}, "<num>", "rx interval in ms");
        l.add(null, false, 5, new int[]{6, -1}, "<num>", "multiplier");
        l.add(null, false, 6, new int[]{7}, "[num]", "key id");
        l.add(null, false, 7, new int[]{-1}, "<str>", "password");
        l.add(null, false, 2, new int[]{3}, "srh", "segment routing header commands");
        l.add(null, false, 3, new int[]{-1}, "enable", "enable/disable processing");
        l.add(null, false, 2, new int[]{3}, "nsh", "networt service header commands");
        l.add(null, false, 3, new int[]{-1}, "enable", "enable/disable processing");
        l.add(null, false, 2, new int[]{3}, "ptp", "precision time protococol commands");
        l.add(null, false, 3, new int[]{-1}, "enable", "enable/disable processing");
        l.add(null, false, 3, new int[]{-1}, "receive", "allow clock adjustment");
        l.add(null, false, 2, new int[]{3}, "tcp-mss-in", "rewrite tcp mss in ingress packets");
        l.add(null, false, 3, new int[]{-1}, "<num>", "value");
        l.add(null, false, 2, new int[]{3}, "tcp-mss-out", "rewrite tcp mss in egress packets");
        l.add(null, false, 3, new int[]{-1}, "<num>", "value");
        l.add(null, false, 2, new int[]{3}, "host-reach", "set next hop cache timeout");
        l.add(null, false, 3, new int[]{-1}, "<num>", "time in ms");
        l.add(null, false, 2, new int[]{3}, "host-retry", "set next hop cache retry");
        l.add(null, false, 3, new int[]{-1}, "<num>", "time in ms");
        l.add(null, false, 2, new int[]{3}, "host-rate", "set next hop cache query rate");
        l.add(null, false, 3, new int[]{4}, "<num>", "queries per interval");
        l.add(null, false, 4, new int[]{-1}, "<num>", "inetrval in ms");
        l.add(null, false, 2, new int[]{3}, "host-static", "set static next hop cache entry");
        l.add(null, false, 3, new int[]{4}, "<addr>", "ip address");
        l.add(null, false, 4, new int[]{-1}, "<addr>", "mac address");
        l.add(null, false, 2, new int[]{3}, "host-remote", "set static remote peer");
        l.add(null, false, 3, new int[]{-1}, "<addr>", "address");
        l.add(null, false, 2, new int[]{-1}, "host-learn", "allow next hop learn");
        l.add(null, false, 2, new int[]{3}, "dlep", "local next hop changes");
        l.add(null, false, 3, new int[]{-1}, "client", "report next hop changes");
        l.add(null, false, 3, new int[]{-1}, "server", "accept next hop changes");
        l.add(null, false, 2, new int[]{3, -1}, "host-watch", "monitor next hop changes");
        l.add(null, false, 3, new int[]{4}, "appear", "script on appearance");
        l.add(null, false, 4, new int[]{3, -1}, "<name:scr>", "name of script");
        l.add(null, false, 3, new int[]{4}, "change", "script on mac change");
        l.add(null, false, 4, new int[]{3, -1}, "<name:scr>", "name of script");
        l.add(null, false, 3, new int[]{4}, "disappear", "script on disappearance");
        l.add(null, false, 4, new int[]{3, -1}, "<name:scr>", "name of script");
        l.add(null, false, 2, new int[]{3}, "multicast", "multicast configuration options");
        l.add(null, false, 3, new int[]{-1}, "broadcast", "broadcast the packets");
        l.add(null, false, 3, new int[]{-1}, "unicast", "unicast the packets");
        l.add(null, false, 3, new int[]{-1}, "mldp-enable", "enable mdlp processing");
        l.add(null, false, 3, new int[]{-1}, "host-enable", "enable igmp/mld processing");
        l.add(null, false, 3, new int[]{-1}, "host-proxy", "send joins for groups");
        l.add(null, false, 3, new int[]{4}, "host-query", "time between queries");
        l.add(null, false, 4, new int[]{-1}, "<num>", "time in ms");
        l.add(null, false, 3, new int[]{4}, "ttl-threshold", "ttl threshold for multicast packets");
        l.add(null, false, 4, new int[]{-1}, "<num>", "ttl");
        l.add(null, false, 3, new int[]{4}, "source-override-in", "override received source for groups");
        l.add(null, false, 4, new int[]{-1}, "<addr>", "source");
        l.add(null, false, 3, new int[]{4}, "source-override-out", "override sent source for groups");
        l.add(null, false, 4, new int[]{-1}, "<addr>", "source");
        l.add(null, false, 3, new int[]{4}, "static-group", "unconditional flooding");
        l.add(null, false, 4, new int[]{5}, "<addr>", "group address");
        l.add(null, false, 5, new int[]{-1}, "<addr>", "source address");
        l.add(null, false, 2, new int[]{-1}, "proxy-remote", "reply to remote networks");
        l.add(null, false, 2, new int[]{-1}, "proxy-local", "reply to link address");
        l.add(null, false, 2, new int[]{3}, "proxy-filter", "access list to filter replies");
        l.add(null, false, 3, new int[]{-1}, "<name:acl>", "name of access list");
        l.add(null, false, 2, new int[]{3}, "autoroute", "point routes from routing protocol");
        cfgRtr.getRouterList(l, 1, "");
        l.add(null, false, 4, new int[]{5}, "<num:rtr>", "process id");
        l.add(null, false, 5, new int[]{6}, "<addr>", "source router");
        l.add(null, false, 6, new int[]{7, -1}, "<addr>", "nexthop");
        l.add(null, false, 7, new int[]{7, -1}, "recursive", "process bgp routes");
        l.add(null, false, 7, new int[]{7, -1}, "multicast", "process multicast table");
        l.add(null, false, 7, new int[]{7, -1}, "no-unicast", "dont process unicast table");
        l.add(null, false, 7, new int[]{7, -1}, "exclude-match", "exclude matching prefix");
        l.add(null, false, 7, new int[]{8}, "include-list", "include matching prefix");
        l.add(null, false, 8, new int[]{7, -1}, "<name:pl>", "name of prefix list");
        l.add(null, false, 7, new int[]{8}, "include-map", "include matching prefix");
        l.add(null, false, 8, new int[]{7, -1}, "<name:rm>", "name of route map");
        l.add(null, false, 7, new int[]{8}, "include-policy", "include matching prefix");
        l.add(null, false, 8, new int[]{7, -1}, "<name:rpl>", "name of route map");
        l.add(null, false, 2, new int[]{3}, "pim", "pim configuration options");
        l.add(null, false, 3, new int[]{-1}, "enable", "enable pim processing");
        l.add(null, false, 3, new int[]{-1}, "bfd", "enable bfd triggered down");
        l.add(null, false, 3, new int[]{-1}, "allow-rx", "suppress processing routing updates");
        l.add(null, false, 3, new int[]{-1}, "allow-tx", "suppress sending routing updates");
        l.add(null, false, 3, new int[]{4}, "bier-tunnel", "use bier encapsulation");
        l.add(null, false, 4, new int[]{-1}, "<num>", "this node index");
        l.add(null, false, 3, new int[]{4}, "join-source", "set join source");
        l.add(null, false, 4, new int[]{-1}, "<name:ifc>", "name of interface");
        l.add(null, false, 3, new int[]{4}, "packet-timer", "inter packet gap time");
        l.add(null, false, 4, new int[]{-1}, "<num>", "time in ms");
        l.add(null, false, 3, new int[]{4}, "priority", "router priority");
        l.add(null, false, 4, new int[]{-1}, "<num>", "priority 0=disable");
        l.add(null, false, 3, new int[]{4}, "hello-time", "time between hellos");
        l.add(null, false, 4, new int[]{-1}, "<num>", "time in ms");
        l.add(null, false, 2, new int[]{3}, "hsrp", "enable hsrp processing");
        l.add(null, false, 3, new int[]{4}, "address", "set virtual ip address");
        l.add(null, false, 4, new int[]{-1}, "<addr>", "address to use");
        l.add(null, false, 3, new int[]{4}, "mac-address", "set virtual mac address");
        l.add(null, false, 4, new int[]{-1}, "<addr>", "address to use");
        l.add(null, false, 3, new int[]{-1}, "bfd", "enable bfd triggered down");
        l.add(null, false, 3, new int[]{4}, "group", "set group number");
        l.add(null, false, 4, new int[]{-1}, "<num>", "group");
        l.add(null, false, 3, new int[]{4}, "password", "set group password");
        l.add(null, false, 4, new int[]{-1}, "<str>", "string");
        l.add(null, false, 3, new int[]{4}, "version", "set protocol version");
        l.add(null, false, 4, new int[]{-1}, "<num>", "group");
        l.add(null, false, 3, new int[]{4}, "timer", "set protocol timers");
        l.add(null, false, 4, new int[]{5}, "<num>", "hello time in ms");
        l.add(null, false, 5, new int[]{-1}, "<num>", "hold time in ms");
        l.add(null, false, 3, new int[]{-1}, "preempt", "overbid lower priority ones");
        l.add(null, false, 3, new int[]{4}, "priority", "set priority level");
        l.add(null, false, 4, new int[]{-1}, "<num>", "priority");
        l.add(null, false, 3, new int[]{4}, "tracker", "set tracker to use");
        l.add(null, false, 4, new int[]{5}, "<name:trk>", "name of tracker");
        l.add(null, false, 5, new int[]{-1}, "<num>", "decrement value");
        l.add(null, false, 2, new int[]{3}, "vrrp", "enable vrrp processing");
        l.add(null, false, 3, new int[]{4}, "address", "set virtual ip address");
        l.add(null, false, 4, new int[]{-1}, "<addr>", "address to use");
        l.add(null, false, 3, new int[]{4}, "mac-address", "set virtual mac address");
        l.add(null, false, 4, new int[]{-1}, "<addr>", "address to use");
        l.add(null, false, 3, new int[]{-1}, "bfd", "enable bfd triggered down");
        l.add(null, false, 3, new int[]{4}, "group", "set group number");
        l.add(null, false, 4, new int[]{-1}, "<num>", "group");
        l.add(null, false, 3, new int[]{4}, "version", "set protocol version");
        l.add(null, false, 4, new int[]{-1}, "<num>", "group");
        l.add(null, false, 3, new int[]{4}, "timer", "set protocol timers");
        l.add(null, false, 4, new int[]{5}, "<num>", "hello time in ms");
        l.add(null, false, 5, new int[]{-1}, "<num>", "hold time in ms");
        l.add(null, false, 3, new int[]{4}, "priority", "set priority level");
        l.add(null, false, 4, new int[]{-1}, "<num>", "priority");
        l.add(null, false, 3, new int[]{4}, "tracker", "set tracker to use");
        l.add(null, false, 4, new int[]{5}, "<name:trk>", "name of tracker");
        l.add(null, false, 5, new int[]{-1}, "<num>", "decrement value");
        l.add(null, false, 2, new int[]{3, 5}, "pbr", "configure policy based routing");
        l.add(null, false, 3, new int[]{4, -1}, "reindex", "reindex pbrs");
        l.add(null, false, 4, new int[]{6, -1}, "[num]", "initial number to start with");
        l.add(null, false, 6, new int[]{-1}, "[num]", "increment number");
        l.add(null, false, 3, new int[]{4}, "sequence", "sequence number");
        l.add(null, false, 4, new int[]{5}, "<num>", "number");
        l.add(null, false, 5, new int[]{6}, "<name:acl>", "access list name");
        l.add(null, false, 6, new int[]{7, -1}, "<name:vrf>", "target vrf");
        l.add(null, false, 7, new int[]{8}, "interface", "set target interface");
        l.add(null, false, 8, new int[]{7, -1}, "<name:ifc>", "interface name");
        l.add(null, false, 7, new int[]{8}, "nexthop", "set target address");
        l.add(null, false, 8, new int[]{7, -1}, "<addr>", "target address");
        l.add(null, false, 7, new int[]{8}, "nsh", "set target service");
        l.add(null, false, 8, new int[]{9}, "<num>", "service path");
        l.add(null, false, 9, new int[]{7, -1}, "<num>", "service index");
    }

    /**
     * get configuration
     *
     * @param l storage
     * @param f forwarder
     * @param beg beginning
     * @param filter filter text
     */
    public void getConfig(List<String> l, ipFwd f, String beg, int filter) {
        cmds.cfgLine(l, !linkLocal, cmds.tabulator, beg + "enable", "");
        cmds.cfgLine(l, !unreachEna, cmds.tabulator, beg + "unreachables", "");
        cmds.cfgLine(l, !netflowRx, cmds.tabulator, beg + "netflow-rx", "");
        cmds.cfgLine(l, !netflowTx, cmds.tabulator, beg + "netflow-tx", "");
        cmds.cfgLine(l, !mplsPropTtlAlways, cmds.tabulator, beg + "propagate-ttl-always", "");
        cmds.cfgLine(l, !mplsPropTtlAllow, cmds.tabulator, beg + "propagate-ttl-allow", "");
        cmds.cfgLine(l, unreachSrc == null, cmds.tabulator, beg + "unreach-source", "" + unreachSrc);
        cmds.cfgLine(l, blockHost2host, cmds.tabulator, beg + "resend-packet", "");
        cmds.cfgLine(l, !disableFlowspec, cmds.tabulator, beg + "flowspec-disable", "");
        cmds.cfgLine(l, !disableDapp, cmds.tabulator, beg + "dapp-disable", "");
        String a = "";
        if (verifySource) {
            a = "any";
        }
        if (verifyStricht) {
            a = "rx";
        }
        cmds.cfgLine(l, !verifySource, cmds.tabulator, beg + "verify-source", a);
        cmds.cfgLine(l, !answerNetReqs, cmds.tabulator, beg + "proxy-local", "");
        cmds.cfgLine(l, !answerDefReqs, cmds.tabulator, beg + "proxy-remote", "");
        cmds.cfgLine(l, answerFilter == null, cmds.tabulator, beg + "proxy-filter", "" + answerFilter);
        for (int i = 0; i < adrs.size(); i++) {
            ipFwdIfaceAddr adr = adrs.get(i);
            if (adr == null) {
                continue;
            }
            if (!adr.cfg) {
                continue;
            }
            l.add(cmds.tabulator + beg + "secondary-address " + adr.ip);
        }
        for (int i = 0; i < nets.size(); i++) {
            ipFwdIfacePref adr = nets.get(i);
            if (adr == null) {
                continue;
            }
            l.add(cmds.tabulator + beg + "secondary-network " + adr.adr + " " + adr.msk);
        }
        if (reasmBuf == null) {
            l.add(cmds.tabulator + cmds.negated + cmds.tabulator + beg + "reassembly");
        } else {
            l.add(cmds.tabulator + beg + "reassembly " + reasmBuf.size());
        }
        cmds.cfgLine(l, fragments < 1, cmds.tabulator, beg + "fragmentation", "" + fragments);
        cmds.cfgLine(l, pmtudIn < 1, cmds.tabulator, beg + "pmtud-in", "" + pmtudIn);
        cmds.cfgLine(l, pmtudOut < 1, cmds.tabulator, beg + "pmtud-out", "" + pmtudOut);
        cmds.cfgLine(l, !gateCon, cmds.tabulator, beg + "gateway-connected", "");
        cmds.cfgLine(l, !gateLoc, cmds.tabulator, beg + "gateway-local", "");
        cmds.cfgLine(l, !gateRem, cmds.tabulator, beg + "gateway-remote", "");
        cmds.cfgLine(l, !gatePrc, cmds.tabulator, beg + "gateway-process", "");
        l.add(cmds.tabulator + beg + "gateway-distance " + gateDstC + " " + gateDstL + " " + gateDstR + " " + gateDstP);
        switch (gateLab) {
            case 0:
                a = "unlabeled";
                break;
            case 1:
                a = "implicit-null";
                break;
            case 2:
                a = "explicit-null";
                break;
            default:
                a = "unknown=" + gateLab;
                break;
        }
        cmds.cfgLine(l, gateLab == 0, cmds.tabulator, beg + "gateway-labeled", a);
        cmds.cfgLine(l, gatePrfx == null, cmds.tabulator, beg + "gateway-prefix", "" + gatePrfx);
        cmds.cfgLine(l, gateRtmp == null, cmds.tabulator, beg + "gateway-map", "" + gateRtmp);
        cmds.cfgLine(l, gateRplc == null, cmds.tabulator, beg + "gateway-policy", "" + gateRplc);
        cmds.cfgLine(l, cfilterIn == null, cmds.tabulator, beg + "access-group-common-in", "" + cfilterIn);
        cmds.cfgLine(l, cfilterOut == null, cmds.tabulator, beg + "access-group-common-out", "" + cfilterOut);
        cmds.cfgLine(l, filterIn == null, cmds.tabulator, beg + "access-group-in", "" + filterIn);
        cmds.cfgLine(l, filterOut == null, cmds.tabulator, beg + "access-group-out", "" + filterOut);
        cmds.cfgLine(l, inspect == null, cmds.tabulator, beg + "inspect", "" + inspect);
        a = "";
        if (autRouRec) {
            a += " recursive";
        }
        if (autRouMcst) {
            a += " multicast";
        }
        if (autRouUnic) {
            a += " no-unicast";
        }
        if (autRouExcld) {
            a += " exclude-match";
        }
        if (autRouPfxlst != null) {
            a += " include-list " + autRouPfxlst.listName;
        }
        if (autRouRoumap != null) {
            a += " include-map " + autRouRoumap.listName;
        }
        if (autRouRoupol != null) {
            a += " include-policy " + autRouRoupol.listName;
        }
        cmds.cfgLine(l, autRouTyp == null, cmds.tabulator, beg + "autoroute", "" + autRouTyp + " " + autRouPrt + " " + autRouRtr + " " + autRouHop + a);
        cmds.cfgLine(l, !lower.getCacheDynmc(), cmds.tabulator, beg + "host-learn", "");
        cmds.cfgLine(l, hostWatch == null, cmds.tabulator, beg + "host-watch", "" + hostWatch);
        cmds.cfgLine(l, dlepCfg == null, cmds.tabulator, beg + "dlep", "" + dlepCfg);
        tabRateLimit hostRate = lower.getCacheRate();
        cmds.cfgLine(l, hostRate == null, cmds.tabulator, beg + "host-rate", "" + hostRate);
        l.add(cmds.tabulator + beg + "host-reach " + lower.getCacheTimer());
        l.add(cmds.tabulator + beg + "host-retry " + lower.getCacheRetry());
        cmds.cfgLine(l, hostRemote == null, cmds.tabulator, beg + "host-remote", "" + hostRemote);
        lower.getL2info(l, cmds.tabulator + beg + "host-static ");
        for (int i = 0; i < pbrCfg.size(); i++) {
            tabPbrN pbr = pbrCfg.get(i);
            l.addAll(pbr.usrString(cmds.tabulator + beg + "pbr ", filter));
        }
        if (srhCfg == null) {
            l.add(cmds.tabulator + cmds.negated + cmds.tabulator + beg + "srh enable");
        } else {
            l.add(cmds.tabulator + beg + "srh enable");
        }
        if (nshCfg == null) {
            l.add(cmds.tabulator + cmds.negated + cmds.tabulator + beg + "nsh enable");
        } else {
            l.add(cmds.tabulator + beg + "nsh enable");
        }
        if (ptpCfg == null) {
            l.add(cmds.tabulator + cmds.negated + cmds.tabulator + beg + "ptp enable");
        } else {
            l.add(cmds.tabulator + beg + "ptp enable");
            cmds.cfgLine(l, !ptpCfg.receive, cmds.tabulator, beg + "ptp receive", "");
        }
        if (bfdCfg == null) {
            l.add(cmds.tabulator + cmds.negated + cmds.tabulator + beg + "bfd");
        } else {
            a = "";
            if (bfdCfg.password != null) {
                a += " " + bfdCfg.keyId + " " + authLocal.passwdEncode(bfdCfg.password, (filter & 2) != 0);
            }
            l.add(cmds.tabulator + beg + "bfd " + bfdCfg.intervalTx + " " + bfdCfg.intervalRx + " " + bfdCfg.multiplier + a);
        }
        cmds.cfgLine(l, tcpMssIn < 1, cmds.tabulator, beg + "tcp-mss-in", "" + tcpMssIn);
        cmds.cfgLine(l, tcpMssOut < 1, cmds.tabulator, beg + "tcp-mss-out", "" + tcpMssOut);
        cmds.cfgLine(l, !mcastAsBcast, cmds.tabulator, beg + "multicast broadcast", "");
        cmds.cfgLine(l, !mcastAsUcast, cmds.tabulator, beg + "multicast unicast", "");
        for (int o = 0; o < f.groups.size(); o++) {
            ipFwdMcast grp = f.groups.get(o);
            if (grp == null) {
                continue;
            }
            if (!grp.configI) {
                continue;
            }
            for (int i = 0; i < grp.flood.size(); i++) {
                ipFwdIface fld = grp.flood.get(i);
                if (ifwNum != fld.ifwNum) {
                    continue;
                }
                l.add(cmds.tabulator + beg + "multicast static-group " + grp.group + " " + grp.source);
            }
        }
        l.add(cmds.tabulator + beg + "multicast ttl-threshold " + mcastTtl);
        cmds.cfgLine(l, mcastSrcIn == null, cmds.tabulator, beg + "multicast source-override-in", "" + mcastSrcIn);
        cmds.cfgLine(l, mcastSrcOut == null, cmds.tabulator, beg + "multicast source-override-out", "" + mcastSrcOut);
        if (pimCfg != null) {
            l.add(cmds.tabulator + beg + "pim enable");
            l.add(cmds.tabulator + beg + "pim bier-tunnel " + pimCfg.bierTunnel);
            cmds.cfgLine(l, !pimCfg.allowRx, cmds.tabulator, beg + "pim allow-rx", "");
            cmds.cfgLine(l, !pimCfg.allowTx, cmds.tabulator, beg + "pim allow-tx", "");
            cmds.cfgLine(l, pimCfg.joinSource == null, cmds.tabulator, beg + "pim join-source", "" + pimCfg.joinSource);
            l.add(cmds.tabulator + beg + "pim packet-timer " + pimCfg.interPackTime);
            l.add(cmds.tabulator + beg + "pim priority " + pimCfg.drPriority);
            l.add(cmds.tabulator + beg + "pim hello-time " + pimCfg.helloInterval);
            cmds.cfgLine(l, !pimCfg.bfdTrigger, cmds.tabulator, beg + "pim bfd", "");
        } else {
            l.add(cmds.tabulator + cmds.negated + cmds.tabulator + beg + "pim enable");
        }
        if (mhostCfg != null) {
            l.add(cmds.tabulator + beg + "multicast host-enable");
            cmds.cfgLine(l, !mhostCfg.sendJoins, cmds.tabulator, beg + "multicast host-proxy", "");
            l.add(cmds.tabulator + beg + "multicast host-query " + mhostCfg.queryInterval);
        } else {
            l.add(cmds.tabulator + cmds.negated + cmds.tabulator + beg + "multicast host-enable");
        }
        cmds.cfgLine(l, mldpCfg == null, cmds.tabulator, beg + "multicast mldp-enable", "");
        if (hsrpCfg != null) {
            l.add(cmds.tabulator + beg + "hsrp address " + hsrpCfg.ip);
            l.add(cmds.tabulator + beg + "hsrp password " + authLocal.passwdEncode(hsrpCfg.authen, (filter & 2) != 0));
            l.add(cmds.tabulator + beg + "hsrp group " + hsrpCfg.group);
            l.add(cmds.tabulator + beg + "hsrp mac-address " + hsrpCfg.mac);
            l.add(cmds.tabulator + beg + "hsrp version " + hsrpCfg.version);
            l.add(cmds.tabulator + beg + "hsrp timer " + hsrpCfg.hello + " " + hsrpCfg.hold);
            l.add(cmds.tabulator + beg + "hsrp priority " + hsrpCfg.priority);
            cmds.cfgLine(l, !hsrpCfg.preempt, cmds.tabulator, beg + "hsrp preempt", "");
            cmds.cfgLine(l, hsrpCfg.trackR == null, cmds.tabulator, beg + "hsrp tracker", hsrpCfg.trackR + " " + hsrpCfg.trackD);
            cmds.cfgLine(l, !hsrpCfg.bfdTrigger, cmds.tabulator, beg + "hsrp bfd", "");
        } else {
            l.add(cmds.tabulator + cmds.negated + cmds.tabulator + beg + "hsrp address");
        }
        if (vrrpCfg != null) {
            l.add(cmds.tabulator + beg + "vrrp address " + vrrpCfg.ip);
            l.add(cmds.tabulator + beg + "vrrp group " + vrrpCfg.group);
            l.add(cmds.tabulator + beg + "vrrp mac-address " + vrrpCfg.mac);
            l.add(cmds.tabulator + beg + "vrrp version " + vrrpCfg.version);
            l.add(cmds.tabulator + beg + "vrrp timer " + vrrpCfg.hello + " " + vrrpCfg.hold);
            l.add(cmds.tabulator + beg + "vrrp priority " + vrrpCfg.priority);
            cmds.cfgLine(l, vrrpCfg.trackR == null, cmds.tabulator, beg + "vrrp tracker", vrrpCfg.trackR + " " + vrrpCfg.trackD);
            cmds.cfgLine(l, !vrrpCfg.bfdTrigger, cmds.tabulator, beg + "vrrp bfd", "");
        } else {
            l.add(cmds.tabulator + cmds.negated + cmds.tabulator + beg + "vrrp address");
        }
    }

    /**
     * do configuration
     *
     * @param a command
     * @param cmd commands
     * @param cor ip core
     * @param fwd forwarder core
     * @param udp udp core
     * @param tcp tcp core
     * @return result code, true on error, false on success
     */
    public boolean doConfig(String a, cmds cmd, ipCor cor, ipFwd fwd, prtUdp udp, prtTcp tcp) {
        if (a.equals("enable")) {
            linkLocal = true;
            fwd.routerStaticChg();
            return false;
        }
        if (a.equals("secondary-address")) {
            addrIP adr = new addrIP();
            if (adr.fromString(cmd.word())) {
                cmd.error("bad ip address");
                return false;
            }
            adrAdd(adr, (addrMac) lower.getL2info(), true);
            return false;
        }
        if (a.equals("secondary-network")) {
            ipFwdIfacePref ntry = new ipFwdIfacePref();
            if (ntry.fromString(cmd)) {
                return false;
            }
            adrAdd(ntry.adr, (addrMac) lower.getL2info(), false);
            nets.add(ntry);
            fwd.routerStaticChg();
            return false;
        }
        if (a.equals("fragmentation")) {
            fragments = bits.str2num(cmd.word()) & (-1 - 7);
            return false;
        }
        if (a.equals("pmtud-in")) {
            pmtudIn = bits.str2num(cmd.word());
            return false;
        }
        if (a.equals("pmtud-out")) {
            pmtudOut = bits.str2num(cmd.word());
            return false;
        }
        if (a.equals("reassembly")) {
            int o = bits.str2num(cmd.word());
            reasmBuf = new ArrayList<packHolder>();
            for (int i = 0; i < o; i++) {
                reasmBuf.add(new packHolder(true, true));
            }
            return false;
        }
        if (a.equals("netflow-rx")) {
            netflowRx = true;
            return false;
        }
        if (a.equals("netflow-tx")) {
            netflowTx = true;
            return false;
        }
        if (a.equals("propagate-ttl-always")) {
            mplsPropTtlAlways = true;
            return false;
        }
        if (a.equals("propagate-ttl-allow")) {
            mplsPropTtlAllow = true;
            return false;
        }
        if (a.equals("unreachables")) {
            unreachEna = true;
            return false;
        }
        if (a.equals("proxy-remote")) {
            answerDefReqs = true;
            return false;
        }
        if (a.equals("proxy-local")) {
            answerNetReqs = true;
            return false;
        }
        if (a.equals("proxy-filter")) {
            cfgAceslst ntry = cfgAll.aclsFind(cmd.word(), false);
            if (ntry == null) {
                cmd.error("no such access list");
                return false;
            }
            answerFilter = ntry.aceslst;
            return false;
        }
        if (a.equals("host-remote")) {
            hostRemote = new addrIP();
            hostRemote.fromString(cmd.word());
            fwd.routerStaticChg();
            return false;
        }
        if (a.equals("host-learn")) {
            lower.setCacheDynmc(true);
            return false;
        }
        if (a.equals("dlep")) {
            if (dlepCfg != null) {
                dlepCfg.stopWork();
            }
            a = cmd.word();
            dlepCfg = new ipDlepIface(fwd, udp, tcp, this, lower, a.equals("client"));
            return false;
        }
        if (a.equals("host-watch")) {
            if (hostWatch != null) {
                hostWatch.stopWork();
            }
            hostWatch = new ipHostWatch(lower);
            for (;;) {
                a = cmd.word();
                if (a.length() < 1) {
                    break;
                }
                if (a.equals("appear")) {
                    hostWatch.nodeOn = cfgAll.scrptFind(cmd.word(), false);
                    continue;
                }
                if (a.equals("change")) {
                    hostWatch.nodeChg = cfgAll.scrptFind(cmd.word(), false);
                    continue;
                }
                if (a.equals("disappear")) {
                    hostWatch.nodeOff = cfgAll.scrptFind(cmd.word(), false);
                    continue;
                }
            }
            return false;
        }
        if (a.equals("host-static")) {
            addrIP adr = new addrIP();
            if (adr.fromString(cmd.word())) {
                cmd.error("bad ip address");
                return false;
            }
            addrMac mac = new addrMac();
            if (mac.fromString(cmd.word())) {
                cmd.error("bad mac address");
                return false;
            }
            lower.updateL2info(1, mac, adr);
            return false;
        }
        if (a.equals("autoroute")) {
            a = cmd.word();
            autRouPrt = bits.str2num(cmd.word());
            autRouRtr = new addrIP();
            autRouRtr.fromString(cmd.word());
            autRouHop = new addrIP();
            autRouHop.fromString(cmd.word());
            autRouTyp = cfgRtr.name2num(a);
            autRouRec = false;
            autRouMcst = false;
            autRouUnic = false;
            autRouExcld = false;
            autRouPfxlst = null;
            autRouRoumap = null;
            autRouRoupol = null;
            for (;;) {
                a = cmd.word();
                if (a.length() < 1) {
                    break;
                }
                if (a.equals("recursive")) {
                    autRouRec = true;
                    continue;
                }
                if (a.equals("multicast")) {
                    autRouMcst = true;
                    continue;
                }
                if (a.equals("no-unicast")) {
                    autRouUnic = true;
                    continue;
                }
                if (a.equals("exclude-match")) {
                    autRouExcld = true;
                    continue;
                }
                if (a.equals("include-list")) {
                    cfgPrfxlst ntry = cfgAll.prfxFind(cmd.word(), false);
                    if (ntry == null) {
                        continue;
                    }
                    autRouPfxlst = ntry.prflst;
                    continue;
                }
                if (a.equals("include-map")) {
                    cfgRoump ntry = cfgAll.rtmpFind(cmd.word(), false);
                    if (ntry == null) {
                        continue;
                    }
                    autRouRoumap = ntry.roumap;
                    continue;
                }
                if (a.equals("include-policy")) {
                    cfgRouplc ntry = cfgAll.rtplFind(cmd.word(), false);
                    if (ntry == null) {
                        continue;
                    }
                    autRouRoupol = ntry.rouplc;
                    continue;
                }
            }
            fwd.routerStaticChg();
            return false;
        }
        if (a.equals("host-reach")) {
            lower.setCacheTimer(bits.str2num(cmd.word()));
            return false;
        }
        if (a.equals("host-rate")) {
            int res = bits.str2num(cmd.word());
            lower.setCacheRate(new tabRateLimit(res, bits.str2num(cmd.word())));
            return false;
        }
        if (a.equals("host-retry")) {
            lower.setCacheRetry(bits.str2num(cmd.word()));
            return false;
        }
        if (a.equals("unreach-source")) {
            cfgIfc ntry = cfgAll.ifcFind(cmd.word(), 0);
            if (ntry == null) {
                cmd.error("no such interface");
                return false;
            }
            ipFwdIface src = ntry.getFwdIfc(addr);
            if (src == null) {
                cmd.error("protocol not enabled");
                return false;
            }
            unreachSrc = src;
            return false;
        }
        if (a.equals("resend-packet")) {
            blockHost2host = false;
            return false;
        }
        if (a.equals("flowspec-disable")) {
            disableFlowspec = true;
            return false;
        }
        if (a.equals("dapp-disable")) {
            disableDapp = true;
            return false;
        }
        if (a.equals("verify-source")) {
            a = cmd.word();
            if (a.equals("rx")) {
                verifySource = true;
                verifyStricht = true;
                return false;
            }
            if (a.equals("any")) {
                verifySource = true;
                verifyStricht = false;
                return false;
            }
            if (a.equals("none")) {
                verifySource = false;
                verifyStricht = false;
                return false;
            }
            cmd.error("invalid mode");
            return false;
        }
        if (a.equals("gateway-connected")) {
            gateCon = true;
            fwd.routerStaticChg();
            return false;
        }
        if (a.equals("gateway-local")) {
            gateLoc = true;
            fwd.routerStaticChg();
            return false;
        }
        if (a.equals("gateway-remote")) {
            gateRem = true;
            fwd.routerStaticChg();
            return false;
        }
        if (a.equals("gateway-process")) {
            gatePrc = true;
            return false;
        }
        if (a.equals("gateway-distance")) {
            gateDstC = bits.str2num(cmd.word());
            gateDstL = bits.str2num(cmd.word());
            gateDstR = bits.str2num(cmd.word());
            gateDstP = bits.str2num(cmd.word());
            fwd.routerStaticChg();
            return false;
        }
        if (a.equals("gateway-labeled")) {
            a = cmd.word();
            gateLab = 0;
            if (a.equals("unlabeled")) {
                gateLab = 0;
            }
            if (a.equals("implicit-null")) {
                gateLab = 1;
            }
            if (a.equals("explicit-null")) {
                gateLab = 2;
            }
            fwd.routerStaticChg();
            return false;
        }
        if (a.equals("gateway-prefix")) {
            cfgPrfxlst ntry = cfgAll.prfxFind(cmd.word(), false);
            if (ntry == null) {
                cmd.error("no such prefix list");
                return false;
            }
            gatePrfx = ntry.prflst;
            fwd.routerStaticChg();
            return false;
        }
        if (a.equals("gateway-map")) {
            cfgRoump ntry = cfgAll.rtmpFind(cmd.word(), false);
            if (ntry == null) {
                cmd.error("no such route map");
                return false;
            }
            gateRtmp = ntry.roumap;
            fwd.routerStaticChg();
            return false;
        }
        if (a.equals("gateway-policy")) {
            cfgRouplc ntry = cfgAll.rtplFind(cmd.word(), false);
            if (ntry == null) {
                cmd.error("no such route map");
                return false;
            }
            gateRplc = ntry.rouplc;
            fwd.routerStaticChg();
            return false;
        }
        if (a.equals("access-group-in")) {
            cfgAceslst ntry = cfgAll.aclsFind(cmd.word(), false);
            if (ntry == null) {
                cmd.error("no such access list");
                return false;
            }
            ntry.aceslst.myCor = cor;
            ntry.aceslst.myIcmp = fwd.icmpCore;
            filterIn = ntry.aceslst;
            return false;
        }
        if (a.equals("access-group-out")) {
            cfgAceslst ntry = cfgAll.aclsFind(cmd.word(), false);
            if (ntry == null) {
                cmd.error("no such access list");
                return false;
            }
            ntry.aceslst.myCor = cor;
            ntry.aceslst.myIcmp = fwd.icmpCore;
            filterOut = ntry.aceslst;
            return false;
        }
        if (a.equals("access-group-common-in")) {
            cfgAceslst ntry = cfgAll.aclsFind(cmd.word(), false);
            if (ntry == null) {
                cmd.error("no such access list");
                return false;
            }
            ntry.aceslst.myCor = cor;
            ntry.aceslst.myIcmp = fwd.icmpCore;
            cfilterIn = ntry.aceslst;
            return false;
        }
        if (a.equals("access-group-common-out")) {
            cfgAceslst ntry = cfgAll.aclsFind(cmd.word(), false);
            if (ntry == null) {
                cmd.error("no such access list");
                return false;
            }
            ntry.aceslst.myCor = cor;
            ntry.aceslst.myIcmp = fwd.icmpCore;
            cfilterOut = ntry.aceslst;
            return false;
        }
        if (a.equals("tcp-mss-in")) {
            tcpMssIn = bits.str2num(cmd.word());
            return false;
        }
        if (a.equals("tcp-mss-out")) {
            tcpMssOut = bits.str2num(cmd.word());
            return false;
        }
        if (a.equals("pbr")) {
            pbrCfg.myCor = cor;
            pbrCfg.myIcmp = fwd.icmpCore;
            tabPbrN ntry = new tabPbrN();
            String s = cmd.getRemaining();
            a = cmd.word();
            if (a.equals("reindex")) {
                int i = bits.str2num(cmd.word());
                pbrCfg.reindex(i, bits.str2num(cmd.word()));
                return false;
            }
            ntry.sequence = pbrCfg.nextseq();
            if (ntry.fromString(fwd.ipVersion, s)) {
                return true;
            }
            ntry.matcher.copyCores(pbrCfg);
            pbrCfg.add(ntry);
            return false;
        }
        if (a.equals("multicast")) {
            a = cmd.word();
            if (a.equals("broadcast")) {
                mcastAsBcast = true;
                return false;
            }
            if (a.equals("unicast")) {
                mcastAsUcast = true;
                return false;
            }
            if (a.equals("source-override-in")) {
                mcastSrcIn = new addrIP();
                mcastSrcIn.fromString(cmd.word());
                return false;
            }
            if (a.equals("source-override-out")) {
                mcastSrcOut = new addrIP();
                mcastSrcOut.fromString(cmd.word());
                return false;
            }
            if (a.equals("ttl-threshold")) {
                mcastTtl = bits.str2num(cmd.word());
                return false;
            }
            if (a.equals("static-group")) {
                addrIP a1 = new addrIP();
                addrIP a2 = new addrIP();
                if (a1.fromString(cmd.word())) {
                    cmd.error("bad group address");
                    return false;
                }
                if (!a1.isMulticast()) {
                    cmd.error("not a multicast address");
                    return false;
                }
                if (a2.fromString(cmd.word())) {
                    cmd.error("bad source address");
                    return false;
                }
                fwd.mcastAddFloodIfc(a1, a2, this, -3);
                return false;
            }
            if (a.equals("mldp-enable")) {
                mldpCfg = new ipMldpIface(fwd, this);
                return false;
            }
            if (a.equals("host-enable")) {
                mhostCfg = new ipMhostIface(fwd, this);
                mhostCfg.restartTimer(false);
                return false;
            }
            if (mhostCfg == null) {
                cmd.error("protocol not running");
                return false;
            }
            if (a.equals("host-query")) {
                mhostCfg.queryInterval = bits.str2num(cmd.word());
                mhostCfg.restartTimer(false);
                return false;
            }
            if (a.equals("host-proxy")) {
                mhostCfg.sendJoins = true;
                return false;
            }
            return true;
        }
        if (a.equals("pim")) {
            a = cmd.word();
            if (a.equals("enable")) {
                pimCfg = new rtrPimIface(fwd, this);
                pimCfg.register2ip();
                return false;
            }
            if (pimCfg == null) {
                cmd.error("protocol not running");
                return false;
            }
            if (a.equals("bfd")) {
                pimCfg.bfdTrigger = true;
                return false;
            }
            if (a.equals("bier-tunnel")) {
                pimCfg.bierTunnel = bits.str2num(cmd.word());
                return false;
            }
            if (a.equals("allow-rx")) {
                pimCfg.allowRx = true;
                return false;
            }
            if (a.equals("allow-tx")) {
                pimCfg.allowTx = true;
                return false;
            }
            if (a.equals("join-source")) {
                cfgIfc ntry = cfgAll.ifcFind(cmd.word(), 0);
                if (ntry == null) {
                    cmd.error("no such interface");
                    return false;
                }
                ipFwdIface src = ntry.getFwdIfc(addr);
                if (src == null) {
                    cmd.error("protocol not enabled");
                    return false;
                }
                pimCfg.joinSource = src;
                return false;
            }
            if (a.equals("packet-timer")) {
                pimCfg.interPackTime = bits.str2num(cmd.word());
                return false;
            }
            if (a.equals("priority")) {
                pimCfg.drPriority = bits.str2num(cmd.word());
                return false;
            }
            if (a.equals("hello-time")) {
                pimCfg.helloInterval = bits.str2num(cmd.word());
                pimCfg.restartTimer(false);
                return false;
            }
            return true;
        }
        if (a.equals("inspect")) {
            if (inspect != null) {
                inspect.stopTimer();
            }
            inspect = new tabSession(true, 180000);
            inspect.fromString(cmd);
            inspect.startTimer();
            inspect.setNotifier(fwd.triggerUpdate);
            return false;
        }
        if (a.equals("bfd")) {
            if (bfdCfg == null) {
                bfdCfg = new rtrBfdIface(udp, this);
                bfdCfg.register2udp();
            }
            bfdCfg.intervalTx = bits.str2num(cmd.word());
            bfdCfg.intervalRx = bits.str2num(cmd.word());
            bfdCfg.multiplier = bits.str2num(cmd.word());
            int i = bits.str2num(cmd.word());
            if (cmd.size() < 1) {
                bfdCfg.keyId = 0;
                bfdCfg.password = null;
            } else {
                bfdCfg.keyId = i;
                bfdCfg.password = authLocal.passwdDecode(cmd.word());
            }
            return false;
        }
        if (a.equals("srh")) {
            srhCfg = new rtrSrhIface(fwd, this);
            srhCfg.register2ip();
            return false;
        }
        if (a.equals("nsh")) {
            nshCfg = new rtrNshIface(fwd, this);
            nshCfg.register2ip();
            return false;
        }
        if (a.equals("ptp")) {
            a = cmd.word();
            if (a.equals("enable")) {
                if (ptpCfg != null) {
                    return false;
                }
                ptpCfg = new rtrPtpIface(udp, this);
                ptpCfg.register2udp();
                return false;
            }
            if (ptpCfg == null) {
                cmd.error("protocol not running");
                return true;
            }
            if (a.equals("receive")) {
                ptpCfg.receive = true;
                return false;
            }
            return true;
        }
        if (a.equals("hsrp")) {
            a = cmd.word();
            if (a.equals("address")) {
                boolean nw = hsrpCfg == null;
                if (nw) {
                    hsrpCfg = new rtrHsrpIface(udp, this);
                    hsrpCfg.mac = hsrpCfg.genPackHolder().genMacAddr();
                } else {
                    hsrpCfg.resetState();
                }
                hsrpCfg.ip.fromString(cmd.word());
                if (nw) {
                    hsrpCfg.register2udp();
                }
                return false;
            }
            if (hsrpCfg == null) {
                cmd.error("protocol not running");
                return false;
            }
            if (a.equals("bfd")) {
                hsrpCfg.bfdTrigger = true;
                return false;
            }
            if (a.equals("group")) {
                hsrpCfg.group = bits.str2num(cmd.word());
                hsrpCfg.mac = hsrpCfg.genPackHolder().genMacAddr();
                hsrpCfg.resetState();
                return false;
            }
            if (a.equals("password")) {
                hsrpCfg.authen = authLocal.passwdDecode(cmd.word());
                return false;
            }
            if (a.equals("mac-address")) {
                hsrpCfg.mac.fromString(cmd.word());
                return false;
            }
            if (a.equals("version")) {
                int nv = bits.str2num(cmd.word());
                if ((nv < 1) || (nv > 2)) {
                    nv = 2;
                }
                if (hsrpCfg.version == nv) {
                    return false;
                }
                hsrpCfg.unregister2udp();
                hsrpCfg.version = nv;
                hsrpCfg.register2udp();
                return false;
            }
            if (a.equals("timer")) {
                hsrpCfg.hello = bits.str2num(cmd.word());
                hsrpCfg.hold = bits.str2num(cmd.word());
                return false;
            }
            if (a.equals("priority")) {
                hsrpCfg.priority = bits.str2num(cmd.word());
                return false;
            }
            if (a.equals("preempt")) {
                hsrpCfg.preempt = true;
                return false;
            }
            if (a.equals("tracker")) {
                hsrpCfg.trackR = cmd.word();
                hsrpCfg.trackD = bits.str2num(cmd.word());
                return false;
            }
            return true;
        }
        if (a.equals("vrrp")) {
            a = cmd.word();
            if (a.equals("address")) {
                boolean nw = vrrpCfg == null;
                if (nw) {
                    vrrpCfg = new rtrVrrpIface(fwd, this);
                    vrrpCfg.mac = vrrpCfg.genPackHolder().genMacAddr();
                } else {
                    vrrpCfg.resetState();
                }
                vrrpCfg.ip.fromString(cmd.word());
                if (nw) {
                    vrrpCfg.register2ip();
                    vrrpCfg.restartTimer(false);
                }
                return false;
            }
            if (vrrpCfg == null) {
                cmd.error("protocol not running");
                return false;
            }
            if (a.equals("bfd")) {
                vrrpCfg.bfdTrigger = true;
                return false;
            }
            if (a.equals("group")) {
                vrrpCfg.group = bits.str2num(cmd.word());
                vrrpCfg.mac = vrrpCfg.genPackHolder().genMacAddr();
                vrrpCfg.resetState();
                return false;
            }
            if (a.equals("mac-address")) {
                vrrpCfg.mac.fromString(cmd.word());
                return false;
            }
            if (a.equals("version")) {
                int nv = bits.str2num(cmd.word());
                if ((nv < 1) || (nv > 3)) {
                    nv = 3;
                }
                vrrpCfg.version = nv;
                return false;
            }
            if (a.equals("timer")) {
                vrrpCfg.hello = bits.str2num(cmd.word());
                vrrpCfg.hold = bits.str2num(cmd.word());
                vrrpCfg.restartTimer(false);
                return false;
            }
            if (a.equals("priority")) {
                vrrpCfg.priority = bits.str2num(cmd.word());
                return false;
            }
            if (a.equals("tracker")) {
                vrrpCfg.trackR = cmd.word();
                vrrpCfg.trackD = bits.str2num(cmd.word());
                return false;
            }
            return true;
        }
        return true;
    }

    /**
     * undo configuration
     *
     * @param a command
     * @param cmd commands
     * @param fwd forwarder core
     * @return result code, true on error, false on success
     */
    public boolean unConfig(String a, cmds cmd, ipFwd fwd) {
        if (a.equals("enable")) {
            linkLocal = false;
            fwd.routerStaticChg();
            return false;
        }
        if (a.equals("secondary-address")) {
            addrIP adr = new addrIP();
            if (adr.fromString(cmd.word())) {
                cmd.error("bad ip address");
                return false;
            }
            adrDel(adr);
            return false;
        }
        if (a.equals("secondary-network")) {
            ipFwdIfacePref ntry = new ipFwdIfacePref();
            if (ntry.fromString(cmd)) {
                return false;
            }
            adrDel(ntry.adr);
            nets.del(ntry);
            fwd.routerStaticChg();
            return false;
        }
        if (a.equals("fragmentation")) {
            fragments = 0;
            return false;
        }
        if (a.equals("pmtud-in")) {
            pmtudIn = 0;
            return false;
        }
        if (a.equals("pmtud-out")) {
            pmtudOut = 0;
            return false;
        }
        if (a.equals("reassembly")) {
            reasmBuf = null;
            return false;
        }
        if (a.equals("netflow-rx")) {
            netflowRx = false;
            return false;
        }
        if (a.equals("netflow-tx")) {
            netflowTx = false;
            return false;
        }
        if (a.equals("propagate-ttl-always")) {
            mplsPropTtlAlways = false;
            return false;
        }
        if (a.equals("propagate-ttl-allow")) {
            mplsPropTtlAllow = false;
            return false;
        }
        if (a.equals("unreachables")) {
            unreachEna = false;
            return false;
        }
        if (a.equals("proxy-remote")) {
            answerDefReqs = false;
            return false;
        }
        if (a.equals("proxy-local")) {
            answerNetReqs = false;
            return false;
        }
        if (a.equals("proxy-filter")) {
            answerFilter = null;
            return false;
        }
        if (a.equals("host-remote")) {
            hostRemote = null;
            fwd.routerStaticChg();
            return false;
        }
        if (a.equals("host-learn")) {
            lower.setCacheDynmc(false);
            return false;
        }
        if (a.equals("dlep")) {
            if (dlepCfg != null) {
                dlepCfg.stopWork();
            }
            dlepCfg = null;
            return false;
        }
        if (a.equals("host-watch")) {
            if (hostWatch != null) {
                hostWatch.stopWork();
            }
            hostWatch = null;
            return false;
        }
        if (a.equals("host-static")) {
            addrIP adr = new addrIP();
            if (adr.fromString(cmd.word())) {
                cmd.error("bad ip address");
                return false;
            }
            addrMac mac = new addrMac();
            if (mac.fromString(cmd.word())) {
                cmd.error("bad mac address");
                return false;
            }
            lower.updateL2info(2, mac, adr);
            return false;
        }
        if (a.equals("autoroute")) {
            autRouTyp = null;
            autRouPrt = 0;
            autRouRtr = null;
            autRouHop = null;
            autRouRec = false;
            autRouMcst = false;
            autRouUnic = false;
            autRouExcld = false;
            fwd.routerStaticChg();
            return false;
        }
        if (a.equals("host-reach")) {
            lower.setCacheTimer(ipIfcLoop.defaultCacheTime);
            return false;
        }
        if (a.equals("host-rate")) {
            lower.setCacheRate(null);
            return false;
        }
        if (a.equals("host-retry")) {
            lower.setCacheRetry(ipIfcLoop.defaultRetryTime);
            return false;
        }
        if (a.equals("unreach-source")) {
            unreachSrc = null;
            return false;
        }
        if (a.equals("resend-packet")) {
            blockHost2host = true;
            return false;
        }
        if (a.equals("flowspec-disable")) {
            disableFlowspec = false;
            return false;
        }
        if (a.equals("dapp-disable")) {
            disableDapp = false;
            return false;
        }
        if (a.equals("verify-source")) {
            verifySource = false;
            verifyStricht = false;
            return false;
        }
        if (a.equals("gateway-connected")) {
            gateCon = false;
            fwd.routerStaticChg();
            return false;
        }
        if (a.equals("gateway-local")) {
            gateLoc = false;
            fwd.routerStaticChg();
            return false;
        }
        if (a.equals("gateway-remote")) {
            gateRem = false;
            fwd.routerStaticChg();
            return false;
        }
        if (a.equals("gateway-process")) {
            gatePrc = false;
            return false;
        }
        if (a.equals("gateway-distance")) {
            gateDstC = 0;
            gateDstL = 0;
            gateDstR = 0;
            gateDstP = 0;
            fwd.routerStaticChg();
            return false;
        }
        if (a.equals("gateway-labeled")) {
            gateLab = 0;
            fwd.routerStaticChg();
            return false;
        }
        if (a.equals("gateway-prefix")) {
            gatePrfx = null;
            fwd.routerStaticChg();
            return false;
        }
        if (a.equals("gateway-map")) {
            gateRtmp = null;
            fwd.routerStaticChg();
            return false;
        }
        if (a.equals("gateway-policy")) {
            gateRplc = null;
            fwd.routerStaticChg();
            return false;
        }
        if (a.equals("access-group-in")) {
            filterIn = null;
            return false;
        }
        if (a.equals("access-group-out")) {
            filterOut = null;
            return false;
        }
        if (a.equals("access-group-common-in")) {
            cfilterIn = null;
            return false;
        }
        if (a.equals("access-group-common-out")) {
            cfilterOut = null;
            return false;
        }
        if (a.equals("tcp-mss-in")) {
            tcpMssIn = 0;
            return false;
        }
        if (a.equals("tcp-mss-out")) {
            tcpMssOut = 0;
            return false;
        }
        if (a.equals("pbr")) {
            tabPbrN ntry = new tabPbrN();
            ntry.sequence = pbrCfg.nextseq();
            if (ntry.fromString(fwd.ipVersion, cmd.getRemaining())) {
                return true;
            }
            pbrCfg.del(ntry);
            return false;
        }
        if (a.equals("multicast")) {
            a = cmd.word();
            if (a.equals("broadcast")) {
                mcastAsBcast = false;
                return false;
            }
            if (a.equals("unicast")) {
                mcastAsUcast = false;
                return false;
            }
            if (a.equals("source-override-in")) {
                mcastSrcIn = null;
                return false;
            }
            if (a.equals("source-override-out")) {
                mcastSrcOut = null;
                return false;
            }
            if (a.equals("ttl-threshold")) {
                mcastTtl = 0;
                return false;
            }
            if (a.equals("static-group")) {
                addrIP a1 = new addrIP();
                addrIP a2 = new addrIP();
                if (a1.fromString(cmd.word())) {
                    cmd.error("bad group address");
                    return false;
                }
                if (a2.fromString(cmd.word())) {
                    cmd.error("bad source address");
                    return false;
                }
                fwd.mcastDelFloodIfc(a1, a2, this);
                return false;
            }
            if (a.equals("mldp-enable")) {
                mldpCfg = null;
                return false;
            }
            if (a.equals("host-enable")) {
                if (mhostCfg != null) {
                    mhostCfg.restartTimer(true);
                }
                mhostCfg = null;
                return false;
            }
            if (mhostCfg == null) {
                cmd.error("protocol not running");
                return false;
            }
            if (a.equals("host-proxy")) {
                mhostCfg.sendJoins = false;
                return false;
            }
            return true;
        }
        if (a.equals("pim")) {
            a = cmd.word();
            if (a.equals("enable")) {
                if (pimCfg != null) {
                    pimCfg.unregister2ip();
                }
                pimCfg = null;
                return false;
            }
            if (pimCfg == null) {
                cmd.error("protocol not running");
                return false;
            }
            if (a.equals("bfd")) {
                pimCfg.bfdTrigger = false;
                return false;
            }
            if (a.equals("bier-tunnel")) {
                pimCfg.bierTunnel = 0;
                return false;
            }
            if (a.equals("allow-rx")) {
                pimCfg.allowRx = false;
                return false;
            }
            if (a.equals("allow-tx")) {
                pimCfg.allowTx = false;
                return false;
            }
            if (a.equals("join-source")) {
                pimCfg.joinSource = null;
                return false;
            }
            return true;
        }
        if (a.equals("inspect")) {
            if (inspect != null) {
                inspect.stopTimer();
            }
            inspect = null;
            return false;
        }
        if (a.equals("bfd")) {
            if (bfdCfg == null) {
                return false;
            }
            bfdCfg.unregister2udp();
            bfdCfg = null;
            return false;
        }
        if (a.equals("srh")) {
            if (srhCfg == null) {
                return false;
            }
            srhCfg.unregister2ip();
            srhCfg = null;
            return false;
        }
        if (a.equals("nsh")) {
            if (nshCfg == null) {
                return false;
            }
            nshCfg = new rtrNshIface(fwd, this);
            nshCfg.unregister2ip();
            nshCfg = null;
            return false;
        }
        if (a.equals("ptp")) {
            a = cmd.word();
            if (a.equals("enable")) {
                if (ptpCfg == null) {
                    return false;
                }
                ptpCfg.unregister2udp();
                ptpCfg = null;
                return false;
            }
            if (ptpCfg == null) {
                cmd.error("protocol not running");
                return true;
            }
            if (a.equals("receive")) {
                ptpCfg.receive = false;
                return false;
            }
            return true;
        }
        if (a.equals("hsrp")) {
            a = cmd.word();
            if (a.equals("address")) {
                if (hsrpCfg == null) {
                    return false;
                }
                hsrpCfg.unregister2udp();
                hsrpCfg = null;
                return false;
            }
            if (hsrpCfg == null) {
                cmd.error("protocol not running");
                return false;
            }
            if (a.equals("bfd")) {
                hsrpCfg.bfdTrigger = false;
                return false;
            }
            if (a.equals("group")) {
                hsrpCfg.group = 0;
                hsrpCfg.mac = hsrpCfg.genPackHolder().genMacAddr();
                hsrpCfg.resetState();
                return false;
            }
            if (a.equals("mac-address")) {
                hsrpCfg.mac = hsrpCfg.genPackHolder().genMacAddr();
                return false;
            }
            if (a.equals("preempt")) {
                hsrpCfg.preempt = false;
                return false;
            }
            if (a.equals("tracker")) {
                hsrpCfg.trackR = null;
                hsrpCfg.trackD = 0;
                return false;
            }
            return true;
        }
        if (a.equals("vrrp")) {
            a = cmd.word();
            if (a.equals("address")) {
                if (vrrpCfg == null) {
                    return false;
                }
                vrrpCfg.restartTimer(true);
                vrrpCfg.unregister2ip();
                vrrpCfg = null;
                return false;
            }
            if (vrrpCfg == null) {
                cmd.error("protocol not running");
                return false;
            }
            if (a.equals("bfd")) {
                vrrpCfg.bfdTrigger = false;
                return false;
            }
            if (a.equals("group")) {
                vrrpCfg.group = 0;
                vrrpCfg.mac = vrrpCfg.genPackHolder().genMacAddr();
                vrrpCfg.resetState();
                return false;
            }
            if (a.equals("mac-address")) {
                vrrpCfg.mac = vrrpCfg.genPackHolder().genMacAddr();
                return false;
            }
            if (a.equals("tracker")) {
                vrrpCfg.trackR = null;
                vrrpCfg.trackD = 0;
                return false;
            }
            return true;
        }
        return true;
    }

    /**
     * wait for bfd peer
     *
     * @param adr address of peer
     * @param tim maximum time to wait
     * @return false on success, true on error
     */
    public boolean bfdWait(addrIP adr, int tim) {
        if (bfdCfg == null) {
            return true;
        }
        return bfdCfg.clientWait(adr, tim);
    }

    /**
     * wait for bfd peer
     *
     * @param adr address of peer
     * @param tim maximum time to wait
     * @return false on success, true on error
     */
    public boolean bfdWait(addrIPv4 adr, int tim) {
        addrIP a = new addrIP();
        a.fromIPv4addr(adr);
        return bfdWait(a, tim);
    }

    /**
     * wait for bfd peer
     *
     * @param adr address of peer
     * @param tim maximum time to wait
     * @return false on success, true on error
     */
    public boolean bfdWait(addrIPv6 adr, int tim) {
        addrIP a = new addrIP();
        a.fromIPv6addr(adr);
        return bfdWait(a, tim);
    }

    /**
     * add one bfd peer
     *
     * @param adr address of peer
     * @param clnt client to notify
     * @param nam name of client
     * @return false on success, true on error
     */
    public boolean bfdAdd(addrIP adr, rtrBfdClnt clnt, String nam) {
        if (bfdCfg == null) {
            return true;
        }
        return bfdCfg.clientAdd(adr, clnt, nam);
    }

    /**
     * add one bfd peer
     *
     * @param adr address of peer
     * @param clnt client to notify
     * @param nam name of client
     * @return false on success, true on error
     */
    public boolean bfdAdd(addrIPv4 adr, rtrBfdClnt clnt, String nam) {
        addrIP a = new addrIP();
        a.fromIPv4addr(adr);
        return bfdAdd(a, clnt, nam);
    }

    /**
     * add one bfd peer
     *
     * @param adr address of peer
     * @param clnt client to notify
     * @param nam name of client
     * @return false on success, true on error
     */
    public boolean bfdAdd(addrIPv6 adr, rtrBfdClnt clnt, String nam) {
        addrIP a = new addrIP();
        a.fromIPv6addr(adr);
        return bfdAdd(a, clnt, nam);
    }

    /**
     * delete one bfd peer
     *
     * @param adr address of peer
     * @param clnt client to notify
     * @return false on success, true on error
     */
    public boolean bfdDel(addrIP adr, rtrBfdClnt clnt) {
        if (bfdCfg == null) {
            return true;
        }
        if (adr == null) {
            return true;
        }
        return bfdCfg.clientDel(adr, clnt);
    }

    /**
     * delete one bfd peer
     *
     * @param adr address of peer
     * @param clnt client to notify
     * @return false on success, true on error
     */
    public boolean bfdDel(addrIPv4 adr, rtrBfdClnt clnt) {
        if (adr == null) {
            return true;
        }
        addrIP a = new addrIP();
        a.fromIPv4addr(adr);
        return bfdDel(a, clnt);
    }

    /**
     * delete one bfd peer
     *
     * @param adr address of peer
     * @param clnt client to notify
     * @return false on success, true on error
     */
    public boolean bfdDel(addrIPv6 adr, rtrBfdClnt clnt) {
        if (adr == null) {
            return true;
        }
        addrIP a = new addrIP();
        a.fromIPv6addr(adr);
        return bfdDel(a, clnt);
    }

    /**
     * find bfd neighbor
     *
     * @param adr address
     * @return bfd neighbor, null if not found
     */
    public rtrBfdNeigh bfdFind(addrIP adr) {
        if (bfdCfg == null) {
            return null;
        }
        return bfdCfg.clientFind(adr);
    }

    /**
     * find bfd neighbor
     *
     * @param adr address
     * @return bfd neighbor, null if not found
     */
    public rtrBfdNeigh bfdFind(addrIPv4 adr) {
        addrIP a = new addrIP();
        a.fromIPv4addr(adr);
        return bfdFind(a);
    }

    /**
     * find bfd neighbor
     *
     * @param adr address
     * @return bfd neighbor, null if not found
     */
    public rtrBfdNeigh bfdFind(addrIPv6 adr) {
        addrIP a = new addrIP();
        a.fromIPv6addr(adr);
        return bfdFind(a);
    }

    /**
     * get one network
     *
     * @param i id
     * @return prefix, null if not found
     */
    public addrPrefix<addrIP> netGet(int i) {
        if (i < 0) {
            return network;
        }
        ipFwdIfacePref ntry = nets.get(i);
        if (ntry == null) {
            return null;
        }
        return ntry.pfx;
    }

    /**
     * get one mapping
     *
     * @param i id
     * @return ip, null if not found
     */
    public addrIP adrGetIp(int i) {
        ipFwdIfaceAddr ntry = adrs.get(i);
        if (ntry == null) {
            return null;
        }
        return ntry.ip.copyBytes();
    }

    /**
     * get one mapping
     *
     * @param i id
     * @return mac, null if not found
     */
    public addrMac adrGetMac(int i) {
        ipFwdIfaceAddr ntry = adrs.get(i);
        if (ntry == null) {
            return null;
        }
        return ntry.mac.copyBytes();
    }

    /**
     * inform that just acquired one address
     *
     * @param ip l3 address
     * @param mac l2 address
     * @param cfg user configured value
     */
    public void adrAdd(addrIP ip, addrMac mac, boolean cfg) {
        ipFwdIfaceAddr ntry = new ipFwdIfaceAddr();
        ntry.ip = ip;
        ntry.mac = mac;
        ntry.cfg = cfg;
        adrs.put(ntry);
        lower.setFilter(adrs.size() > 0);
        lower.sendL2info(mac, ip);
    }

    /**
     * inform that just lost one address
     *
     * @param ip l3 address
     */
    public void adrDel(addrIP ip) {
        ipFwdIfaceAddr ntry = new ipFwdIfaceAddr();
        ntry.ip = ip;
        adrs.del(ntry);
        lower.setFilter(adrs.size() > 0);
    }

    /**
     * check if my address
     *
     * @param ip l3 address
     * @return mac if mine, null if not
     */
    public addrMac adrChk(addrIP ip) {
        if (ip.compareTo(addr) == 0) {
            return null;
        }
        ipFwdIfaceAddr ntry = new ipFwdIfaceAddr();
        ntry.ip = ip;
        ntry = adrs.find(ntry);
        if (ntry == null) {
            return null;
        }
        return ntry.mac;
    }

    /**
     * get ldp password
     *
     * @param ip address
     * @return password, null if not found
     */
    public String ldpasFind(addrIP ip) {
        ipFwdIfaceLdpas ntry = new ipFwdIfaceLdpas();
        ntry.ip = ip;
        ntry = ldpas.find(ntry);
        if (ntry == null) {
            return null;
        }
        return ntry.pwd;
    }

    /**
     * get config
     *
     * @param l list to append
     * @param beg beginning
     * @param filter filter defaults
     */
    public void ldpasCfg(List<String> l, String beg, int filter) {
        for (int i = 0; i < ldpas.size(); i++) {
            ipFwdIfaceLdpas ntry = ldpas.get(i);
            l.add(beg + " " + ntry.ip + " " + authLocal.passwdEncode(ntry.pwd, (filter & 2) != 0));
        }
    }

    /**
     * put ldp password
     *
     * @param ip address
     * @param pwd password
     */
    public void ldpasPut(addrIP ip, String pwd) {
        ipFwdIfaceLdpas ntry = new ipFwdIfaceLdpas();
        ntry.ip = ip;
        ntry.pwd = authLocal.passwdDecode(pwd);
        ldpas.put(ntry);
    }

    /**
     * delete ldp password
     *
     * @param ip address
     */
    public void ldpasDel(addrIP ip) {
        ipFwdIfaceLdpas ntry = new ipFwdIfaceLdpas();
        ntry.ip = ip;
        ldpas.del(ntry);
    }

    /**
     * get static label
     *
     * @param lower forwarder
     * @param pfx prefix
     * @param hop next hop
     * @return fake route entry, null if not found
     */
    public tabRouteEntry<addrIP> labelsFind(ipFwd lower, addrPrefix<addrIP> pfx, addrIP hop) {
        ipFwdIfaceBind bnd = new ipFwdIfaceBind();
        bnd.pfx = pfx;
        bnd.hop = hop;
        bnd = mplStat.find(bnd);
        if (bnd != null) {
            tabRouteEntry<addrIP> res = new tabRouteEntry<addrIP>();
            res.best.iface = this;
            res.best.nextHop = hop;
            if (bnd.lab >= 0) {
                res.best.labelRem = tabLabel.int2labels(bnd.lab);
            }
            return res;
        }
        if (mplPeer == null) {
            return null;
        }
        rtrLdpNeigh nei = lower.ldpNeighFind(mplPeer, false);
        if (nei == null) {
            return null;
        }
        tabRouteEntry<addrIP> res = nei.prefLearn.find(pfx);
        if (res == null) {
            return null;
        }
        List<Integer> lab = res.best.labelRem;
        if (lab == null) {
            return null;
        }
        res = new tabRouteEntry<addrIP>();
        res.best.iface = this;
        res.best.nextHop = hop;
        res.best.labelRem = lab;
        return res;
    }

    /**
     * get config
     *
     * @param l list to append
     * @param beg1 beginning
     * @param beg2 beginning
     */
    public void labelsCfg(List<String> l, String beg1, String beg2) {
        for (int i = 0; i < mplStat.size(); i++) {
            ipFwdIfaceBind ntry = mplStat.get(i);
            l.add(beg1 + " " + addrPrefix.ip2str(ntry.pfx) + " " + ntry.hop + " " + ntry.lab);
        }
        if (mplPeer != null) {
            l.add(beg2 + " " + mplPeer + " " + mplHop);
        }
    }

    /**
     * get static label
     *
     * @param pfx prefix
     * @param hop next hop
     * @param lab label
     */
    public void labelsPut(addrPrefix<addrIP> pfx, addrIP hop, int lab) {
        ipFwdIfaceBind bnd = new ipFwdIfaceBind();
        bnd.pfx = pfx;
        bnd.hop = hop;
        bnd.lab = lab;
        mplStat.put(bnd);
    }

    /**
     * remove static label
     *
     * @param pfx prefix
     * @param hop next hop
     */
    public void labelsDel(addrPrefix<addrIP> pfx, addrIP hop) {
        ipFwdIfaceBind bnd = new ipFwdIfaceBind();
        bnd.pfx = pfx;
        bnd.hop = hop;
        mplStat.del(bnd);
    }

}

class ipFwdIfaceAddr implements Comparable<ipFwdIfaceAddr> {

    public addrIP ip;

    public addrMac mac;

    public boolean cfg;

    public int compareTo(ipFwdIfaceAddr o) {
        return ip.compareTo(o.ip);
    }

}

class ipFwdIfacePref implements Comparable<ipFwdIfacePref> {

    public addrIP adr;

    public addrIP msk;

    public addrPrefix<addrIP> pfx;

    public boolean fromString(cmds cmd) {
        adr = new addrIP();
        if (adr.fromString(cmd.word())) {
            cmd.error("bad ip address");
            return true;
        }
        msk = new addrIP();
        if (msk.fromString(cmd.word())) {
            cmd.error("bad mask address");
            return true;
        }
        addrIP a = new addrIP();
        addrIP m = new addrIP();
        if (adr.isIPv4()) {
            m.fromIPv4mask(msk.toIPv4());
            a.fromIPv4addr(adr.toIPv4());
        } else {
            m.fromIPv6addr(msk.toIPv6());
            a.fromIPv6addr(adr.toIPv6());
        }
        pfx = new addrPrefix<addrIP>(a, m.toNetmask());
        return false;
    }

    public int compareTo(ipFwdIfacePref o) {
        return pfx.compareTo(o.pfx);
    }

}

class ipFwdIfaceLdpas implements Comparable<ipFwdIfaceLdpas> {

    public addrIP ip;

    public String pwd;

    public int compareTo(ipFwdIfaceLdpas o) {
        return ip.compareTo(o.ip);
    }

}

class ipFwdIfaceBind implements Comparable<ipFwdIfaceBind> {

    public addrPrefix<addrIP> pfx;

    public addrIP hop;

    public int lab;

    public int compareTo(ipFwdIfaceBind o) {
        int i = hop.compareTo(o.hop);
        if (i != 0) {
            return i;
        }
        return pfx.compareTo(o.pfx);
    }

}
