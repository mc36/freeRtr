package net.freertr.ip;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import net.freertr.addr.addrIP;
import net.freertr.addr.addrIPv4;
import net.freertr.addr.addrIPv6;
import net.freertr.addr.addrMac;
import net.freertr.addr.addrPrefix;
import net.freertr.auth.authLocal;
import net.freertr.cfg.cfgAceslst;
import net.freertr.cfg.cfgAll;
import net.freertr.cfg.cfgIfc;
import net.freertr.cfg.cfgPrfxlst;
import net.freertr.cfg.cfgRoump;
import net.freertr.cfg.cfgRouplc;
import net.freertr.cfg.cfgRtr;
import net.freertr.pack.packHolder;
import net.freertr.prt.prtUdp;
import net.freertr.rtr.rtrBfdClnt;
import net.freertr.rtr.rtrBfdIface;
import net.freertr.rtr.rtrBfdNeigh;
import net.freertr.rtr.rtrHsrpIface;
import net.freertr.rtr.rtrPimIface;
import net.freertr.rtr.rtrPtpIface;
import net.freertr.rtr.rtrSrhIface;
import net.freertr.rtr.rtrVrrpIface;
import net.freertr.tab.tabAceslstN;
import net.freertr.tab.tabGen;
import net.freertr.tab.tabListing;
import net.freertr.tab.tabPbrN;
import net.freertr.tab.tabPrfxlstN;
import net.freertr.tab.tabRouteAttr;
import net.freertr.tab.tabRouteIface;
import net.freertr.tab.tabRtrmapN;
import net.freertr.tab.tabRtrplcN;
import net.freertr.tab.tabSession;
import net.freertr.user.userHelping;
import net.freertr.util.bits;
import net.freertr.util.cmds;
import net.freertr.util.counter;

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
     * set true to block directed broadcasts
     */
    public boolean blockBroadcast = true;

    /**
     * set true to send multicast as broadcast
     */
    public boolean mcastAsBcast = false;

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
     * install local route
     */
    public boolean gateLoc = true;

    /**
     * install remote route
     */
    public boolean gateRem = true;

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
     * fragmentation payload size
     */
    public int fragments;

    /**
     * pmtud payload size
     */
    public int pmtuds;

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
     * point to point link
     */
    public boolean point2point;

    /**
     * srh interface handler
     */
    public rtrSrhIface srhCfg;

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

    private final tabGen<ipFwdIfaceAddr> adrs = new tabGen<ipFwdIfaceAddr>();

    private final tabGen<ipFwdIfaceLdpas> ldpas = new tabGen<ipFwdIfaceLdpas>();

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
    public List<String> getShow() {
        List<String> l = new ArrayList<String>();
        l.add(lower + " ready=" + ready);
        l.add(cmds.tabulator + "num=" + ifwNum + " mtu=" + mtu + " bw=" + bits.bandwidth(bandwidth) + " p2p=" + point2point);
        l.add(cmds.tabulator + "addr=" + addr + " mask=" + mask);
        l.add(cmds.tabulator + "net=" + network);
        l.add(cmds.tabulator + "ll=" + lower.getLinkLocalAddr());
        for (int i = 0; i < adrs.size(); i++) {
            ipFwdIfaceAddr adr = adrs.get(i);
            if (adr == null) {
                continue;
            }
            l.add(cmds.tabulator + "additional=" + adr.ip + " " + adr.mac + " " + adr.cfg);
        }
        l.add(cmds.tabulator + "cntr=" + cntr.getShStat());
        l.add(cmds.tabulator + "ipcntr=" + lower.getCounter().getShStat());
        return l;
    }

    /**
     * get help text
     *
     * @param l storage
     */
    public static void getHelp(userHelping l) {
        l.add(null, "2 3     address                     set the ip address of an interface");
        l.add(null, "3 4       dynamic                   dynamic address");
        l.add(null, "3 4       <addr>                    address of interface");
        l.add(null, "4 .         dynamic                 dynamic netmask");
        l.add(null, "4 .         <mask>                  subnet mask of address");
        l.add(null, "2 3     secondary-address           set up an additional ip address");
        l.add(null, "3 .       <addr>                    address of interface");
        l.add(null, "2 3     reassembly                  set up a reassembly buffer");
        l.add(null, "3 .       <num>                     number of packets");
        l.add(null, "2 3     fragmentation               set up fragmentation");
        l.add(null, "3 .       <num>                     maximum payload size");
        l.add(null, "2 3     pmtud-reply                 set up pmtud responder");
        l.add(null, "3 .       <num>                     maximum payload size");
        l.add(null, "2 .     netflow-rx                  netflow received packets");
        l.add(null, "2 .     netflow-tx                  netflow transmitted packets");
        l.add(null, "2 .     propagate-ttl-always        enable ttl propagation to mpls");
        l.add(null, "2 .     propagate-ttl-allow         allow ttl propagation to mpls");
        l.add(null, "2 .     unreachables                enable sending icmp unreachable messages");
        l.add(null, "2 3     unreach-source              set unreachable source");
        l.add(null, "3 .       <name:ifc>                name of interface");
        l.add(null, "2 3     redirection                 send packets out on different interface");
        l.add(null, "3 .       <name:ifc>                name of interface");
        l.add(null, "2 .     resend-packet               enable sending packet out on same interface");
        l.add(null, "2 .     directed-broadcast          enable forwarding of directed broadcasts");
        l.add(null, "2 .     broadcast-multicast         broadcast the multicast packets");
        l.add(null, "2 .     flowspec-disable            disable flowspec processing");
        l.add(null, "2 .     dapp-disable                disable dapp processing");
        l.add(null, "2 3     verify-source               enable per packet validation");
        l.add(null, "3 .       any                       source is reachable via any interface");
        l.add(null, "3 .       rx                        source is reachable via this interface");
        l.add(null, "3 .       none                      disable per packet source checking");
        l.add(null, "2 .     gateway-local               install local route");
        l.add(null, "2 .     gateway-remote              install remote route");
        l.add(null, "2 3     gateway-prefix              prefix list to install throught gateway");
        l.add(null, "3 .       <name:pl>                 name of prefix list");
        l.add(null, "2 3     gateway-routemap            route map to set throught gateway");
        l.add(null, "3 .       <name:rm>                 name of route map");
        l.add(null, "2 3     access-group-in             access list to apply to ingress packets");
        l.add(null, "3 .       <name:acl>                name of access list");
        l.add(null, "2 3     access-group-out            access list to apply to egress packets");
        l.add(null, "3 .       <name:acl>                name of access list");
        l.add(null, "2 3     access-group-common-in      common access list to apply to ingress packets");
        l.add(null, "3 .       <name:acl>                name of access list");
        l.add(null, "2 3     access-group-common-out     common access list to apply to egress packets");
        l.add(null, "3 .       <name:acl>                name of access list");
        l.add(null, "2 3,.   inspect                     enable packet inspection");
        l.add(null, "3 3,.     mac                       with mac addresses");
        l.add(null, "3 3,.     before                    log on session start");
        l.add(null, "3 3,.     after                     log on session stop");
        l.add(null, "3 3,.     dropped                   log dropped sessions");
        l.add(null, "3 3,.     drop-rx                   drop sessions ingress");
        l.add(null, "3 3,.     drop-tx                   drop sessions egress");
        l.add(null, "3 3,.     drop-frg                  drop fragmented packets");
        l.add(null, "3 3,.     allow-routing             allow control multicast traffic");
        l.add(null, "3 3,.     allow-linklocal           allow link local traffic");
        l.add(null, "3 3,.     allow-multicast           allow multicast traffic");
        l.add(null, "3 3,.     allow-broadcast           allow broadcast traffic");
        l.add(null, "3 4       allow-list                allow specific traffic");
        l.add(null, "4 3,.       <name:acl>              name of access list");
        l.add(null, "3 4       member                    member of inspection");
        l.add(null, "4 5         <name:ses>              name of session group");
        l.add(null, "5 3,.         <name>                local identifier in the group");
        l.add(null, "2 3     bfd                         enable bidirectional forwarding detection");
        l.add(null, "3 4       <num>                     tx interval in ms");
        l.add(null, "4 5         <num>                   rx interval in ms");
        l.add(null, "5 6,.         <num>                 multiplier");
        l.add(null, "6 7             [num]               key id");
        l.add(null, "7 .               <str>             password");
        l.add(null, "2 3     srh                         segment routing header commands");
        l.add(null, "3 .       enable                    enable/disable processing");
        l.add(null, "2 3     ptp                         precision time protococol commands");
        l.add(null, "3 .       enable                    enable/disable processing");
        l.add(null, "3 .       receive                   allow clock adjustment");
        l.add(null, "2 3     tcp-mss-in                  rewrite tcp mss in ingress packets");
        l.add(null, "3 .       <num>                     value");
        l.add(null, "2 3     tcp-mss-out                 rewrite tcp mss in egress packets");
        l.add(null, "3 .       <num>                     value");
        l.add(null, "2 3     host-reach                  set next hop cache timeout");
        l.add(null, "3 .       <num>                     time in ms");
        l.add(null, "2 3     host-retry                  set next hop cache retry");
        l.add(null, "3 .       <num>                     time in ms");
        l.add(null, "2 3     host-static                 set static next hop cache entry");
        l.add(null, "3 4       <addr>                    ip address");
        l.add(null, "4 .         <addr>                  mac address");
        l.add(null, "2 3,.   host-watch                  monitor next hop changes");
        l.add(null, "3 4       appear                    script on appearance");
        l.add(null, "4 3,.       <name:scr>              name of script");
        l.add(null, "3 4       change                    script on mac change");
        l.add(null, "4 3,.       <name:scr>              name of script");
        l.add(null, "3 4       disappear                 script on disappearance");
        l.add(null, "4 3,.       <name:scr>              name of script");
        l.add(null, "2 3     multicast                   multicast configuration options");
        l.add(null, "3 .       mldp-enable               enable mdlp processing");
        l.add(null, "3 .       host-enable               enable igmp/mld processing");
        l.add(null, "3 .       host-proxy                send joins for groups");
        l.add(null, "3 4       host-query                time between queries");
        l.add(null, "4 .         <num>                   time in ms");
        l.add(null, "3 4       ttl-threshold             ttl threshold for multicast packets");
        l.add(null, "4 .         <num>                   ttl");
        l.add(null, "3 4       source-override-in        override received source for groups");
        l.add(null, "4 .         <addr>                  source");
        l.add(null, "3 4       source-override-out       override sent source for groups");
        l.add(null, "4 .         <addr>                  source");
        l.add(null, "3 4       static-group              unconditional flooding");
        l.add(null, "4 5         <addr>                  group address");
        l.add(null, "5 .           <addr>                source address");
        l.add(null, "2 .     proxy-remote                reply to remote networks");
        l.add(null, "2 .     proxy-local                 reply to link address");
        l.add(null, "2 3     proxy-filter                access list to filter replies");
        l.add(null, "3 .       <name:acl>                name of access list");
        l.add(null, "2 3     autoroute                   point routes from routing protocol");
        cfgRtr.getRouterList(l, 1, "");
        l.add(null, "4  5        <num>                   process id");
        l.add(null, "5  6          <addr>                source router");
        l.add(null, "6  7,.          <addr>              nexthop");
        l.add(null, "7  7,.            recursive         process bgp routes");
        l.add(null, "7  7,.            multicast         process multicast table");
        l.add(null, "7  7,.            no-unicast        dont process unicast table");
        l.add(null, "7  7,.            exclude-match     exclude matching prefix");
        l.add(null, "7  8              include-list      include matching prefix");
        l.add(null, "8  7,.              <name:pl>       name of prefix list");
        l.add(null, "7  8              include-map       include matching prefix");
        l.add(null, "8  7,.              <name:rm>       name of route map");
        l.add(null, "7  8              include-policy    include matching prefix");
        l.add(null, "8  7,.              <name:rpl>      name of route map");
        l.add(null, "2 3     pim                         pim configuration options");
        l.add(null, "3 .       enable                    enable pim processing");
        l.add(null, "3 .       bfd                       enable bfd triggered down");
        l.add(null, "3 .       allow-rx                  suppress processing routing updates");
        l.add(null, "3 .       allow-tx                  suppress sending routing updates");
        l.add(null, "3 4       bier-tunnel               use bier encapsulation");
        l.add(null, "4 .         <num>                   this node index");
        l.add(null, "3 4       join-source               set join source");
        l.add(null, "4 .         <name:ifc>              name of interface");
        l.add(null, "3 4       packet-timer              inter packet gap time");
        l.add(null, "4 .         <num>                   time in ms");
        l.add(null, "3 4       priority                  router priority");
        l.add(null, "4 .         <num>                   priority 0=disable");
        l.add(null, "3 4       hello-time                time between hellos");
        l.add(null, "4 .         <num>                   time in ms");
        l.add(null, "2 3     hsrp                        enable hsrp processing");
        l.add(null, "3 4       address                   set virtual ip address");
        l.add(null, "4 .         <addr>                  address to use");
        l.add(null, "3 4       mac-address               set virtual mac address");
        l.add(null, "4 .         <addr>                  address to use");
        l.add(null, "3 .       bfd                       enable bfd triggered down");
        l.add(null, "3 4       group                     set group number");
        l.add(null, "4 .         <num>                   group");
        l.add(null, "3 4       password                  set group password");
        l.add(null, "4 .         <str>                   string");
        l.add(null, "3 4       version                   set protocol version");
        l.add(null, "4 .         <num>                   group");
        l.add(null, "3 4       timer                     set protocol timers");
        l.add(null, "4 5         <num>                   hello time in ms");
        l.add(null, "5 .           <num>                 hold time in ms");
        l.add(null, "3 .       preempt                   overbid lower priority ones");
        l.add(null, "3 4       priority                  set priority level");
        l.add(null, "4 .         <num>                   priority");
        l.add(null, "3 4       tracker                   set tracker to use");
        l.add(null, "4 5         <name:trk>              name of tracker");
        l.add(null, "5 .           <num>                 decrement value");
        l.add(null, "2 3     vrrp                        enable vrrp processing");
        l.add(null, "3 4       address                   set virtual ip address");
        l.add(null, "4 .         <addr>                  address to use");
        l.add(null, "3 4       mac-address               set virtual mac address");
        l.add(null, "4 .         <addr>                  address to use");
        l.add(null, "3 .       bfd                       enable bfd triggered down");
        l.add(null, "3 4       group                     set group number");
        l.add(null, "4 .         <num>                   group");
        l.add(null, "3 4       version                   set protocol version");
        l.add(null, "4 .         <num>                   group");
        l.add(null, "3 4       timer                     set protocol timers");
        l.add(null, "4 5         <num>                   hello time in ms");
        l.add(null, "5 .           <num>                 hold time in ms");
        l.add(null, "3 4       priority                  set priority level");
        l.add(null, "4 .         <num>                   priority");
        l.add(null, "3 4       tracker                   set tracker to use");
        l.add(null, "4 5         <name:trk>              name of tracker");
        l.add(null, "5 .           <num>                 decrement value");
        l.add(null, "2 3,5  pbr                          configure policy based routing");
        l.add(null, "3 4      sequence                   sequence number");
        l.add(null, "4 5        <num>                    number");
        l.add(null, "5 6          <name:acl>             access list name");
        l.add(null, "6 7,.          <name:vrf>           target vrf");
        l.add(null, "7 8              interface          set target interface");
        l.add(null, "8 7,.              <name:ifc>       interface name");
        l.add(null, "7 8              nexthop            set target address");
        l.add(null, "8 7,.              <addr>           target address");
        l.add(null, "7 8              nsh                set target service");
        l.add(null, "8 9                <num>            service path");
        l.add(null, "9 7,.                <num>          service index");
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
        cmds.cfgLine(l, blockBroadcast, cmds.tabulator, beg + "directed-broadcast", "");
        cmds.cfgLine(l, !mcastAsBcast, cmds.tabulator, beg + "broadcast-multicast", "");
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
        if (reasmBuf == null) {
            l.add(cmds.tabulator + "no " + beg + "reassembly");
        } else {
            l.add(cmds.tabulator + beg + "reassembly " + reasmBuf.size());
        }
        cmds.cfgLine(l, fragments < 1, cmds.tabulator, beg + "fragmentation", "" + fragments);
        cmds.cfgLine(l, pmtuds < 1, cmds.tabulator, beg + "pmtud-reply", "" + pmtuds);
        cmds.cfgLine(l, !gateLoc, cmds.tabulator, beg + "gateway-local", "");
        cmds.cfgLine(l, !gateRem, cmds.tabulator, beg + "gateway-remote", "");
        cmds.cfgLine(l, gatePrfx == null, cmds.tabulator, beg + "gateway-prefix", "" + gatePrfx);
        cmds.cfgLine(l, gateRtmp == null, cmds.tabulator, beg + "gateway-routemap", "" + gateRtmp);
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
        cmds.cfgLine(l, hostWatch == null, cmds.tabulator, beg + "host-watch", "" + hostWatch);
        l.add(cmds.tabulator + beg + "host-reach " + lower.getCacheTimer());
        l.add(cmds.tabulator + beg + "host-retry " + lower.getCacheRetry());
        lower.getL2info(l, cmds.tabulator + beg + "host-static ");
        for (int i = 0; i < pbrCfg.size(); i++) {
            tabPbrN pbr = pbrCfg.get(i);
            l.addAll(pbr.usrString(cmds.tabulator + beg + "pbr "));
        }
        if (srhCfg == null) {
            l.add(cmds.tabulator + "no " + beg + "srh enable");
        } else {
            l.add(cmds.tabulator + beg + "srh enable");
        }
        if (ptpCfg == null) {
            l.add(cmds.tabulator + "no " + beg + "ptp enable");
        } else {
            l.add(cmds.tabulator + beg + "ptp enable");
            cmds.cfgLine(l, !ptpCfg.receive, cmds.tabulator, beg + "ptp receive", "");
        }
        if (bfdCfg == null) {
            l.add(cmds.tabulator + "no " + beg + "bfd");
        } else {
            a = "";
            if (bfdCfg.password != null) {
                a += " " + bfdCfg.keyId + " " + authLocal.passwdEncode(bfdCfg.password, (filter & 2) != 0);
            }
            l.add(cmds.tabulator + beg + "bfd " + bfdCfg.intervalTx + " " + bfdCfg.intervalRx + " " + bfdCfg.multiplier + a);
        }
        cmds.cfgLine(l, tcpMssIn < 1, cmds.tabulator, beg + "tcp-mss-in", "" + tcpMssIn);
        cmds.cfgLine(l, tcpMssOut < 1, cmds.tabulator, beg + "tcp-mss-out", "" + tcpMssOut);
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
            l.add(cmds.tabulator + "no " + beg + "pim enable");
        }
        if (mhostCfg != null) {
            l.add(cmds.tabulator + beg + "multicast host-enable");
            cmds.cfgLine(l, !mhostCfg.sendJoins, cmds.tabulator, beg + "multicast host-proxy", "");
            l.add(cmds.tabulator + beg + "multicast host-query " + mhostCfg.queryInterval);
        } else {
            l.add(cmds.tabulator + "no " + beg + "multicast host-enable");
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
            l.add(cmds.tabulator + "no " + beg + "hsrp address");
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
            l.add(cmds.tabulator + "no " + beg + "vrrp address");
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
     * @return result code, true on error, false on success
     */
    public boolean doConfig(String a, cmds cmd, ipCor cor, ipFwd fwd, prtUdp udp) {
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
        if (a.equals("fragmentation")) {
            fragments = bits.str2num(cmd.word());
            return false;
        }
        if (a.equals("pmtud-reply")) {
            pmtuds = bits.str2num(cmd.word());
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
        if (a.equals("host-retry")) {
            lower.setCacheRetry(bits.str2num(cmd.word()));
            return false;
        }
        if (a.equals("unreach-source")) {
            cfgIfc ntry = cfgAll.ifcFind(cmd.word(), false);
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
        if (a.equals("broadcast-multicast")) {
            mcastAsBcast = true;
            return false;
        }
        if (a.equals("directed-broadcast")) {
            blockBroadcast = false;
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
        if (a.equals("gateway-routemap")) {
            cfgRoump ntry = cfgAll.rtmpFind(cmd.word(), false);
            if (ntry == null) {
                cmd.error("no such route map");
                return false;
            }
            gateRtmp = ntry.roumap;
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
            ntry.sequence = pbrCfg.nextseq();
            if (ntry.fromString(fwd.ipVersion, cmd.getRemaining())) {
                return true;
            }
            ntry.matcher.copyCores(fwd.pbrCfg);
            pbrCfg.add(ntry);
            return false;
        }
        if (a.equals("multicast")) {
            a = cmd.word();
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
                cfgIfc ntry = cfgAll.ifcFind(cmd.word(), false);
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
            inspect = new tabSession(true, 60000);
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
        if (a.equals("fragmentation")) {
            fragments = 0;
            return false;
        }
        if (a.equals("pmtud-reply")) {
            pmtuds = 0;
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
        if (a.equals("broadcast-multicast")) {
            mcastAsBcast = false;
            return false;
        }
        if (a.equals("directed-broadcast")) {
            blockBroadcast = true;
            return false;
        }
        if (a.equals("verify-source")) {
            verifySource = false;
            verifyStricht = false;
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
        if (a.equals("gateway-prefix")) {
            gatePrfx = null;
            fwd.routerStaticChg();
            return false;
        }
        if (a.equals("gateway-routemap")) {
            gateRtmp = null;
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
        if (ip.compare(ip, addr) == 0) {
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

}

class ipFwdIfaceAddr implements Comparator<ipFwdIfaceAddr> {

    public addrIP ip;

    public addrMac mac;

    public boolean cfg;

    public int compare(ipFwdIfaceAddr o1, ipFwdIfaceAddr o2) {
        return o1.ip.compare(o1.ip, o2.ip);
    }

}

class ipFwdIfaceLdpas implements Comparator<ipFwdIfaceLdpas> {

    public addrIP ip;

    public String pwd;

    public int compare(ipFwdIfaceLdpas o1, ipFwdIfaceLdpas o2) {
        return o1.ip.compare(o1.ip, o2.ip);
    }

}
