package net.freertr.cfg;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import net.freertr.addr.addrIP;
import net.freertr.ip.ipCor4;
import net.freertr.ip.ipCor6;
import net.freertr.ip.ipFwd;
import net.freertr.ip.ipFwdIface;
import net.freertr.ip.ipFwdMcast;
import net.freertr.ip.ipFwdRoute;
import net.freertr.ip.ipFwdTab;
import net.freertr.ip.ipIcmp4;
import net.freertr.ip.ipIcmp6;
import net.freertr.ip.ipMhost4;
import net.freertr.ip.ipMhost6;
import net.freertr.ipx.ipxFwd;
import net.freertr.prt.prtDccp;
import net.freertr.prt.prtLudp;
import net.freertr.prt.prtSctp;
import net.freertr.prt.prtTcp;
import net.freertr.prt.prtUdp;
import net.freertr.tab.tabGen;
import net.freertr.tab.tabLabel;
import net.freertr.tab.tabLabelEntry;
import net.freertr.tab.tabNatCfgN;
import net.freertr.tab.tabPbrN;
import net.freertr.tab.tabQos;
import net.freertr.tab.tabRtrmapN;
import net.freertr.user.userFilter;
import net.freertr.user.userFormat;
import net.freertr.user.userHelping;
import net.freertr.util.bits;
import net.freertr.util.cmds;

/**
 * one vrf configuration
 *
 * @author matecsaba
 */
public class cfgVrf implements Comparator<cfgVrf>, cfgGeneric {

    /**
     * name of this vrf
     */
    public String name;

    /**
     * description of this vrf
     */
    public String description = "";

    /**
     * hidden vrf
     */
    protected boolean hidden = false;

    /**
     * route distinguisher
     */
    public long rd;

    /**
     * multicast distribution tree ipv4
     */
    public boolean mdt4;

    /**
     * multicast distribution tree ipv6
     */
    public boolean mdt6;

    /**
     * route target import
     */
    public List<Long> rtImp = new ArrayList<Long>();

    /**
     * route target export
     */
    public List<Long> rtExp = new ArrayList<Long>();

    /**
     * ipx forwarder
     */
    public ipxFwd ipx;

    /**
     * ipv4 forwarder
     */
    public ipFwd fwd4;

    /**
     * ipv6 forwarder
     */
    public ipFwd fwd6;

    /**
     * ipv4 core
     */
    public ipCor4 core4;

    /**
     * ipv6 core
     */
    public ipCor6 core6;

    /**
     * icmp v4
     */
    public ipIcmp4 icmp4;

    /**
     * icmp v6
     */
    public ipIcmp6 icmp6;

    /**
     * igmp v4
     */
    public ipMhost4 mhst4;

    /**
     * mld v6
     */
    public ipMhost6 mhst6;

    /**
     * udp for ipv4
     */
    public prtUdp udp4;

    /**
     * udp for ipv6
     */
    public prtUdp udp6;

    /**
     * ludp for ipv4
     */
    public prtLudp ludp4;

    /**
     * ludp for ipv6
     */
    public prtLudp ludp6;

    /**
     * dccp for ipv4
     */
    public prtDccp dccp4;

    /**
     * dccp for ipv6
     */
    public prtDccp dccp6;

    /**
     * sctp for ipv4
     */
    public prtSctp sctp4;

    /**
     * sctp for ipv6
     */
    public prtSctp sctp6;

    /**
     * tcp for ipv4
     */
    public prtTcp tcp4;

    /**
     * tcp for ipv6
     */
    public prtTcp tcp6;

    /**
     * allocate label for prefix
     */
    public ipFwd.labelMode labelMode = ipFwd.labelMode.common;

    /**
     * mpls propagate ip ttl
     */
    public boolean mplsPropTtl = true;

    /**
     * mpls extended report
     */
    public boolean mplsExtRep = true;

    /**
     * unreachable interval
     */
    public int unreachInt = 0;

    /**
     * ruin remote pmtud
     */
    public boolean ruinPmtuD = false;

    /**
     * ipv4 label filter
     */
    public cfgPrfxlst label4fltr = null;

    /**
     * ipv6 label filter
     */
    public cfgPrfxlst label6fltr = null;

    /**
     * ipv4 label value
     */
    public int label4comm = 0;

    /**
     * ipv6 label value
     */
    public int label6comm = 0;

    /**
     * ipv4 interface value
     */
    public int iface4start = 0;

    /**
     * ipv6 interface value
     */
    public int iface6start = 0;

    /**
     * ipv4 import list
     */
    public cfgPrfxlst import4list = null;

    /**
     * ipv6 import list
     */
    public cfgPrfxlst import6list = null;

    /**
     * ipv4 export list
     */
    public cfgPrfxlst export4list = null;

    /**
     * ipv6 export list
     */
    public cfgPrfxlst export6list = null;

    /**
     * ipv4 import map
     */
    public cfgRoump import4map = null;

    /**
     * ipv6 import map
     */
    public cfgRoump import6map = null;

    /**
     * ipv4 export map
     */
    public cfgRoump export4map = null;

    /**
     * ipv6 export map
     */
    public cfgRoump export6map = null;

    /**
     * ipv4 import policy
     */
    public cfgRouplc import4pol = null;

    /**
     * ipv6 import policy
     */
    public cfgRouplc import6pol = null;

    /**
     * ipv4 export policy
     */
    public cfgRouplc export4pol = null;

    /**
     * ipv6 export policy
     */
    public cfgRouplc export6pol = null;

    /**
     * ipv4 packet filter
     */
    public cfgAceslst packet4fltr = null;

    /**
     * ipv6 packet filter
     */
    public cfgAceslst packet6fltr = null;

    /**
     * ipv4 counter map
     */
    public cfgRoump counter4map = null;

    /**
     * ipv6 counter map
     */
    public cfgRoump counter6map = null;

    /**
     * ipv4 dapp
     */
    public cfgPlymp dapp4 = null;

    /**
     * ipv6 dapp
     */
    public cfgPlymp dapp6 = null;

    /**
     * ipv4 receive copp
     */
    public cfgPlymp copp4in = null;

    /**
     * ipv4 transmit copp
     */
    public cfgPlymp copp4out = null;

    /**
     * ipv6 receive copp
     */
    public cfgPlymp copp6in = null;

    /**
     * ipv6 transmit copp
     */
    public cfgPlymp copp6out = null;

    /**
     * defaults text
     */
    public final static String[] defaultL = {
        "vrf definition .*! no description",
        "vrf definition .*! rd 0:0",
        "vrf definition .*! rt-import",
        "vrf definition .*! rt-export",
        "vrf definition .*! label-mode per-vrf",
        "vrf definition .*! propagate-ttl",
        "vrf definition .*! report-labels",
        "vrf definition .*! unreach-interval 0",
        "vrf definition .*! no punish-pmtud",
        "vrf definition .*! no mdt4",
        "vrf definition .*! no mdt6",
        "vrf definition .*! no label4filter",
        "vrf definition .*! no label6filter",
        "vrf definition .*! label4common 0",
        "vrf definition .*! label6common 0",
        "vrf definition .*! iface4start 0",
        "vrf definition .*! iface6start 0",
        "vrf definition .*! no import4list",
        "vrf definition .*! no import6list",
        "vrf definition .*! no export4list",
        "vrf definition .*! no export6list",
        "vrf definition .*! no import4map",
        "vrf definition .*! no import6map",
        "vrf definition .*! no export4map",
        "vrf definition .*! no export6map",
        "vrf definition .*! no import4policy",
        "vrf definition .*! no import6policy",
        "vrf definition .*! no export4policy",
        "vrf definition .*! no export6policy",
        "vrf definition .*! no route4limit",
        "vrf definition .*! no route6limit",
        "vrf definition .*! no dapp4",
        "vrf definition .*! no dapp6",
        "vrf definition .*! no copp4in",
        "vrf definition .*! no copp4out",
        "vrf definition .*! no copp6in",
        "vrf definition .*! no copp6out",
        "vrf definition .*! no packet4filter",
        "vrf definition .*! no packet6filter",
        "vrf definition .*! no counter4map",
        "vrf definition .*! no counter6map",
        "!ipv[4|6] nat .* sequence .* timeout 300000",
        "!ipv[4|6] nat .* sequence .* sessions 0"
    };

    /**
     * defaults filter
     */
    public static tabGen<userFilter> defaultF;

    /**
     * get fwd instance
     *
     * @param adr address to test
     * @return fwd instance
     */
    public ipFwd getFwd(addrIP adr) {
        if (adr.isIPv4()) {
            return fwd4;
        } else {
            return fwd6;
        }
    }

    /**
     * get tcp instance
     *
     * @param adr address to test
     * @return tcp instance
     */
    public prtTcp getTcp(addrIP adr) {
        if (adr.isIPv4()) {
            return tcp4;
        } else {
            return tcp6;
        }
    }

    /**
     * get udp instance
     *
     * @param adr address to test
     * @return udp instance
     */
    public prtUdp getUdp(addrIP adr) {
        if (adr.isIPv4()) {
            return udp4;
        } else {
            return udp6;
        }
    }

    /**
     * get ludp instance
     *
     * @param adr address to test
     * @return ludp instance
     */
    public prtLudp getLudp(addrIP adr) {
        if (adr.isIPv4()) {
            return ludp4;
        } else {
            return ludp6;
        }
    }

    /**
     * get dccp instance
     *
     * @param adr address to test
     * @return dccp instance
     */
    public prtDccp getDccp(addrIP adr) {
        if (adr.isIPv4()) {
            return dccp4;
        } else {
            return dccp6;
        }
    }

    /**
     * get dccp instance
     *
     * @param adr address to test
     * @return sctp instance
     */
    public prtSctp getSctp(addrIP adr) {
        if (adr.isIPv4()) {
            return sctp4;
        } else {
            return sctp6;
        }
    }

    /**
     * get other interface
     *
     * @param fwd forwarder
     * @param ifc interface
     * @return other interface, null if not found
     */
    public ipFwdIface getOtherIface(ipFwd fwd, ipFwdIface ifc) {
        ipFwd of = null;
        if (fwd == fwd4) {
            of = fwd6;
        }
        if (fwd == fwd6) {
            of = fwd4;
        }
        if (of == null) {
            return null;
        }
        ifc = fwd.ifaces.find(ifc);
        if (ifc == null) {
            return null;
        }
        ifc = ifc.otherHandler;
        if (ifc == null) {
            return null;
        }
        ifc = of.ifaces.find(ifc);
        if (ifc == null) {
            return null;
        }
        return ifc;
    }

    public int compare(cfgVrf o1, cfgVrf o2) {
        return o1.name.toLowerCase().compareTo(o2.name.toLowerCase());
    }

    public String toString() {
        return "vrf " + name;
    }

    /**
     * create new vrf
     *
     * @param nam name of vrf
     */
    public cfgVrf(String nam) {
        name = nam.trim();
    }

    /**
     * start this vrf now
     */
    public synchronized void startNow() {
        core4 = new ipCor4();
        core6 = new ipCor6();
        icmp4 = new ipIcmp4();
        icmp6 = new ipIcmp6();
        mhst4 = new ipMhost4();
        mhst6 = new ipMhost6();
        ipx = new ipxFwd(name);
        fwd4 = new ipFwd(core4, icmp4, mhst4, name, name + ":4");
        fwd6 = new ipFwd(core6, icmp6, mhst6, name, name + ":6");
        udp4 = new prtUdp(fwd4);
        udp6 = new prtUdp(fwd6);
        ludp4 = new prtLudp(fwd4);
        ludp6 = new prtLudp(fwd6);
        dccp4 = new prtDccp(fwd4);
        dccp6 = new prtDccp(fwd6);
        sctp4 = new prtSctp(fwd4);
        sctp6 = new prtSctp(fwd6);
        tcp4 = new prtTcp(fwd4);
        tcp6 = new prtTcp(fwd6);
    }

    /**
     * destroy this vrf
     */
    public synchronized void closeUp() {
        ipx.stopThisVrf();
        fwd4.stopThisVrf();
        fwd6.stopThisVrf();
    }

    /**
     * close connections
     */
    public synchronized void closeConns() {
        udp4.closeConns();
        udp6.closeConns();
        ludp4.closeConns();
        ludp6.closeConns();
        dccp4.closeConns();
        dccp6.closeConns();
        sctp4.closeConns();
        sctp6.closeConns();
        tcp4.closeConns();
        tcp6.closeConns();
    }

    private void addRoutes(List<String> l, int p, ipFwd f) {
        for (int i = 0; i < f.staticU.size(); i++) {
            ipFwdRoute prf = f.staticU.get(i);
            l.add("ipv" + p + " route " + name + " " + prf);
        }
    }

    private void addMroutes(List<String> l, int p, ipFwd f) {
        for (int i = 0; i < f.staticM.size(); i++) {
            ipFwdRoute prf = f.staticM.get(i);
            l.add("ipv" + p + " mroute " + name + " " + prf);
        }
    }

    private void addCfgNats(List<String> l, int p, ipFwd f) {
        for (int i = 0; i < f.natCfg.size(); i++) {
            tabNatCfgN nat = f.natCfg.get(i);
            l.addAll(nat.usrString("ipv" + p + " nat " + name + " "));
        }
    }

    private void addCfgPbrs(List<String> l, int p, ipFwd f) {
        for (int i = 0; i < f.pbrCfg.size(); i++) {
            tabPbrN pbr = f.pbrCfg.get(i);
            l.addAll(pbr.usrString("ipv" + p + " pbr " + name + " "));
        }
    }

    private void addCfgMcast(List<String> l, int p, ipFwd f) {
        for (int o = 0; o < f.groups.size(); o++) {
            ipFwdMcast grp = f.groups.get(o);
            if (grp == null) {
                continue;
            }
            if (!grp.configG) {
                continue;
            }
            l.add("ipv" + p + " multicast " + name + " join-group " + grp.group + " " + grp.source);
        }
    }

    private void addCfgFlow(List<String> l, int p, ipFwd f) {
        if (f.netflow == null) {
            return;
        }
        l.add("ipv" + p + " flow " + name + " " + f.netflow);
    }

    public List<String> getShRun(int filter) {
        List<String> l = new ArrayList<String>();
        if (hidden) {
            return l;
        }
        l.add("vrf definition " + name);
        cmds.cfgLine(l, description.length() < 1, cmds.tabulator, "description", description);
        l.add(cmds.tabulator + "rd " + tabRtrmapN.rd2string(rd));
        String s = "";
        for (int i = 0; i < rtImp.size(); i++) {
            s += " " + tabRtrmapN.rd2string(rtImp.get(i));
        }
        l.add(cmds.tabulator + "rt-import" + s);
        s = "";
        for (int i = 0; i < rtExp.size(); i++) {
            s += " " + tabRtrmapN.rd2string(rtExp.get(i));
        }
        l.add(cmds.tabulator + "rt-export" + s);
        s = "unknown";
        switch (labelMode) {
            case all:
                s = "per-prefix";
                break;
            case igp:
                s = "all-igp";
                break;
            case host:
                s = "host-route";
                break;
            case common:
                s = "per-vrf";
                break;
        }
        l.add(cmds.tabulator + "iface4start " + iface4start);
        l.add(cmds.tabulator + "iface6start " + iface6start);
        l.add(cmds.tabulator + "label-mode " + s);
        l.add(cmds.tabulator + "label4common " + label4comm);
        l.add(cmds.tabulator + "label6common " + label6comm);
        cmds.cfgLine(l, !mplsPropTtl, cmds.tabulator, "propagate-ttl", "");
        cmds.cfgLine(l, !mplsExtRep, cmds.tabulator, "report-labels", "");
        l.add(cmds.tabulator + "unreach-interval " + unreachInt);
        cmds.cfgLine(l, !ruinPmtuD, cmds.tabulator, "punish-pmtud", "");
        cmds.cfgLine(l, label4fltr == null, cmds.tabulator, "label4filter", "" + label4fltr);
        cmds.cfgLine(l, label6fltr == null, cmds.tabulator, "label6filter", "" + label6fltr);
        cmds.cfgLine(l, import4list == null, cmds.tabulator, "import4list", "" + import4list);
        cmds.cfgLine(l, import6list == null, cmds.tabulator, "import6list", "" + import6list);
        cmds.cfgLine(l, export4list == null, cmds.tabulator, "export4list", "" + export4list);
        cmds.cfgLine(l, export6list == null, cmds.tabulator, "export6list", "" + export6list);
        cmds.cfgLine(l, import4map == null, cmds.tabulator, "import4map", "" + import4map);
        cmds.cfgLine(l, import6map == null, cmds.tabulator, "import6map", "" + import6map);
        cmds.cfgLine(l, export4map == null, cmds.tabulator, "export4map", "" + export4map);
        cmds.cfgLine(l, export6map == null, cmds.tabulator, "export6map", "" + export6map);
        cmds.cfgLine(l, import4pol == null, cmds.tabulator, "import4policy", "" + import4pol);
        cmds.cfgLine(l, import6pol == null, cmds.tabulator, "import6policy", "" + import6pol);
        cmds.cfgLine(l, export4pol == null, cmds.tabulator, "export4policy", "" + export4pol);
        cmds.cfgLine(l, export6pol == null, cmds.tabulator, "export6policy", "" + export6pol);
        cmds.cfgLine(l, fwd4.routeLimit < 1, cmds.tabulator, "route4limit", "" + fwd4.routeLimit);
        cmds.cfgLine(l, fwd6.routeLimit < 1, cmds.tabulator, "route6limit", "" + fwd6.routeLimit);
        cmds.cfgLine(l, dapp4 == null, cmds.tabulator, "dapp4", "" + dapp4);
        cmds.cfgLine(l, dapp6 == null, cmds.tabulator, "dapp6", "" + dapp6);
        cmds.cfgLine(l, copp4in == null, cmds.tabulator, "copp4in", "" + copp4in);
        cmds.cfgLine(l, copp4out == null, cmds.tabulator, "copp4out", "" + copp4out);
        cmds.cfgLine(l, copp6in == null, cmds.tabulator, "copp6in", "" + copp6in);
        cmds.cfgLine(l, copp6out == null, cmds.tabulator, "copp6out", "" + copp6out);
        cmds.cfgLine(l, packet4fltr == null, cmds.tabulator, "packet4filter", "" + packet4fltr);
        cmds.cfgLine(l, packet6fltr == null, cmds.tabulator, "packet6filter", "" + packet6fltr);
        cmds.cfgLine(l, counter4map == null, cmds.tabulator, "counter4map", "" + counter4map);
        cmds.cfgLine(l, counter6map == null, cmds.tabulator, "counter6map", "" + counter6map);
        cmds.cfgLine(l, !mdt4, cmds.tabulator, "mdt4", "");
        cmds.cfgLine(l, !mdt6, cmds.tabulator, "mdt6", "");
        l.add(cmds.tabulator + cmds.finish);
        l.add(cmds.comment);
        if ((filter & 1) == 0) {
            return l;
        }
        return userFilter.filterText(l, defaultF);
    }

    /**
     * get post all config
     *
     * @param filter filter defaults
     * @return list of config lines
     */
    public synchronized List<String> getShRun2(int filter) {
        List<String> l = new ArrayList<String>();
        if (hidden) {
            return l;
        }
        ipx.getShRun(l);
        l.add(cmds.comment);
        addRoutes(l, 4, fwd4);
        l.add(cmds.comment);
        addRoutes(l, 6, fwd6);
        l.add(cmds.comment);
        addMroutes(l, 4, fwd4);
        l.add(cmds.comment);
        addMroutes(l, 6, fwd6);
        l.add(cmds.comment);
        addCfgNats(l, 4, fwd4);
        l.add(cmds.comment);
        addCfgNats(l, 6, fwd6);
        l.add(cmds.comment);
        addCfgPbrs(l, 4, fwd4);
        l.add(cmds.comment);
        addCfgPbrs(l, 6, fwd6);
        l.add(cmds.comment);
        addCfgMcast(l, 4, fwd4);
        l.add(cmds.comment);
        addCfgMcast(l, 6, fwd6);
        l.add(cmds.comment);
        addCfgFlow(l, 4, fwd4);
        l.add(cmds.comment);
        addCfgFlow(l, 6, fwd6);
        l.add(cmds.comment);
        if ((filter & 1) == 0) {
            return l;
        }
        return userFilter.filterText(l, defaultF);
    }

    public void getHelp(userHelping l) {
        l.add(null, "1 2,. description        description of this vrf");
        l.add(null, "2 2,.   [text]           text describing this vrf");
        l.add(null, "1 2  rename              rename this vrf");
        l.add(null, "2 .    <str>             set new name of vrf");
        l.add(null, "1 2  rd                  specify route distinguisher");
        l.add(null, "2 .    <rd>              rd in ASnum:IDnum format");
        l.add(null, "1 2  rt-both             specify route target");
        l.add(null, "2 2,.  <rt>              rt in ASnum:IDnum format");
        l.add(null, "1 2  rt-import           specify route target import");
        l.add(null, "2 2,.  <rt>              rt in ASnum:IDnum format");
        l.add(null, "1 2  rt-export           specify route target export");
        l.add(null, "2 2,.  <rt>              rt in ASnum:IDnum format");
        l.add(null, ".1 . punish-pmtud        send back mtu exceeded if needed");
        l.add(null, "1 2  unreach-interval    rate limit icmp generation");
        l.add(null, "2 .    <num>             millisecs between them");
        l.add(null, "1 2  route4limit         maximum ipv4 routes allowed");
        l.add(null, "2 .    <num>             number of routes");
        l.add(null, "1 2  route6limit         maximum ipv6 routes allowed");
        l.add(null, "2 .    <num>             number of routes");
        l.add(null, "1 2  label4filter        specify ipv4 label filter");
        l.add(null, "2 .    <name:pl>         name of prefix list");
        l.add(null, "1 2  label6filter        specify ipv6 label filter");
        l.add(null, "2 .    <name:pl>         name of prefix list");
        l.add(null, "1 2  label4common        specify ipv4 common label");
        l.add(null, "2 .    <num>             label value");
        l.add(null, "1 2  label6common        specify ipv6 common label");
        l.add(null, "2 .    <num>             label value");
        l.add(null, "1 2  iface4start         specify ipv4 interface index");
        l.add(null, "2 .    <num>             start index");
        l.add(null, "1 2  iface6start         specify ipv6 interface index");
        l.add(null, "2 .    <num>             start index");
        l.add(null, "1 2  import4list         specify ipv4 import filter");
        l.add(null, "2 .    <name:pl>         name of prefix list");
        l.add(null, "1 2  import6list         specify ipv6 import filter");
        l.add(null, "2 .    <name:pl>         name of prefix list");
        l.add(null, "1 2  export4list         specify ipv4 export filter");
        l.add(null, "2 .    <name:pl>         name of prefix list");
        l.add(null, "1 2  export6list         specify ipv6 export filter");
        l.add(null, "2 .    <name:pl>         name of prefix list");
        l.add(null, "1 2  import4map          specify ipv4 import filter");
        l.add(null, "2 .    <name:rm>         name of route map");
        l.add(null, "1 2  import6map          specify ipv6 import filter");
        l.add(null, "2 .    <name:rm>         name of route map");
        l.add(null, "1 2  export4map          specify ipv4 export filter");
        l.add(null, "2 .    <name:rm>         name of route map");
        l.add(null, "1 2  export6map          specify ipv6 export filter");
        l.add(null, "2 .    <name:rm>         name of route map");
        l.add(null, "1 2  import4policy       specify ipv4 import filter");
        l.add(null, "2 .    <name:rpl>        name of route policy");
        l.add(null, "1 2  import6policy       specify ipv6 import filter");
        l.add(null, "2 .    <name:rpl>        name of route policy");
        l.add(null, "1 2  export4policy       specify ipv4 export filter");
        l.add(null, "2 .    <name:rpl>        name of route policy");
        l.add(null, "1 2  export6policy       specify ipv6 export filter");
        l.add(null, "2 .    <name:rpl>        name of route policy");
        l.add(null, "1 2  dapp4               specify ipv4 data plane policer");
        l.add(null, "2 .    <name:pm>         name of policy map");
        l.add(null, "1 2  dapp6               specify ipv6 data plane policer");
        l.add(null, "2 .    <name:pm>         name of policy map");
        l.add(null, "1 2  copp4in             specify ipv4 receive control plane policer");
        l.add(null, "2 .    <name:pm>         name of policy map");
        l.add(null, "1 2  copp4out            specify ipv4 transmit control plane policer");
        l.add(null, "2 .    <name:pm>         name of policy map");
        l.add(null, "1 2  copp6in             specify ipv6 receive control plane policer");
        l.add(null, "2 .    <name:pm>         name of policy map");
        l.add(null, "1 2  copp6out            specify ipv6 transmit control plane policer");
        l.add(null, "2 .    <name:pm>         name of policy map");
        l.add(null, "1 2  packet4filter       specify ipv4 packet filter");
        l.add(null, "2 .    <name:acl>        name of access list");
        l.add(null, "1 2  packet6filter       specify ipv6 packet filter");
        l.add(null, "2 .    <name:acl>        name of access list");
        l.add(null, "1 2  counter4map         specify ipv4 traffic counter");
        l.add(null, "2 .    <name:acl>        name of route map");
        l.add(null, "1 2  counter6map         specify ipv6 traffic counter");
        l.add(null, "2 .    <name:acl>        name of route map");
        l.add(null, "1 .  propagate-ttl       specify to copy ip ttl to mpls ttl");
        l.add(null, "1 .  report-labels       append icmp extension with labels");
        l.add(null, "1 .  mdt4                enable multicast distribution tree for ipv4");
        l.add(null, "1 .  mdt6                enable multicast distribution tree for ipv6");
        l.add(null, "1 2  label-mode          specify label allocation mode");
        l.add(null, "2 .    per-prefix        label for all prefixes");
        l.add(null, "2 .    all-igp           label for all igp prefixes");
        l.add(null, "2 .    host-route        label for host routes");
        l.add(null, "2 .    per-vrf           common label for the vrf");
    }

    public synchronized void doCfgStr(cmds cmd) {
        String a = cmd.word();
        if (a.equals("description")) {
            description = cmd.getRemaining();
            return;
        }
        if (a.equals("rename")) {
            a = cmd.word();
            cfgVrf v = cfgAll.vrfFind(a, false);
            if (v != null) {
                cmd.error("vrf already exists");
                return;
            }
            name = a;
            return;
        }
        if (a.equals("rd")) {
            rd = tabRtrmapN.string2rd(cmd.word());
            fwd4.rd = rd;
            fwd6.rd = rd;
            return;
        }
        if (a.equals("rt-import")) {
            rtImp = new ArrayList<Long>();
            for (;;) {
                a = cmd.word();
                if (a.length() < 1) {
                    break;
                }
                rtImp.add(tabRtrmapN.string2rd(a));
            }
            fwd4.routerStaticChg();
            fwd6.routerStaticChg();
            return;
        }
        if (a.equals("rt-export")) {
            rtExp = new ArrayList<Long>();
            for (;;) {
                a = cmd.word();
                if (a.length() < 1) {
                    break;
                }
                rtExp.add(tabRtrmapN.string2rd(a));
            }
            fwd4.routerStaticChg();
            fwd6.routerStaticChg();
            return;
        }
        if (a.equals("rt-both")) {
            rtImp = new ArrayList<Long>();
            rtExp = new ArrayList<Long>();
            for (;;) {
                a = cmd.word();
                if (a.length() < 1) {
                    break;
                }
                rtImp.add(tabRtrmapN.string2rd(a));
                rtExp.add(tabRtrmapN.string2rd(a));
            }
            fwd4.routerStaticChg();
            fwd6.routerStaticChg();
            return;
        }
        if (a.equals("route4limit")) {
            fwd4.routeLimit = bits.str2num(cmd.word());
            fwd4.routerStaticChg();
            return;
        }
        if (a.equals("route6limit")) {
            fwd6.routeLimit = bits.str2num(cmd.word());
            fwd6.routerStaticChg();
            return;
        }
        if (a.equals("mdt4")) {
            mdt4 = true;
            fwd4.mdt = true;
            fwd4.routerStaticChg();
            return;
        }
        if (a.equals("mdt6")) {
            mdt6 = true;
            fwd6.mdt = true;
            fwd6.routerStaticChg();
            return;
        }
        if (a.equals("label-mode")) {
            a = cmd.word();
            if (a.equals("per-prefix")) {
                labelMode = ipFwd.labelMode.all;
            }
            if (a.equals("all-igp")) {
                labelMode = ipFwd.labelMode.igp;
            }
            if (a.equals("host-route")) {
                labelMode = ipFwd.labelMode.host;
            }
            if (a.equals("per-vrf")) {
                labelMode = ipFwd.labelMode.common;
            }
            fwd4.prefixMode = labelMode;
            fwd6.prefixMode = labelMode;
            fwd4.routerStaticChg();
            fwd6.routerStaticChg();
            return;
        }
        if (a.equals("iface4start")) {
            iface4start = bits.str2num(cmd.word());
            fwd4.nextIfaceNumber = iface4start;
            return;
        }
        if (a.equals("iface6start")) {
            iface6start = bits.str2num(cmd.word());
            fwd6.nextIfaceNumber = iface6start;
            return;
        }
        if (a.equals("label4common")) {
            label4comm = bits.str2num(cmd.word());
            tabLabelEntry[] ntry = tabLabel.allocate(1, label4comm, 1);
            tabLabelEntry old = fwd4.commonLabel;
            fwd4.commonLabel = ntry[0];
            fwd4.routerStaticChg();
            tabLabel.release(old, 1);
            return;
        }
        if (a.equals("label6common")) {
            label6comm = bits.str2num(cmd.word());
            tabLabelEntry[] ntry = tabLabel.allocate(1, label6comm, 1);
            tabLabelEntry old = fwd6.commonLabel;
            fwd6.commonLabel = ntry[0];
            fwd6.routerStaticChg();
            tabLabel.release(old, 1);
            return;
        }
        if (a.equals("unreach-interval")) {
            unreachInt = bits.str2num(cmd.word());
            fwd4.unreachInt = unreachInt;
            fwd6.unreachInt = unreachInt;
            return;
        }
        if (a.equals("punish-pmtud")) {
            ruinPmtuD = true;
            fwd4.ruinPmtuD = true;
            fwd6.ruinPmtuD = true;
            return;
        }
        if (a.equals("report-labels")) {
            mplsExtRep = true;
            fwd4.mplsExtRep = true;
            fwd6.mplsExtRep = true;
            return;
        }
        if (a.equals("propagate-ttl")) {
            mplsPropTtl = true;
            fwd4.mplsPropTtl = true;
            fwd6.mplsPropTtl = true;
            return;
        }
        if (a.equals("label4filter")) {
            label4fltr = cfgAll.prfxFind(cmd.word(), false);
            if (label4fltr == null) {
                cmd.error("no such prefix list");
                return;
            }
            fwd4.labelFilter = label4fltr.prflst;
            fwd4.routerStaticChg();
            return;
        }
        if (a.equals("label6filter")) {
            label6fltr = cfgAll.prfxFind(cmd.word(), false);
            if (label6fltr == null) {
                cmd.error("no such prefix list");
                return;
            }
            fwd6.labelFilter = label6fltr.prflst;
            fwd6.routerStaticChg();
            return;
        }
        if (a.equals("import4list")) {
            import4list = cfgAll.prfxFind(cmd.word(), false);
            if (import4list == null) {
                cmd.error("no such prefix list");
                return;
            }
            fwd4.importList = import4list.prflst;
            fwd4.routerStaticChg();
            return;
        }
        if (a.equals("import6list")) {
            import6list = cfgAll.prfxFind(cmd.word(), false);
            if (import6list == null) {
                cmd.error("no such prefix list");
                return;
            }
            fwd6.importList = import6list.prflst;
            fwd6.routerStaticChg();
            return;
        }
        if (a.equals("export4list")) {
            export4list = cfgAll.prfxFind(cmd.word(), false);
            if (export4list == null) {
                cmd.error("no such prefix list");
                return;
            }
            fwd4.exportList = export4list.prflst;
            fwd4.routerStaticChg();
            return;
        }
        if (a.equals("export6list")) {
            export6list = cfgAll.prfxFind(cmd.word(), false);
            if (export6list == null) {
                cmd.error("no such prefix list");
                return;
            }
            fwd6.exportList = export6list.prflst;
            fwd6.routerStaticChg();
            return;
        }
        if (a.equals("import4map")) {
            import4map = cfgAll.rtmpFind(cmd.word(), false);
            if (import4map == null) {
                cmd.error("no such route map");
                return;
            }
            fwd4.importMap = import4map.roumap;
            fwd4.routerStaticChg();
            return;
        }
        if (a.equals("import6map")) {
            import6map = cfgAll.rtmpFind(cmd.word(), false);
            if (import6map == null) {
                cmd.error("no such route map");
                return;
            }
            fwd6.importMap = import6map.roumap;
            fwd6.routerStaticChg();
            return;
        }
        if (a.equals("export4map")) {
            export4map = cfgAll.rtmpFind(cmd.word(), false);
            if (export4map == null) {
                cmd.error("no such route map");
                return;
            }
            fwd4.exportMap = export4map.roumap;
            fwd4.routerStaticChg();
            return;
        }
        if (a.equals("export6map")) {
            export6map = cfgAll.rtmpFind(cmd.word(), false);
            if (export6map == null) {
                cmd.error("no such route map");
                return;
            }
            fwd6.exportMap = export6map.roumap;
            fwd6.routerStaticChg();
            return;
        }
        if (a.equals("import4policy")) {
            import4pol = cfgAll.rtplFind(cmd.word(), false);
            if (import4pol == null) {
                cmd.error("no such route policy");
                return;
            }
            fwd4.importPol = import4pol.rouplc;
            fwd4.routerStaticChg();
            return;
        }
        if (a.equals("import6policy")) {
            import6pol = cfgAll.rtplFind(cmd.word(), false);
            if (import6pol == null) {
                cmd.error("no such route policy");
                return;
            }
            fwd6.importPol = import6pol.rouplc;
            fwd6.routerStaticChg();
            return;
        }
        if (a.equals("export4policy")) {
            export4pol = cfgAll.rtplFind(cmd.word(), false);
            if (export4pol == null) {
                cmd.error("no such route policy");
                return;
            }
            fwd4.exportPol = export4pol.rouplc;
            fwd4.routerStaticChg();
            return;
        }
        if (a.equals("export6policy")) {
            export6pol = cfgAll.rtplFind(cmd.word(), false);
            if (export6pol == null) {
                cmd.error("no such route policy");
                return;
            }
            fwd6.exportPol = export6pol.rouplc;
            fwd6.routerStaticChg();
            return;
        }
        if (a.equals("dapp4")) {
            dapp4 = cfgAll.plmpFind(cmd.word(), false);
            if (dapp4 == null) {
                cmd.error("no such policy map");
                return;
            }
            tabQos wrkr = tabQos.convertPolicy(dapp4.plcmap);
            if (wrkr == null) {
                cmd.error("error applying policy map");
                return;
            }
            fwd4.dapp = wrkr;
            return;
        }
        if (a.equals("dapp6")) {
            dapp6 = cfgAll.plmpFind(cmd.word(), false);
            if (dapp6 == null) {
                cmd.error("no such policy map");
                return;
            }
            tabQos wrkr = tabQos.convertPolicy(dapp6.plcmap);
            if (wrkr == null) {
                cmd.error("error applying policy map");
                return;
            }
            fwd6.dapp = wrkr;
            return;
        }
        if (a.equals("copp4in")) {
            copp4in = cfgAll.plmpFind(cmd.word(), false);
            if (copp4in == null) {
                cmd.error("no such policy map");
                return;
            }
            tabQos wrkr = tabQos.convertPolicy(copp4in.plcmap);
            if (wrkr == null) {
                cmd.error("error applying policy map");
                return;
            }
            fwd4.coppIn = wrkr;
            return;
        }
        if (a.equals("copp4out")) {
            copp4out = cfgAll.plmpFind(cmd.word(), false);
            if (copp4out == null) {
                cmd.error("no such policy map");
                return;
            }
            tabQos wrkr = tabQos.convertPolicy(copp4out.plcmap);
            if (wrkr == null) {
                cmd.error("error applying policy map");
                return;
            }
            fwd4.coppOut = wrkr;
            return;
        }
        if (a.equals("copp6in")) {
            copp6in = cfgAll.plmpFind(cmd.word(), false);
            if (copp6in == null) {
                cmd.error("no such policy map");
                return;
            }
            tabQos wrkr = tabQos.convertPolicy(copp6in.plcmap);
            if (wrkr == null) {
                cmd.error("error applying policy map");
                return;
            }
            fwd6.coppIn = wrkr;
            return;
        }
        if (a.equals("copp6out")) {
            copp6out = cfgAll.plmpFind(cmd.word(), false);
            if (copp6out == null) {
                cmd.error("no such policy map");
                return;
            }
            tabQos wrkr = tabQos.convertPolicy(copp6out.plcmap);
            if (wrkr == null) {
                cmd.error("error applying policy map");
                return;
            }
            fwd6.coppOut = wrkr;
            return;
        }
        if (a.equals("packet4filter")) {
            packet4fltr = cfgAll.aclsFind(cmd.word(), false);
            if (packet4fltr == null) {
                cmd.error("no such access list");
                return;
            }
            packet4fltr.aceslst.myCor = core4;
            packet4fltr.aceslst.myIcmp = icmp4;
            fwd4.packetFilter = packet4fltr.aceslst;
            return;
        }
        if (a.equals("packet6filter")) {
            packet6fltr = cfgAll.aclsFind(cmd.word(), false);
            if (packet6fltr == null) {
                cmd.error("no such access list");
                return;
            }
            packet6fltr.aceslst.myCor = core6;
            packet6fltr.aceslst.myIcmp = icmp6;
            fwd6.packetFilter = packet6fltr.aceslst;
            return;
        }
        if (a.equals("counter4map")) {
            counter4map = cfgAll.rtmpFind(cmd.word(), false);
            if (counter4map == null) {
                cmd.error("no such route map");
                return;
            }
            fwd4.counterMap = counter4map.roumap;
            fwd4.routerStaticChg();
            return;
        }
        if (a.equals("counter6map")) {
            counter6map = cfgAll.rtmpFind(cmd.word(), false);
            if (counter6map == null) {
                cmd.error("no such route map");
                return;
            }
            fwd6.counterMap = counter6map.roumap;
            fwd6.routerStaticChg();
            return;
        }
        if (!a.equals("no")) {
            cmd.badCmd();
            return;
        }
        a = cmd.word();
        if (a.equals("rt-import")) {
            rtImp = new ArrayList<Long>();
            fwd4.routerStaticChg();
            fwd6.routerStaticChg();
            return;
        }
        if (a.equals("rt-export")) {
            rtExp = new ArrayList<Long>();
            fwd4.routerStaticChg();
            fwd6.routerStaticChg();
            return;
        }
        if (a.equals("rt-both")) {
            rtImp = new ArrayList<Long>();
            rtExp = new ArrayList<Long>();
            fwd4.routerStaticChg();
            fwd6.routerStaticChg();
            return;
        }
        if (a.equals("route4limit")) {
            fwd4.routeLimit = 0;
            fwd4.routerStaticChg();
            return;
        }
        if (a.equals("route6limit")) {
            fwd6.routeLimit = 0;
            fwd6.routerStaticChg();
            return;
        }
        if (a.equals("description")) {
            description = "";
            return;
        }
        if (a.equals("mdt4")) {
            mdt4 = false;
            fwd4.mdt = false;
            fwd4.routerStaticChg();
            return;
        }
        if (a.equals("mdt6")) {
            mdt6 = false;
            fwd6.mdt = false;
            fwd6.routerStaticChg();
            return;
        }
        if (a.equals("unreach-interval")) {
            unreachInt = 0;
            fwd4.unreachInt = 0;
            fwd6.unreachInt = 0;
            return;
        }
        if (a.equals("punish-pmtud")) {
            ruinPmtuD = false;
            fwd4.ruinPmtuD = false;
            fwd6.ruinPmtuD = false;
            return;
        }
        if (a.equals("report-labels")) {
            mplsExtRep = false;
            fwd4.mplsExtRep = false;
            fwd6.mplsExtRep = false;
            return;
        }
        if (a.equals("propagate-ttl")) {
            mplsPropTtl = false;
            fwd4.mplsPropTtl = false;
            fwd6.mplsPropTtl = false;
            return;
        }
        if (a.equals("label4filter")) {
            label4fltr = null;
            fwd4.labelFilter = null;
            fwd4.routerStaticChg();
            return;
        }
        if (a.equals("label6filter")) {
            label6fltr = null;
            fwd6.labelFilter = null;
            fwd6.routerStaticChg();
            return;
        }
        if (a.equals("iface4start")) {
            iface4start = 0;
            return;
        }
        if (a.equals("iface6start")) {
            iface6start = 0;
            return;
        }
        if (a.equals("label4common")) {
            label4comm = 0;
            return;
        }
        if (a.equals("label6common")) {
            label6comm = 0;
            return;
        }
        if (a.equals("import4list")) {
            import4list = null;
            fwd4.importList = null;
            fwd4.routerStaticChg();
            return;
        }
        if (a.equals("import6list")) {
            import6list = null;
            fwd6.importList = null;
            fwd6.routerStaticChg();
            return;
        }
        if (a.equals("export4list")) {
            export4list = null;
            fwd4.exportList = null;
            fwd4.routerStaticChg();
            return;
        }
        if (a.equals("export6list")) {
            export6list = null;
            fwd6.exportList = null;
            fwd6.routerStaticChg();
            return;
        }
        if (a.equals("import4map")) {
            import4map = null;
            fwd4.importMap = null;
            fwd4.routerStaticChg();
            return;
        }
        if (a.equals("import6map")) {
            import6map = null;
            fwd6.importMap = null;
            fwd6.routerStaticChg();
            return;
        }
        if (a.equals("export4map")) {
            export4map = null;
            fwd4.exportMap = null;
            fwd4.routerStaticChg();
            return;
        }
        if (a.equals("export6map")) {
            export6map = null;
            fwd6.exportMap = null;
            fwd6.routerStaticChg();
            return;
        }
        if (a.equals("import4policy")) {
            import4pol = null;
            fwd4.importPol = null;
            fwd4.routerStaticChg();
            return;
        }
        if (a.equals("import6policy")) {
            import6pol = null;
            fwd6.importPol = null;
            fwd6.routerStaticChg();
            return;
        }
        if (a.equals("export4policy")) {
            export4pol = null;
            fwd4.exportPol = null;
            fwd4.routerStaticChg();
            return;
        }
        if (a.equals("export6policy")) {
            export6pol = null;
            fwd6.exportPol = null;
            fwd6.routerStaticChg();
            return;
        }
        if (a.equals("dapp4")) {
            dapp4 = null;
            fwd4.dapp = null;
            return;
        }
        if (a.equals("dapp6")) {
            dapp6 = null;
            fwd6.dapp = null;
            return;
        }
        if (a.equals("copp4in")) {
            copp4in = null;
            fwd4.coppIn = null;
            return;
        }
        if (a.equals("copp4out")) {
            copp4out = null;
            fwd4.coppOut = null;
            return;
        }
        if (a.equals("copp6in")) {
            copp6in = null;
            fwd6.coppIn = null;
            return;
        }
        if (a.equals("copp6out")) {
            copp6out = null;
            fwd6.coppOut = null;
            return;
        }
        if (a.equals("packet4filter")) {
            packet4fltr = null;
            fwd4.packetFilter = null;
            return;
        }
        if (a.equals("packet6filter")) {
            packet6fltr = null;
            fwd6.packetFilter = null;
            return;
        }
        if (a.equals("counter4map")) {
            counter4map = null;
            fwd4.counterMap = null;
            fwd4.routerStaticChg();
            return;
        }
        if (a.equals("counter6map")) {
            counter6map = null;
            fwd6.counterMap = null;
            fwd6.routerStaticChg();
            return;
        }
        cmd.badCmd();
    }

    public String getPrompt() {
        return "vrf";
    }

    /**
     * get show sockets
     *
     * @param ver ip version
     * @return text to display
     */
    public userFormat getShSockets(int ver) {
        ipFwd fwd;
        prtUdp udp;
        prtLudp ludp;
        prtDccp dccp;
        prtSctp sctp;
        prtTcp tcp;
        if (ver == 4) {
            fwd = fwd4;
            udp = udp4;
            ludp = ludp4;
            dccp = dccp4;
            sctp = sctp4;
            tcp = tcp4;
        } else {
            fwd = fwd6;
            udp = udp6;
            ludp = ludp6;
            dccp = dccp6;
            sctp = sctp6;
            tcp = tcp6;
        }
        userFormat l = new userFormat("|", "lower|name|state|iface|local|remote|address|hit");
        ipFwdTab.listProtocols(fwd, l, "ipPrt");
        udp.listServers(l, "udpSrv");
        ludp.listServers(l, "ludpSrv");
        dccp.listServers(l, "dccpSrv");
        sctp.listServers(l, "sctpSrv");
        tcp.listServers(l, "tcpSrv");
        udp.listConnects(l, "udpCln");
        ludp.listConnects(l, "ludpCln");
        dccp.listConnects(l, "dccpCln");
        sctp.listConnects(l, "sctpCln");
        tcp.listConnects(l, "tcpCln");
        return l;
    }

}
