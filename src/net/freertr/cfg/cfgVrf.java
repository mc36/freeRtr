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
import net.freertr.tab.tabRouteUtil;
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
     * defaults text
     */
    public final static String[] defaultL = {
        "vrf definition .*! no description",
        "vrf definition .*! rd 0:0",
        "vrf definition .*! update4interval 0",
        "vrf definition .*! update6interval 0",
        "vrf definition .*! rt4import",
        "vrf definition .*! rt4export",
        "vrf definition .*! rt6import",
        "vrf definition .*! rt6export",
        "vrf definition .*! label4mode per-vrf",
        "vrf definition .*! label6mode per-vrf",
        "vrf definition .*! propagate4ttl",
        "vrf definition .*! report4labels",
        "vrf definition .*! unreach4interval 0",
        "vrf definition .*! no punish4pmtud",
        "vrf definition .*! propagate6ttl",
        "vrf definition .*! report6labels",
        "vrf definition .*! unreach6interval 0",
        "vrf definition .*! no punish6pmtud",
        "vrf definition .*! no mdt4",
        "vrf definition .*! no mdt6",
        "vrf definition .*! no label4filter",
        "vrf definition .*! no label6filter",
        "vrf definition .*! label4common 0",
        "vrf definition .*! label6common 0",
        "vrf definition .*! iface4start 0",
        "vrf definition .*! iface6start 0",
        "vrf definition .*! route4limit 0 0 0 0",
        "vrf definition .*! route6limit 0 0 0 0",
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
        "vrf definition .*! no dapp4",
        "vrf definition .*! no dapp6",
        "vrf definition .*! no copp4in",
        "vrf definition .*! no copp4out",
        "vrf definition .*! no copp6in",
        "vrf definition .*! no copp6out",
        "vrf definition .*! no packet4filter",
        "vrf definition .*! no packet6filter",
        "vrf definition .*! incremental4 1000",
        "vrf definition .*! incremental6 1000",
        "!ipv[46] nat .* sequence .* timeout 300000",
        "!ipv[46] nat .* sequence .* sessions 0"
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
     * allocate this vrf now
     */
    public synchronized void allocThisVrf() {
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
     * start this vrf now
     */
    public synchronized void startThisVrf() {
        ipx.startThisVrf();
        fwd4.startThisVrf();
        fwd6.startThisVrf();
    }

    /**
     * destroy this vrf
     */
    public synchronized void stopThisVrf() {
        ipx.stopThisVrf();
        fwd4.stopThisVrf();
        fwd6.stopThisVrf();
    }

    /**
     * close connections
     */
    public synchronized void closeAllConns() {
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

    private static ipFwd.labelMode string2labmod(String a) {
        if (a.equals("per-prefix")) {
            return ipFwd.labelMode.all;
        }
        if (a.equals("all-igp")) {
            return ipFwd.labelMode.igp;
        }
        if (a.equals("host-route")) {
            return ipFwd.labelMode.host;
        }
        if (a.equals("connected")) {
            return ipFwd.labelMode.conn;
        }
        if (a.equals("per-vrf")) {
            return ipFwd.labelMode.common;
        }
        return ipFwd.labelMode.common;
    }

    private static String labmod2string(ipFwd.labelMode lm) {
        switch (lm) {
            case all:
                return "per-prefix";
            case igp:
                return "all-igp";
            case host:
                return "host-route";
            case conn:
                return "connected";
            case common:
                return "per-vrf";
            default:
                return "unknown";
        }
    }

    public List<String> getShRun(int filter) {
        List<String> l = new ArrayList<String>();
        if (hidden) {
            return l;
        }
        l.add("vrf definition " + name);
        cmds.cfgLine(l, description.length() < 1, cmds.tabulator, "description", description);
        l.add(cmds.tabulator + "rd " + tabRouteUtil.rd2string(fwd4.rd));
        String s = "";
        for (int i = 0; i < fwd4.rtImp.size(); i++) {
            s += " " + tabRouteUtil.rd2string(fwd4.rtImp.get(i));
        }
        l.add(cmds.tabulator + "update4interval " + fwd4.updateInterval);
        l.add(cmds.tabulator + "update6interval " + fwd6.updateInterval);
        l.add(cmds.tabulator + "rt4import" + s);
        s = "";
        for (int i = 0; i < fwd4.rtExp.size(); i++) {
            s += " " + tabRouteUtil.rd2string(fwd4.rtExp.get(i));
        }
        l.add(cmds.tabulator + "rt4export" + s);
        s = "";
        for (int i = 0; i < fwd6.rtImp.size(); i++) {
            s += " " + tabRouteUtil.rd2string(fwd6.rtImp.get(i));
        }
        l.add(cmds.tabulator + "rt6import" + s);
        s = "";
        for (int i = 0; i < fwd6.rtExp.size(); i++) {
            s += " " + tabRouteUtil.rd2string(fwd6.rtExp.get(i));
        }
        l.add(cmds.tabulator + "rt6export" + s);
        l.add(cmds.tabulator + "iface4start " + iface4start);
        l.add(cmds.tabulator + "iface6start " + iface6start);
        l.add(cmds.tabulator + "label4mode " + labmod2string(fwd4.prefixMode));
        l.add(cmds.tabulator + "label6mode " + labmod2string(fwd6.prefixMode));
        l.add(cmds.tabulator + "label4common " + label4comm);
        l.add(cmds.tabulator + "label6common " + label6comm);
        l.add(cmds.tabulator + "route4limit " + fwd4.routeLimitU + " " + fwd4.routeLimitL + " " + fwd4.routeLimitM + " " + fwd4.routeLimitF);
        l.add(cmds.tabulator + "route6limit " + fwd6.routeLimitU + " " + fwd6.routeLimitL + " " + fwd6.routeLimitM + " " + fwd6.routeLimitF);
        cmds.cfgLine(l, !fwd4.mplsPropTtl, cmds.tabulator, "propagate4ttl", "");
        cmds.cfgLine(l, !fwd6.mplsPropTtl, cmds.tabulator, "propagate6ttl", "");
        cmds.cfgLine(l, !fwd4.mplsExtRep, cmds.tabulator, "report4labels", "");
        cmds.cfgLine(l, !fwd6.mplsExtRep, cmds.tabulator, "report6labels", "");
        l.add(cmds.tabulator + "unreach4interval " + fwd4.unreachInt);
        l.add(cmds.tabulator + "unreach6interval " + fwd6.unreachInt);
        cmds.cfgLine(l, !fwd4.ruinPmtuD, cmds.tabulator, "punish4pmtud", "");
        cmds.cfgLine(l, !fwd6.ruinPmtuD, cmds.tabulator, "punish6pmtud", "");
        cmds.cfgLine(l, fwd4.labelFilter == null, cmds.tabulator, "label4filter", "" + fwd4.labelFilter);
        cmds.cfgLine(l, fwd6.labelFilter == null, cmds.tabulator, "label6filter", "" + fwd6.labelFilter);
        cmds.cfgLine(l, fwd4.importList == null, cmds.tabulator, "import4list", "" + fwd4.importList);
        cmds.cfgLine(l, fwd6.importList == null, cmds.tabulator, "import6list", "" + fwd6.importList);
        cmds.cfgLine(l, fwd4.exportList == null, cmds.tabulator, "export4list", "" + fwd4.exportList);
        cmds.cfgLine(l, fwd6.exportList == null, cmds.tabulator, "export6list", "" + fwd6.exportList);
        cmds.cfgLine(l, fwd4.importMap == null, cmds.tabulator, "import4map", "" + fwd4.importMap);
        cmds.cfgLine(l, fwd6.importMap == null, cmds.tabulator, "import6map", "" + fwd6.importMap);
        cmds.cfgLine(l, fwd4.exportMap == null, cmds.tabulator, "export4map", "" + fwd4.exportMap);
        cmds.cfgLine(l, fwd6.exportMap == null, cmds.tabulator, "export6map", "" + fwd6.exportMap);
        cmds.cfgLine(l, fwd4.importPol == null, cmds.tabulator, "import4policy", "" + fwd4.importPol);
        cmds.cfgLine(l, fwd6.importPol == null, cmds.tabulator, "import6policy", "" + fwd6.importPol);
        cmds.cfgLine(l, fwd4.exportPol == null, cmds.tabulator, "export4policy", "" + fwd4.exportPol);
        cmds.cfgLine(l, fwd6.exportPol == null, cmds.tabulator, "export6policy", "" + fwd6.exportPol);
        cmds.cfgLine(l, fwd4.dapp == null, cmds.tabulator, "dapp4", "" + fwd4.dapp);
        cmds.cfgLine(l, fwd6.dapp == null, cmds.tabulator, "dapp6", "" + fwd6.dapp);
        cmds.cfgLine(l, fwd4.coppIn == null, cmds.tabulator, "copp4in", "" + fwd4.coppIn);
        cmds.cfgLine(l, fwd4.coppOut == null, cmds.tabulator, "copp4out", "" + fwd4.coppOut);
        cmds.cfgLine(l, fwd6.coppIn == null, cmds.tabulator, "copp6in", "" + fwd6.coppIn);
        cmds.cfgLine(l, fwd6.coppOut == null, cmds.tabulator, "copp6out", "" + fwd6.coppOut);
        cmds.cfgLine(l, fwd4.packetFilter == null, cmds.tabulator, "packet4filter", "" + fwd4.packetFilter);
        cmds.cfgLine(l, fwd6.packetFilter == null, cmds.tabulator, "packet6filter", "" + fwd6.packetFilter);
        cmds.cfgLine(l, !fwd4.mdt, cmds.tabulator, "mdt4", "");
        cmds.cfgLine(l, !fwd6.mdt, cmds.tabulator, "mdt6", "");
        l.add(cmds.tabulator + "incremental4 " + fwd4.incrLimit);
        l.add(cmds.tabulator + "incremental6 " + fwd6.incrLimit);
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
        l.add(null, "1 2  update4interval     specify time between table calculation");
        l.add(null, "2 .    <num>             time in ms");
        l.add(null, "1 2  update6interval     specify time between table calculation");
        l.add(null, "2 .    <num>             time in ms");
        l.add(null, "1 2  rd                  specify route distinguisher");
        l.add(null, "2 .    <rd>              rd in ASnum:IDnum format");
        l.add(null, "1 2  rt-both             specify route target");
        l.add(null, "2 2,.  <rt>              rt in ASnum:IDnum format");
        l.add(null, "1 2  rt-import           specify route target import");
        l.add(null, "2 2,.  <rt>              rt in ASnum:IDnum format");
        l.add(null, "1 2  rt-export           specify route target export");
        l.add(null, "2 2,.  <rt>              rt in ASnum:IDnum format");
        l.add(null, "1 2  rt4both             specify route target");
        l.add(null, "2 2,.  <rt>              rt in ASnum:IDnum format");
        l.add(null, "1 2  rt4import           specify route target import");
        l.add(null, "2 2,.  <rt>              rt in ASnum:IDnum format");
        l.add(null, "1 2  rt4export           specify route target export");
        l.add(null, "2 2,.  <rt>              rt in ASnum:IDnum format");
        l.add(null, "1 2  rt6both             specify route target");
        l.add(null, "2 2,.  <rt>              rt in ASnum:IDnum format");
        l.add(null, "1 2  rt6import           specify route target import");
        l.add(null, "2 2,.  <rt>              rt in ASnum:IDnum format");
        l.add(null, "1 2  rt6export           specify route target export");
        l.add(null, "2 2,.  <rt>              rt in ASnum:IDnum format");
        l.add(null, ".1 . punish-pmtud        send back mtu exceeded if needed");
        l.add(null, ".1 . punish4pmtud        send back mtu exceeded if needed");
        l.add(null, ".1 . punish6pmtud        send back mtu exceeded if needed");
        l.add(null, "1 2  unreach-interval    rate limit icmp generation");
        l.add(null, "2 .    <num>             millisecs between them");
        l.add(null, "1 2  unreach4interval    rate limit icmp generation");
        l.add(null, "2 .    <num>             millisecs between them");
        l.add(null, "1 2  unreach6interval    rate limit icmp generation");
        l.add(null, "2 .    <num>             millisecs between them");
        l.add(null, "1 2  route4limit         maximum ipv4 routes allowed");
        l.add(null, "2 3    <num>             number of unicast routes");
        l.add(null, "3 4      <num>           number of labeled routes");
        l.add(null, "4 5        <num>         number of multicast routes");
        l.add(null, "5 .          <num>       number of flowspec routes");
        l.add(null, "1 2  route6limit         maximum ipv6 routes allowed");
        l.add(null, "2 3    <num>             number of unicast routes");
        l.add(null, "3 4      <num>           number of labeled routes");
        l.add(null, "4 5        <num>         number of multicast routes");
        l.add(null, "5 .          <num>       number of flowspec routes");
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
        l.add(null, "1 .  propagate-ttl       specify to copy ip ttl to mpls ttl");
        l.add(null, "1 .  propagate4ttl       specify to copy ip ttl to mpls ttl");
        l.add(null, "1 .  propagate6ttl       specify to copy ip ttl to mpls ttl");
        l.add(null, "1 .  report-labels       append icmp extension with labels");
        l.add(null, "1 .  report4labels       append icmp extension with labels");
        l.add(null, "1 .  report6labels       append icmp extension with labels");
        l.add(null, "1 .  mdt4                enable multicast distribution tree for ipv4");
        l.add(null, "1 .  mdt6                enable multicast distribution tree for ipv6");
        l.add(null, "1 2  label-mode          specify label allocation mode");
        l.add(null, "1 2  label4mode          specify label allocation mode");
        l.add(null, "1 2  label6mode          specify label allocation mode");
        l.add(null, "2 .    per-prefix        label for all prefixes");
        l.add(null, "2 .    all-igp           label for all igp prefixes");
        l.add(null, "2 .    host-route        label for host routes");
        l.add(null, "2 .    connected         label for connected routes");
        l.add(null, "2 .    per-vrf           common label for the vrf");
        l.add(null, "1 2  incremental4        specify ipv4 incremental limit");
        l.add(null, "2 .    <num>             routes");
        l.add(null, "1 2  incremental6        specify ipv6 incremental limit");
        l.add(null, "2 .    <num>             routes");
    }

    private List<Long> string2rts(cmds cmd) {
        List<Long> res = new ArrayList<Long>();
        for (;;) {
            String a = cmd.word();
            if (a.length() < 1) {
                break;
            }
            res.add(tabRouteUtil.string2rd(a));
        }
        return res;
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
            long res = tabRouteUtil.string2rd(cmd.word());
            fwd4.rd = res;
            fwd6.rd = res;
            return;
        }
        if (a.equals("rt-import")) {
            List<Long> res = string2rts(cmd);
            fwd4.rtImp = res;
            fwd6.rtImp = res;
            fwd4.routerStaticChg();
            fwd6.routerStaticChg();
            return;
        }
        if (a.equals("rt-export")) {
            List<Long> res = string2rts(cmd);
            fwd4.rtExp = res;
            fwd6.rtExp = res;
            fwd4.routerStaticChg();
            fwd6.routerStaticChg();
            return;
        }
        if (a.equals("rt-both")) {
            List<Long> res = string2rts(cmd);
            fwd4.rtImp = res;
            fwd6.rtImp = res;
            fwd4.rtExp = res;
            fwd6.rtExp = res;
            fwd4.routerStaticChg();
            fwd6.routerStaticChg();
            return;
        }
        if (a.equals("update4interval")) {
            fwd4.updateInterval = bits.str2num(cmd.word());
            return;
        }
        if (a.equals("update6interval")) {
            fwd6.updateInterval = bits.str2num(cmd.word());
            return;
        }
        if (a.equals("rt4import")) {
            fwd4.rtImp = string2rts(cmd);
            fwd4.routerStaticChg();
            return;
        }
        if (a.equals("rt4export")) {
            fwd4.rtExp = string2rts(cmd);
            fwd4.routerStaticChg();
            return;
        }
        if (a.equals("rt4both")) {
            List<Long> res = string2rts(cmd);
            fwd4.rtImp = res;
            fwd4.rtExp = res;
            fwd4.routerStaticChg();
            return;
        }
        if (a.equals("rt6import")) {
            fwd6.rtImp = string2rts(cmd);
            fwd6.routerStaticChg();
            return;
        }
        if (a.equals("rt6export")) {
            fwd6.rtExp = string2rts(cmd);
            fwd6.routerStaticChg();
            return;
        }
        if (a.equals("rt6both")) {
            List<Long> res = string2rts(cmd);
            fwd6.rtImp = res;
            fwd6.rtExp = res;
            fwd6.routerStaticChg();
            return;
        }
        if (a.equals("route4limit")) {
            fwd4.routeLimitU = bits.str2num(cmd.word());
            fwd4.routeLimitL = bits.str2num(cmd.word());
            fwd4.routeLimitM = bits.str2num(cmd.word());
            fwd4.routeLimitF = bits.str2num(cmd.word());
            fwd4.routerStaticChg();
            return;
        }
        if (a.equals("route6limit")) {
            fwd6.routeLimitU = bits.str2num(cmd.word());
            fwd6.routeLimitL = bits.str2num(cmd.word());
            fwd6.routeLimitM = bits.str2num(cmd.word());
            fwd6.routeLimitF = bits.str2num(cmd.word());
            fwd6.routerStaticChg();
            return;
        }
        if (a.equals("mdt4")) {
            fwd4.mdt = true;
            fwd4.routerStaticChg();
            return;
        }
        if (a.equals("mdt6")) {
            fwd6.mdt = true;
            fwd6.routerStaticChg();
            return;
        }
        if (a.equals("label-mode")) {
            a = cmd.word();
            fwd4.prefixMode = string2labmod(a);
            fwd6.prefixMode = string2labmod(a);
            fwd4.routerStaticChg();
            fwd6.routerStaticChg();
            return;
        }
        if (a.equals("label4mode")) {
            a = cmd.word();
            fwd4.prefixMode = string2labmod(a);
            fwd4.routerStaticChg();
            return;
        }
        if (a.equals("label6mode")) {
            a = cmd.word();
            fwd6.prefixMode = string2labmod(a);
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
            int res = bits.str2num(cmd.word());
            fwd4.unreachInt = res;
            fwd6.unreachInt = res;
            return;
        }
        if (a.equals("punish-pmtud")) {
            fwd4.ruinPmtuD = true;
            fwd6.ruinPmtuD = true;
            return;
        }
        if (a.equals("report-labels")) {
            fwd4.mplsExtRep = true;
            fwd6.mplsExtRep = true;
            return;
        }
        if (a.equals("propagate-ttl")) {
            fwd4.mplsPropTtl = true;
            fwd6.mplsPropTtl = true;
            return;
        }
        if (a.equals("unreach4interval")) {
            int res = bits.str2num(cmd.word());
            fwd4.unreachInt = res;
            return;
        }
        if (a.equals("punish4pmtud")) {
            fwd4.ruinPmtuD = true;
            return;
        }
        if (a.equals("report4labels")) {
            fwd4.mplsExtRep = true;
            return;
        }
        if (a.equals("propagate4ttl")) {
            fwd4.mplsPropTtl = true;
            return;
        }
        if (a.equals("unreach6interval")) {
            int res = bits.str2num(cmd.word());
            fwd6.unreachInt = res;
            return;
        }
        if (a.equals("punish6pmtud")) {
            fwd6.ruinPmtuD = true;
            return;
        }
        if (a.equals("report6labels")) {
            fwd6.mplsExtRep = true;
            return;
        }
        if (a.equals("propagate6ttl")) {
            fwd6.mplsPropTtl = true;
            return;
        }
        if (a.equals("label4filter")) {
            cfgPrfxlst pfx = cfgAll.prfxFind(cmd.word(), false);
            if (pfx == null) {
                cmd.error("no such prefix list");
                return;
            }
            fwd4.labelFilter = pfx.prflst;
            fwd4.routerStaticChg();
            return;
        }
        if (a.equals("label6filter")) {
            cfgPrfxlst pfx = cfgAll.prfxFind(cmd.word(), false);
            if (pfx == null) {
                cmd.error("no such prefix list");
                return;
            }
            fwd6.labelFilter = pfx.prflst;
            fwd6.routerStaticChg();
            return;
        }
        if (a.equals("import4list")) {
            cfgPrfxlst pfx = cfgAll.prfxFind(cmd.word(), false);
            if (pfx == null) {
                cmd.error("no such prefix list");
                return;
            }
            fwd4.importList = pfx.prflst;
            fwd4.routerStaticChg();
            return;
        }
        if (a.equals("import6list")) {
            cfgPrfxlst pfx = cfgAll.prfxFind(cmd.word(), false);
            if (pfx == null) {
                cmd.error("no such prefix list");
                return;
            }
            fwd6.importList = pfx.prflst;
            fwd6.routerStaticChg();
            return;
        }
        if (a.equals("export4list")) {
            cfgPrfxlst pfx = cfgAll.prfxFind(cmd.word(), false);
            if (pfx == null) {
                cmd.error("no such prefix list");
                return;
            }
            fwd4.exportList = pfx.prflst;
            fwd4.routerStaticChg();
            return;
        }
        if (a.equals("export6list")) {
            cfgPrfxlst pfx = cfgAll.prfxFind(cmd.word(), false);
            if (pfx == null) {
                cmd.error("no such prefix list");
                return;
            }
            fwd6.exportList = pfx.prflst;
            fwd6.routerStaticChg();
            return;
        }
        if (a.equals("import4map")) {
            cfgRoump rm = cfgAll.rtmpFind(cmd.word(), false);
            if (rm == null) {
                cmd.error("no such route map");
                return;
            }
            fwd4.importMap = rm.roumap;
            fwd4.routerStaticChg();
            return;
        }
        if (a.equals("import6map")) {
            cfgRoump rm = cfgAll.rtmpFind(cmd.word(), false);
            if (rm == null) {
                cmd.error("no such route map");
                return;
            }
            fwd6.importMap = rm.roumap;
            fwd6.routerStaticChg();
            return;
        }
        if (a.equals("export4map")) {
            cfgRoump rm = cfgAll.rtmpFind(cmd.word(), false);
            if (rm == null) {
                cmd.error("no such route map");
                return;
            }
            fwd4.exportMap = rm.roumap;
            fwd4.routerStaticChg();
            return;
        }
        if (a.equals("export6map")) {
            cfgRoump rm = cfgAll.rtmpFind(cmd.word(), false);
            if (rm == null) {
                cmd.error("no such route map");
                return;
            }
            fwd6.exportMap = rm.roumap;
            fwd6.routerStaticChg();
            return;
        }
        if (a.equals("import4policy")) {
            cfgRouplc rpl = cfgAll.rtplFind(cmd.word(), false);
            if (rpl == null) {
                cmd.error("no such route policy");
                return;
            }
            fwd4.importPol = rpl.rouplc;
            fwd4.routerStaticChg();
            return;
        }
        if (a.equals("import6policy")) {
            cfgRouplc rpl = cfgAll.rtplFind(cmd.word(), false);
            if (rpl == null) {
                cmd.error("no such route policy");
                return;
            }
            fwd6.importPol = rpl.rouplc;
            fwd6.routerStaticChg();
            return;
        }
        if (a.equals("export4policy")) {
            cfgRouplc rpl = cfgAll.rtplFind(cmd.word(), false);
            if (rpl == null) {
                cmd.error("no such route policy");
                return;
            }
            fwd4.exportPol = rpl.rouplc;
            fwd4.routerStaticChg();
            return;
        }
        if (a.equals("export6policy")) {
            cfgRouplc rpl = cfgAll.rtplFind(cmd.word(), false);
            if (rpl == null) {
                cmd.error("no such route policy");
                return;
            }
            fwd6.exportPol = rpl.rouplc;
            fwd6.routerStaticChg();
            return;
        }
        if (a.equals("dapp4")) {
            cfgPlymp pm = cfgAll.plmpFind(cmd.word(), false);
            if (pm == null) {
                cmd.error("no such policy map");
                return;
            }
            tabQos wrkr = tabQos.convertPolicy(pm.plcmap);
            if (wrkr == null) {
                cmd.error("error applying policy map");
                return;
            }
            fwd4.dapp = wrkr;
            return;
        }
        if (a.equals("dapp6")) {
            cfgPlymp pm = cfgAll.plmpFind(cmd.word(), false);
            if (pm == null) {
                cmd.error("no such policy map");
                return;
            }
            tabQos wrkr = tabQos.convertPolicy(pm.plcmap);
            if (wrkr == null) {
                cmd.error("error applying policy map");
                return;
            }
            fwd6.dapp = wrkr;
            return;
        }
        if (a.equals("copp4in")) {
            cfgPlymp pm = cfgAll.plmpFind(cmd.word(), false);
            if (pm == null) {
                cmd.error("no such policy map");
                return;
            }
            tabQos wrkr = tabQos.convertPolicy(pm.plcmap);
            if (wrkr == null) {
                cmd.error("error applying policy map");
                return;
            }
            fwd4.coppIn = wrkr;
            return;
        }
        if (a.equals("copp4out")) {
            cfgPlymp pm = cfgAll.plmpFind(cmd.word(), false);
            if (pm == null) {
                cmd.error("no such policy map");
                return;
            }
            tabQos wrkr = tabQos.convertPolicy(pm.plcmap);
            if (wrkr == null) {
                cmd.error("error applying policy map");
                return;
            }
            fwd4.coppOut = wrkr;
            return;
        }
        if (a.equals("copp6in")) {
            cfgPlymp pm = cfgAll.plmpFind(cmd.word(), false);
            if (pm == null) {
                cmd.error("no such policy map");
                return;
            }
            tabQos wrkr = tabQos.convertPolicy(pm.plcmap);
            if (wrkr == null) {
                cmd.error("error applying policy map");
                return;
            }
            fwd6.coppIn = wrkr;
            return;
        }
        if (a.equals("copp6out")) {
            cfgPlymp pm = cfgAll.plmpFind(cmd.word(), false);
            if (pm == null) {
                cmd.error("no such policy map");
                return;
            }
            tabQos wrkr = tabQos.convertPolicy(pm.plcmap);
            if (wrkr == null) {
                cmd.error("error applying policy map");
                return;
            }
            fwd6.coppOut = wrkr;
            return;
        }
        if (a.equals("packet4filter")) {
            cfgAceslst acl = cfgAll.aclsFind(cmd.word(), false);
            if (acl == null) {
                cmd.error("no such access list");
                return;
            }
            acl.aceslst.myCor = core4;
            acl.aceslst.myIcmp = icmp4;
            fwd4.packetFilter = acl.aceslst;
            return;
        }
        if (a.equals("packet6filter")) {
            cfgAceslst acl = cfgAll.aclsFind(cmd.word(), false);
            if (acl == null) {
                cmd.error("no such access list");
                return;
            }
            acl.aceslst.myCor = core6;
            acl.aceslst.myIcmp = icmp6;
            fwd6.packetFilter = acl.aceslst;
            return;
        }
        if (a.equals("incremental4")) {
            fwd4.incrLimit = bits.str2num(cmd.word());
            return;
        }
        if (a.equals("route6limit")) {
            fwd6.incrLimit = bits.str2num(cmd.word());
            return;
        }
        if (!a.equals("no")) {
            cmd.badCmd();
            return;
        }
        a = cmd.word();
        if (a.equals("rd")) {
            fwd4.rd = 0;
            fwd6.rd = 0;
            return;
        }
        if (a.equals("rt-import")) {
            fwd4.rtImp = new ArrayList<Long>();
            fwd6.rtImp = new ArrayList<Long>();
            fwd4.routerStaticChg();
            fwd6.routerStaticChg();
            return;
        }
        if (a.equals("rt-export")) {
            fwd4.rtExp = new ArrayList<Long>();
            fwd6.rtExp = new ArrayList<Long>();
            fwd4.routerStaticChg();
            fwd6.routerStaticChg();
            return;
        }
        if (a.equals("rt-both")) {
            fwd4.rtImp = new ArrayList<Long>();
            fwd6.rtImp = new ArrayList<Long>();
            fwd4.rtExp = new ArrayList<Long>();
            fwd6.rtExp = new ArrayList<Long>();
            fwd4.routerStaticChg();
            fwd6.routerStaticChg();
            return;
        }
        if (a.equals("update4interval")) {
            fwd4.updateInterval = 0;
            return;
        }
        if (a.equals("update6interval")) {
            fwd6.updateInterval = 0;
            return;
        }
        if (a.equals("rt4import")) {
            fwd4.rtImp = new ArrayList<Long>();
            fwd4.routerStaticChg();
            return;
        }
        if (a.equals("rt4export")) {
            fwd4.rtExp = new ArrayList<Long>();
            fwd4.routerStaticChg();
            return;
        }
        if (a.equals("rt4both")) {
            fwd4.rtImp = new ArrayList<Long>();
            fwd4.rtExp = new ArrayList<Long>();
            fwd4.routerStaticChg();
            return;
        }
        if (a.equals("rt6import")) {
            fwd6.rtImp = new ArrayList<Long>();
            fwd6.routerStaticChg();
            return;
        }
        if (a.equals("rt6export")) {
            fwd6.rtExp = new ArrayList<Long>();
            fwd6.routerStaticChg();
            return;
        }
        if (a.equals("rt6both")) {
            fwd6.rtImp = new ArrayList<Long>();
            fwd6.rtExp = new ArrayList<Long>();
            fwd6.routerStaticChg();
            return;
        }
        if (a.equals("route4limit")) {
            fwd4.routeLimitU = 0;
            fwd4.routeLimitL = 0;
            fwd4.routeLimitM = 0;
            fwd4.routeLimitF = 0;
            fwd4.routerStaticChg();
            return;
        }
        if (a.equals("route6limit")) {
            fwd6.routeLimitU = 0;
            fwd6.routeLimitL = 0;
            fwd6.routeLimitM = 0;
            fwd6.routeLimitF = 0;
            fwd6.routerStaticChg();
            return;
        }
        if (a.equals("description")) {
            description = "";
            return;
        }
        if (a.equals("mdt4")) {
            fwd4.mdt = false;
            fwd4.routerStaticChg();
            return;
        }
        if (a.equals("mdt6")) {
            fwd6.mdt = false;
            fwd6.routerStaticChg();
            return;
        }
        if (a.equals("label-mode")) {
            fwd4.prefixMode = ipFwd.labelMode.common;
            fwd6.prefixMode = ipFwd.labelMode.common;
            fwd4.routerStaticChg();
            fwd6.routerStaticChg();
            return;
        }
        if (a.equals("label4mode")) {
            fwd4.prefixMode = ipFwd.labelMode.common;
            fwd4.routerStaticChg();
            return;
        }
        if (a.equals("label6mode")) {
            fwd6.prefixMode = ipFwd.labelMode.common;
            fwd6.routerStaticChg();
            return;
        }
        if (a.equals("unreach-interval")) {
            fwd4.unreachInt = 0;
            fwd6.unreachInt = 0;
            return;
        }
        if (a.equals("punish-pmtud")) {
            fwd4.ruinPmtuD = false;
            fwd6.ruinPmtuD = false;
            return;
        }
        if (a.equals("report-labels")) {
            fwd4.mplsExtRep = false;
            fwd6.mplsExtRep = false;
            return;
        }
        if (a.equals("propagate-ttl")) {
            fwd4.mplsPropTtl = false;
            fwd6.mplsPropTtl = false;
            return;
        }
        if (a.equals("unreach4interval")) {
            fwd4.unreachInt = 0;
            return;
        }
        if (a.equals("punish4pmtud")) {
            fwd4.ruinPmtuD = false;
            return;
        }
        if (a.equals("report4labels")) {
            fwd4.mplsExtRep = false;
            return;
        }
        if (a.equals("propagate4ttl")) {
            fwd4.mplsPropTtl = false;
            return;
        }
        if (a.equals("unreach6interval")) {
            fwd6.unreachInt = 0;
            return;
        }
        if (a.equals("punish6pmtud")) {
            fwd6.ruinPmtuD = false;
            return;
        }
        if (a.equals("report6labels")) {
            fwd6.mplsExtRep = false;
            return;
        }
        if (a.equals("propagate6ttl")) {
            fwd6.mplsPropTtl = false;
            return;
        }
        if (a.equals("label4filter")) {
            fwd4.labelFilter = null;
            fwd4.routerStaticChg();
            return;
        }
        if (a.equals("label6filter")) {
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
            fwd4.importList = null;
            fwd4.routerStaticChg();
            return;
        }
        if (a.equals("import6list")) {
            fwd6.importList = null;
            fwd6.routerStaticChg();
            return;
        }
        if (a.equals("export4list")) {
            fwd4.exportList = null;
            fwd4.routerStaticChg();
            return;
        }
        if (a.equals("export6list")) {
            fwd6.exportList = null;
            fwd6.routerStaticChg();
            return;
        }
        if (a.equals("import4map")) {
            fwd4.importMap = null;
            fwd4.routerStaticChg();
            return;
        }
        if (a.equals("import6map")) {
            fwd6.importMap = null;
            fwd6.routerStaticChg();
            return;
        }
        if (a.equals("export4map")) {
            fwd4.exportMap = null;
            fwd4.routerStaticChg();
            return;
        }
        if (a.equals("export6map")) {
            fwd6.exportMap = null;
            fwd6.routerStaticChg();
            return;
        }
        if (a.equals("import4policy")) {
            fwd4.importPol = null;
            fwd4.routerStaticChg();
            return;
        }
        if (a.equals("import6policy")) {
            fwd6.importPol = null;
            fwd6.routerStaticChg();
            return;
        }
        if (a.equals("export4policy")) {
            fwd4.exportPol = null;
            fwd4.routerStaticChg();
            return;
        }
        if (a.equals("export6policy")) {
            fwd6.exportPol = null;
            fwd6.routerStaticChg();
            return;
        }
        if (a.equals("dapp4")) {
            fwd4.dapp = null;
            return;
        }
        if (a.equals("dapp6")) {
            fwd6.dapp = null;
            return;
        }
        if (a.equals("copp4in")) {
            fwd4.coppIn = null;
            return;
        }
        if (a.equals("copp4out")) {
            fwd4.coppOut = null;
            return;
        }
        if (a.equals("copp6in")) {
            fwd6.coppIn = null;
            return;
        }
        if (a.equals("copp6out")) {
            fwd6.coppOut = null;
            return;
        }
        if (a.equals("packet4filter")) {
            fwd4.packetFilter = null;
            return;
        }
        if (a.equals("packet6filter")) {
            fwd6.packetFilter = null;
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
