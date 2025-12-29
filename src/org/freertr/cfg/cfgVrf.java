package org.freertr.cfg;

import java.util.ArrayList;
import java.util.List;
import org.freertr.addr.addrIP;
import org.freertr.ip.ipCor4;
import org.freertr.ip.ipCor6;
import org.freertr.ip.ipFwd;
import org.freertr.ip.ipFwdIface;
import org.freertr.ip.ipFwdMcast;
import org.freertr.ip.ipFwdRoute;
import org.freertr.ip.ipFwdTab;
import org.freertr.ip.ipIcmp4;
import org.freertr.ip.ipIcmp6;
import org.freertr.ip.ipMhost4;
import org.freertr.ip.ipMhost6;
import org.freertr.ip.ipxFwd;
import org.freertr.prt.prtDccp;
import org.freertr.prt.prtLudp;
import org.freertr.prt.prtSctp;
import org.freertr.prt.prtTcp;
import org.freertr.prt.prtUdp;
import org.freertr.tab.tabLabel;
import org.freertr.tab.tabLabelEntry;
import org.freertr.tab.tabNatCfgN;
import org.freertr.tab.tabPbrN;
import org.freertr.tab.tabQos;
import org.freertr.tab.tabRateLimit;
import org.freertr.tab.tabRouteUtil;
import org.freertr.user.userFilter;
import org.freertr.user.userFormat;
import org.freertr.user.userHelp;
import org.freertr.util.bits;
import org.freertr.util.cmds;

/**
 * one vrf configuration
 *
 * @author matecsaba
 */
public class cfgVrf implements Comparable<cfgVrf>, cfgGeneric {

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
    public final static userFilter[] defaultF = {
        new userFilter("vrf definition .*", cmds.tabulator + cmds.negated + cmds.tabulator + "description", null),
        new userFilter("vrf definition .*", cmds.tabulator + "rd 0:0", null),
        new userFilter("vrf definition .*", cmds.tabulator + "update4interval 0", null),
        new userFilter("vrf definition .*", cmds.tabulator + "update6interval 0", null),
        new userFilter("vrf definition .*", cmds.tabulator + cmds.negated + cmds.tabulator + "optimize4lookup", null),
        new userFilter("vrf definition .*", cmds.tabulator + cmds.negated + cmds.tabulator + "optimize6lookup", null),
        new userFilter("vrf definition .*", cmds.tabulator + "rt4import", null),
        new userFilter("vrf definition .*", cmds.tabulator + "rt4export", null),
        new userFilter("vrf definition .*", cmds.tabulator + "rt6import", null),
        new userFilter("vrf definition .*", cmds.tabulator + "rt6export", null),
        new userFilter("vrf definition .*", cmds.tabulator + "clr4import", null),
        new userFilter("vrf definition .*", cmds.tabulator + "clr4export", null),
        new userFilter("vrf definition .*", cmds.tabulator + "clr6import", null),
        new userFilter("vrf definition .*", cmds.tabulator + "clr6export", null),
        new userFilter("vrf definition .*", cmds.tabulator + "label4mode per-vrf", null),
        new userFilter("vrf definition .*", cmds.tabulator + "label6mode per-vrf", null),
        new userFilter("vrf definition .*", cmds.tabulator + "mdt4 none", null),
        new userFilter("vrf definition .*", cmds.tabulator + "mdt6 none", null),
        new userFilter("vrf definition .*", cmds.tabulator + "propagate4ttl", null),
        new userFilter("vrf definition .*", cmds.tabulator + "propagate6ttl", null),
        new userFilter("vrf definition .*", cmds.tabulator + "report4labels", null),
        new userFilter("vrf definition .*", cmds.tabulator + "report6labels", null),
        new userFilter("vrf definition .*", cmds.tabulator + cmds.negated + cmds.tabulator + "unreach4rate", null),
        new userFilter("vrf definition .*", cmds.tabulator + cmds.negated + cmds.tabulator + "unreach6rate", null),
        new userFilter("vrf definition .*", cmds.tabulator + cmds.negated + cmds.tabulator + "label4filter", null),
        new userFilter("vrf definition .*", cmds.tabulator + cmds.negated + cmds.tabulator + "label6filter", null),
        new userFilter("vrf definition .*", cmds.tabulator + "label4common 0", null),
        new userFilter("vrf definition .*", cmds.tabulator + "label6common 0", null),
        new userFilter("vrf definition .*", cmds.tabulator + "iface4start 0", null),
        new userFilter("vrf definition .*", cmds.tabulator + "iface6start 0", null),
        new userFilter("vrf definition .*", cmds.tabulator + "route4limit 0 0 0 0", null),
        new userFilter("vrf definition .*", cmds.tabulator + "route6limit 0 0 0 0", null),
        new userFilter("vrf definition .*", cmds.tabulator + cmds.negated + cmds.tabulator + "import4list", null),
        new userFilter("vrf definition .*", cmds.tabulator + cmds.negated + cmds.tabulator + "import6list", null),
        new userFilter("vrf definition .*", cmds.tabulator + cmds.negated + cmds.tabulator + "export4list", null),
        new userFilter("vrf definition .*", cmds.tabulator + cmds.negated + cmds.tabulator + "export6list", null),
        new userFilter("vrf definition .*", cmds.tabulator + cmds.negated + cmds.tabulator + "import4map", null),
        new userFilter("vrf definition .*", cmds.tabulator + cmds.negated + cmds.tabulator + "import6map", null),
        new userFilter("vrf definition .*", cmds.tabulator + cmds.negated + cmds.tabulator + "export4map", null),
        new userFilter("vrf definition .*", cmds.tabulator + cmds.negated + cmds.tabulator + "export6map", null),
        new userFilter("vrf definition .*", cmds.tabulator + cmds.negated + cmds.tabulator + "import4policy", null),
        new userFilter("vrf definition .*", cmds.tabulator + cmds.negated + cmds.tabulator + "import6policy", null),
        new userFilter("vrf definition .*", cmds.tabulator + cmds.negated + cmds.tabulator + "export4policy", null),
        new userFilter("vrf definition .*", cmds.tabulator + cmds.negated + cmds.tabulator + "export6policy", null),
        new userFilter("vrf definition .*", cmds.tabulator + cmds.negated + cmds.tabulator + "dapp4", null),
        new userFilter("vrf definition .*", cmds.tabulator + cmds.negated + cmds.tabulator + "dapp6", null),
        new userFilter("vrf definition .*", cmds.tabulator + cmds.negated + cmds.tabulator + "copp4in", null),
        new userFilter("vrf definition .*", cmds.tabulator + cmds.negated + cmds.tabulator + "copp4out", null),
        new userFilter("vrf definition .*", cmds.tabulator + cmds.negated + cmds.tabulator + "copp6in", null),
        new userFilter("vrf definition .*", cmds.tabulator + cmds.negated + cmds.tabulator + "copp6out", null),
        new userFilter("vrf definition .*", cmds.tabulator + cmds.negated + cmds.tabulator + "packet4filter", null),
        new userFilter("vrf definition .*", cmds.tabulator + cmds.negated + cmds.tabulator + "packet6filter", null),
        new userFilter("vrf definition .*", cmds.tabulator + "incremental4 1000", null),
        new userFilter("vrf definition .*", cmds.tabulator + "incremental6 1000", null),
        new userFilter("vrf definition .*", cmds.tabulator + "threshold4traffic 0", null),
        new userFilter("vrf definition .*", cmds.tabulator + "threshold6traffic 0", null),
        new userFilter("vrf definition .*", cmds.tabulator + "threshold4route 0", null),
        new userFilter("vrf definition .*", cmds.tabulator + "threshold6route 0", null),
        new userFilter("", "ipv[46] nat .* sequence .* timeout 300000", null),
        new userFilter("", "ipv[46] nat .* sequence .* sessions 0", null),
        new userFilter("", "ipv[46] flow .* parameters ", null)
    };

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

    public int compareTo(cfgVrf o) {
        return name.toLowerCase().compareTo(o.name.toLowerCase());
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
        fwd4.other = fwd6;
        fwd6.other = fwd4;
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
     *
     * @param honor exempt restart candidates
     */
    public synchronized void closeAllConns(boolean honor) {
        udp4.closeConns(honor);
        udp6.closeConns(honor);
        ludp4.closeConns(honor);
        ludp6.closeConns(honor);
        dccp4.closeConns(honor);
        dccp6.closeConns(honor);
        sctp4.closeConns(honor);
        sctp6.closeConns(honor);
        tcp4.closeConns(honor);
        tcp6.closeConns(honor);
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

    private void addCfgNats(List<String> l, int p, ipFwd f, int filter) {
        for (int i = 0; i < f.natCfg.size(); i++) {
            tabNatCfgN nat = f.natCfg.get(i);
            l.addAll(nat.usrString("ipv" + p + " nat " + name + " ", filter));
        }
    }

    private void addCfgPbrs(List<String> l, int p, ipFwd f, int filter) {
        for (int i = 0; i < f.pbrCfg.size(); i++) {
            tabPbrN pbr = f.pbrCfg.get(i);
            l.addAll(pbr.usrString("ipv" + p + " pbr " + name + " ", filter));
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
        l.add("ipv" + p + " flow " + name + " parameters " + f.netflow.session);
    }

    private static ipFwd.mdtMode string2mdtmod(String a) {
        if (a.equals("none")) {
            return ipFwd.mdtMode.none;
        }
        if (a.equals("mldp")) {
            return ipFwd.mdtMode.mldp;
        }
        if (a.equals("bier")) {
            return ipFwd.mdtMode.bier;
        }
        return ipFwd.mdtMode.none;
    }

    private static String mdtmod2string(ipFwd.mdtMode mm) {
        switch (mm) {
            case none:
                return "none";
            case mldp:
                return "mldp";
            case bier:
                return "bier";
            default:
                return "unknown";
        }
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
        cmds.cfgLine(l, !fwd4.optimize, cmds.tabulator, "optimize4lookup", "");
        cmds.cfgLine(l, !fwd6.optimize, cmds.tabulator, "optimize6lookup", "");
        l.add(cmds.tabulator + "update4interval " + fwd4.updateInterval);
        l.add(cmds.tabulator + "update6interval " + fwd6.updateInterval);
        String s = "";
        for (int i = 0; i < fwd4.rtImp.size(); i++) {
            s += " " + tabRouteUtil.rd2string(fwd4.rtImp.get(i));
        }
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
        s = "";
        for (int i = 0; i < fwd4.clrImp.size(); i++) {
            s += " " + fwd4.clrImp.get(i);
        }
        l.add(cmds.tabulator + "clr4import" + s);
        s = "";
        for (int i = 0; i < fwd4.clrExp.size(); i++) {
            s += " " + fwd4.clrExp.get(i);
        }
        l.add(cmds.tabulator + "clr4export" + s);
        s = "";
        for (int i = 0; i < fwd6.clrImp.size(); i++) {
            s += " " + fwd6.clrImp.get(i);
        }
        l.add(cmds.tabulator + "clr6import" + s);
        s = "";
        for (int i = 0; i < fwd6.clrExp.size(); i++) {
            s += " " + fwd6.clrExp.get(i);
        }
        l.add(cmds.tabulator + "clr6export" + s);
        l.add(cmds.tabulator + "iface4start " + iface4start);
        l.add(cmds.tabulator + "iface6start " + iface6start);
        l.add(cmds.tabulator + "label4mode " + labmod2string(fwd4.prefixMode));
        l.add(cmds.tabulator + "label6mode " + labmod2string(fwd6.prefixMode));
        l.add(cmds.tabulator + "mdt4 " + mdtmod2string(fwd4.mdtMod));
        l.add(cmds.tabulator + "mdt6 " + mdtmod2string(fwd6.mdtMod));
        l.add(cmds.tabulator + "label4common " + label4comm);
        l.add(cmds.tabulator + "label6common " + label6comm);
        l.add(cmds.tabulator + "route4limit " + fwd4.routeLimitU + " " + fwd4.routeLimitL + " " + fwd4.routeLimitM + " " + fwd4.routeLimitF);
        l.add(cmds.tabulator + "route6limit " + fwd6.routeLimitU + " " + fwd6.routeLimitL + " " + fwd6.routeLimitM + " " + fwd6.routeLimitF);
        cmds.cfgLine(l, !fwd4.mplsPropTtl, cmds.tabulator, "propagate4ttl", "");
        cmds.cfgLine(l, !fwd6.mplsPropTtl, cmds.tabulator, "propagate6ttl", "");
        cmds.cfgLine(l, !fwd4.mplsExtRep, cmds.tabulator, "report4labels", "");
        cmds.cfgLine(l, !fwd6.mplsExtRep, cmds.tabulator, "report6labels", "");
        cmds.cfgLine(l, fwd4.unreach == null, cmds.tabulator, "unreach4rate", "" + fwd4.unreach);
        cmds.cfgLine(l, fwd6.unreach == null, cmds.tabulator, "unreach6rate", "" + fwd6.unreach);
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
        l.add(cmds.tabulator + "incremental4 " + fwd4.incrLimit);
        l.add(cmds.tabulator + "incremental6 " + fwd6.incrLimit);
        l.add(cmds.tabulator + "threshold4traffic " + fwd4.thresholdT);
        l.add(cmds.tabulator + "threshold6traffic " + fwd6.thresholdT);
        l.add(cmds.tabulator + "threshold4route " + fwd4.thresholdR);
        l.add(cmds.tabulator + "threshold6route " + fwd6.thresholdR);
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
        addCfgNats(l, 4, fwd4, filter);
        l.add(cmds.comment);
        addCfgNats(l, 6, fwd6, filter);
        l.add(cmds.comment);
        addCfgPbrs(l, 4, fwd4, filter);
        l.add(cmds.comment);
        addCfgPbrs(l, 6, fwd6, filter);
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

    public void getHelp(userHelp l) {
        l.add(null, false, 1, new int[]{2, -1}, "description", "description of this vrf");
        l.add(null, false, 2, new int[]{2, -1}, "[text]", "text describing this vrf");
        l.add(null, false, 1, new int[]{2}, "rename", "rename this vrf");
        l.add(null, false, 2, new int[]{-1}, "<str>", "set new name of vrf");
        l.add(null, false, 1, new int[]{-1}, "optimize-lookup", "optimize rib for software lookup");
        l.add(null, false, 1, new int[]{-1}, "optimize4lookup", "optimize rib for software lookup");
        l.add(null, false, 1, new int[]{-1}, "optimize6lookup", "optimize rib for software lookup");
        l.add(null, false, 1, new int[]{2}, "update-interval", "specify time between table calculation");
        l.add(null, false, 2, new int[]{-1}, "<num>", "time in ms");
        l.add(null, false, 1, new int[]{2}, "update4interval", "specify time between table calculation");
        l.add(null, false, 2, new int[]{-1}, "<num>", "time in ms");
        l.add(null, false, 1, new int[]{2}, "update6interval", "specify time between table calculation");
        l.add(null, false, 2, new int[]{-1}, "<num>", "time in ms");
        l.add(null, false, 1, new int[]{2}, "rd", "specify route distinguisher");
        l.add(null, false, 2, new int[]{-1}, "<rd>", "rd in ASnum:IDnum format");
        l.add(null, false, 1, new int[]{2}, "rt-both", "specify route target");
        l.add(null, false, 2, new int[]{2, -1}, "<rt>", "rt in ASnum:IDnum format");
        l.add(null, false, 1, new int[]{2}, "rt-import", "specify route target import");
        l.add(null, false, 2, new int[]{2, -1}, "<rt>", "rt in ASnum:IDnum format");
        l.add(null, false, 1, new int[]{2}, "rt-export", "specify route target export");
        l.add(null, false, 2, new int[]{2, -1}, "<rt>", "rt in ASnum:IDnum format");
        l.add(null, false, 1, new int[]{2}, "rt4both", "specify route target");
        l.add(null, false, 2, new int[]{2, -1}, "<rt>", "rt in ASnum:IDnum format");
        l.add(null, false, 1, new int[]{2}, "rt4import", "specify route target import");
        l.add(null, false, 2, new int[]{2, -1}, "<rt>", "rt in ASnum:IDnum format");
        l.add(null, false, 1, new int[]{2}, "rt4export", "specify route target export");
        l.add(null, false, 2, new int[]{2, -1}, "<rt>", "rt in ASnum:IDnum format");
        l.add(null, false, 1, new int[]{2}, "rt6both", "specify route target");
        l.add(null, false, 2, new int[]{2, -1}, "<rt>", "rt in ASnum:IDnum format");
        l.add(null, false, 1, new int[]{2}, "rt6import", "specify route target import");
        l.add(null, false, 2, new int[]{2, -1}, "<rt>", "rt in ASnum:IDnum format");
        l.add(null, false, 1, new int[]{2}, "rt6export", "specify route target export");
        l.add(null, false, 2, new int[]{2, -1}, "<rt>", "rt in ASnum:IDnum format");
        l.add(null, false, 1, new int[]{2}, "clr-both", "specify color");
        l.add(null, false, 2, new int[]{2, -1}, "<num>", "number");
        l.add(null, false, 1, new int[]{2}, "clr-import", "specify color import");
        l.add(null, false, 2, new int[]{2, -1}, "<num>", "number");
        l.add(null, false, 1, new int[]{2}, "clr-export", "specify color export");
        l.add(null, false, 2, new int[]{2, -1}, "<num>", "number");
        l.add(null, false, 1, new int[]{2}, "clr4both", "specify color");
        l.add(null, false, 2, new int[]{2, -1}, "<num>", "number");
        l.add(null, false, 1, new int[]{2}, "clr4import", "specify color import");
        l.add(null, false, 2, new int[]{2, -1}, "<num>", "number");
        l.add(null, false, 1, new int[]{2}, "clr4export", "specify color export");
        l.add(null, false, 2, new int[]{2, -1}, "<num>", "number");
        l.add(null, false, 1, new int[]{2}, "clr6both", "specify color");
        l.add(null, false, 2, new int[]{2, -1}, "<num>", "number");
        l.add(null, false, 1, new int[]{2}, "clr6import", "specify color import");
        l.add(null, false, 2, new int[]{2, -1}, "<num>", "number");
        l.add(null, false, 1, new int[]{2}, "clr6export", "specify color export");
        l.add(null, false, 2, new int[]{2, -1}, "<num>", "number");
        l.add(null, false, 1, new int[]{2}, "unreach-rate", "rate limit icmp generation");
        l.add(null, false, 2, new int[]{3}, "<num>", "packets allowed");
        l.add(null, false, 3, new int[]{-1}, "<num>", "millisecs between them");
        l.add(null, false, 1, new int[]{2}, "unreach4rate", "rate limit icmp generation");
        l.add(null, false, 2, new int[]{3}, "<num>", "packets allowed");
        l.add(null, false, 3, new int[]{-1}, "<num>", "millisecs between them");
        l.add(null, false, 1, new int[]{2}, "unreach6rate", "rate limit icmp generation");
        l.add(null, false, 2, new int[]{3}, "<num>", "packets allowed");
        l.add(null, false, 3, new int[]{-1}, "<num>", "millisecs between them");
        l.add(null, false, 1, new int[]{2}, "route4limit", "maximum ipv4 routes allowed");
        l.add(null, false, 2, new int[]{3}, "<num>", "number of unicast routes");
        l.add(null, false, 3, new int[]{4}, "<num>", "number of labeled routes");
        l.add(null, false, 4, new int[]{5}, "<num>", "number of multicast routes");
        l.add(null, false, 5, new int[]{-1}, "<num>", "number of flowspec routes");
        l.add(null, false, 1, new int[]{2}, "route6limit", "maximum ipv6 routes allowed");
        l.add(null, false, 2, new int[]{3}, "<num>", "number of unicast routes");
        l.add(null, false, 3, new int[]{4}, "<num>", "number of labeled routes");
        l.add(null, false, 4, new int[]{5}, "<num>", "number of multicast routes");
        l.add(null, false, 5, new int[]{-1}, "<num>", "number of flowspec routes");
        l.add(null, false, 1, new int[]{2}, "label4filter", "specify ipv4 label filter");
        l.add(null, false, 2, new int[]{-1}, "<name:pl>", "name of prefix list");
        l.add(null, false, 1, new int[]{2}, "label6filter", "specify ipv6 label filter");
        l.add(null, false, 2, new int[]{-1}, "<name:pl>", "name of prefix list");
        l.add(null, false, 1, new int[]{2}, "label4common", "specify ipv4 common label");
        l.add(null, false, 2, new int[]{-1}, "<num>", "label value");
        l.add(null, false, 1, new int[]{2}, "label6common", "specify ipv6 common label");
        l.add(null, false, 2, new int[]{-1}, "<num>", "label value");
        l.add(null, false, 1, new int[]{2}, "iface4start", "specify ipv4 interface index");
        l.add(null, false, 2, new int[]{-1}, "<num>", "start index");
        l.add(null, false, 1, new int[]{2}, "iface6start", "specify ipv6 interface index");
        l.add(null, false, 2, new int[]{-1}, "<num>", "start index");
        l.add(null, false, 1, new int[]{2}, "import4list", "specify ipv4 import filter");
        l.add(null, false, 2, new int[]{-1}, "<name:pl>", "name of prefix list");
        l.add(null, false, 1, new int[]{2}, "import6list", "specify ipv6 import filter");
        l.add(null, false, 2, new int[]{-1}, "<name:pl>", "name of prefix list");
        l.add(null, false, 1, new int[]{2}, "export4list", "specify ipv4 export filter");
        l.add(null, false, 2, new int[]{-1}, "<name:pl>", "name of prefix list");
        l.add(null, false, 1, new int[]{2}, "export6list", "specify ipv6 export filter");
        l.add(null, false, 2, new int[]{-1}, "<name:pl>", "name of prefix list");
        l.add(null, false, 1, new int[]{2}, "import4map", "specify ipv4 import filter");
        l.add(null, false, 2, new int[]{-1}, "<name:rm>", "name of route map");
        l.add(null, false, 1, new int[]{2}, "import6map", "specify ipv6 import filter");
        l.add(null, false, 2, new int[]{-1}, "<name:rm>", "name of route map");
        l.add(null, false, 1, new int[]{2}, "export4map", "specify ipv4 export filter");
        l.add(null, false, 2, new int[]{-1}, "<name:rm>", "name of route map");
        l.add(null, false, 1, new int[]{2}, "export6map", "specify ipv6 export filter");
        l.add(null, false, 2, new int[]{-1}, "<name:rm>", "name of route map");
        l.add(null, false, 1, new int[]{2}, "import4policy", "specify ipv4 import filter");
        l.add(null, false, 2, new int[]{-1}, "<name:rpl>", "name of route policy");
        l.add(null, false, 1, new int[]{2}, "import6policy", "specify ipv6 import filter");
        l.add(null, false, 2, new int[]{-1}, "<name:rpl>", "name of route policy");
        l.add(null, false, 1, new int[]{2}, "export4policy", "specify ipv4 export filter");
        l.add(null, false, 2, new int[]{-1}, "<name:rpl>", "name of route policy");
        l.add(null, false, 1, new int[]{2}, "export6policy", "specify ipv6 export filter");
        l.add(null, false, 2, new int[]{-1}, "<name:rpl>", "name of route policy");
        l.add(null, false, 1, new int[]{2}, "dapp4", "specify ipv4 data plane policer");
        l.add(null, false, 2, new int[]{-1}, "<name:pm>", "name of policy map");
        l.add(null, false, 1, new int[]{2}, "dapp6", "specify ipv6 data plane policer");
        l.add(null, false, 2, new int[]{-1}, "<name:pm>", "name of policy map");
        l.add(null, false, 1, new int[]{2}, "copp4in", "specify ipv4 receive control plane policer");
        l.add(null, false, 2, new int[]{-1}, "<name:pm>", "name of policy map");
        l.add(null, false, 1, new int[]{2}, "copp4out", "specify ipv4 transmit control plane policer");
        l.add(null, false, 2, new int[]{-1}, "<name:pm>", "name of policy map");
        l.add(null, false, 1, new int[]{2}, "copp6in", "specify ipv6 receive control plane policer");
        l.add(null, false, 2, new int[]{-1}, "<name:pm>", "name of policy map");
        l.add(null, false, 1, new int[]{2}, "copp6out", "specify ipv6 transmit control plane policer");
        l.add(null, false, 2, new int[]{-1}, "<name:pm>", "name of policy map");
        l.add(null, false, 1, new int[]{2}, "packet4filter", "specify ipv4 packet filter");
        l.add(null, false, 2, new int[]{-1}, "<name:acl>", "name of access list");
        l.add(null, false, 1, new int[]{2}, "packet6filter", "specify ipv6 packet filter");
        l.add(null, false, 2, new int[]{-1}, "<name:acl>", "name of access list");
        l.add(null, false, 1, new int[]{-1}, "propagate-ttl", "specify to copy ip ttl to mpls ttl");
        l.add(null, false, 1, new int[]{-1}, "propagate4ttl", "specify to copy ip ttl to mpls ttl");
        l.add(null, false, 1, new int[]{-1}, "propagate6ttl", "specify to copy ip ttl to mpls ttl");
        l.add(null, false, 1, new int[]{-1}, "report-labels", "append icmp extension with labels");
        l.add(null, false, 1, new int[]{-1}, "report4labels", "append icmp extension with labels");
        l.add(null, false, 1, new int[]{-1}, "report6labels", "append icmp extension with labels");
        l.add(null, false, 1, new int[]{2}, "mdt4", "enable multicast distribution tree for ipv4");
        l.add(null, false, 1, new int[]{2}, "mdt6", "enable multicast distribution tree for ipv6");
        l.add(null, false, 2, new int[]{-1}, "none", "no vpn mode");
        l.add(null, false, 2, new int[]{-1}, "mldp", "use mldp");
        l.add(null, false, 2, new int[]{-1}, "bier", "use bier");
        l.add(null, false, 1, new int[]{2}, "label-mode", "specify label allocation mode");
        l.add(null, false, 1, new int[]{2}, "label4mode", "specify label allocation mode");
        l.add(null, false, 1, new int[]{2}, "label6mode", "specify label allocation mode");
        l.add(null, false, 2, new int[]{-1}, "per-prefix", "label for all prefixes");
        l.add(null, false, 2, new int[]{-1}, "all-igp", "label for all igp prefixes");
        l.add(null, false, 2, new int[]{-1}, "host-route", "label for host routes");
        l.add(null, false, 2, new int[]{-1}, "connected", "label for connected routes");
        l.add(null, false, 2, new int[]{-1}, "per-vrf", "common label for the vrf");
        l.add(null, false, 1, new int[]{2}, "incremental4", "specify ipv4 incremental limit");
        l.add(null, false, 2, new int[]{-1}, "<num>", "routes");
        l.add(null, false, 1, new int[]{2}, "incremental6", "specify ipv6 incremental limit");
        l.add(null, false, 2, new int[]{-1}, "<num>", "routes");
        l.add(null, false, 1, new int[]{2}, "threshold-traffic", "specify alarm limit");
        l.add(null, false, 1, new int[]{2}, "threshold4traffic", "specify ipv4 alarm limit");
        l.add(null, false, 1, new int[]{2}, "threshold6traffic", "specify ipv6 alarm limit");
        l.add(null, false, 2, new int[]{-1}, "<num>", "percent");
        l.add(null, false, 1, new int[]{2}, "threshold-route", "specify alarm limit");
        l.add(null, false, 1, new int[]{2}, "threshold4route", "specify ipv4 alarm limit");
        l.add(null, false, 1, new int[]{2}, "threshold6route", "specify ipv6 alarm limit");
        l.add(null, false, 2, new int[]{-1}, "<num>", "percent");
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

    private List<Integer> string2clrs(cmds cmd) {
        List<Integer> res = new ArrayList<Integer>();
        for (;;) {
            String a = cmd.word();
            if (a.length() < 1) {
                break;
            }
            res.add(bits.str2num(a));
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
        if (a.equals("clr-import")) {
            List<Integer> res = string2clrs(cmd);
            fwd4.clrImp = res;
            fwd6.clrImp = res;
            fwd4.routerStaticChg();
            fwd6.routerStaticChg();
            return;
        }
        if (a.equals("clr-export")) {
            List<Integer> res = string2clrs(cmd);
            fwd4.clrExp = res;
            fwd6.clrExp = res;
            fwd4.routerStaticChg();
            fwd6.routerStaticChg();
            return;
        }
        if (a.equals("clr-both")) {
            List<Integer> res = string2clrs(cmd);
            fwd4.clrImp = res;
            fwd6.clrImp = res;
            fwd4.clrExp = res;
            fwd6.clrExp = res;
            fwd4.routerStaticChg();
            fwd6.routerStaticChg();
            return;
        }
        if (a.equals("optimize-lookup")) {
            fwd4.optimize = true;
            fwd6.optimize = true;
            fwd4.routerStaticChg();
            return;
        }
        if (a.equals("optimize4lookup")) {
            fwd4.optimize = true;
            fwd4.routerStaticChg();
            return;
        }
        if (a.equals("optimize6lookup")) {
            fwd6.optimize = true;
            fwd6.routerStaticChg();
            return;
        }
        if (a.equals("update-interval")) {
            int res = bits.str2num(cmd.word());
            fwd4.updateInterval = res;
            fwd6.updateInterval = res;
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
        if (a.equals("clr4import")) {
            fwd4.clrImp = string2clrs(cmd);
            fwd4.routerStaticChg();
            return;
        }
        if (a.equals("clr4export")) {
            fwd4.clrExp = string2clrs(cmd);
            fwd4.routerStaticChg();
            return;
        }
        if (a.equals("clr4both")) {
            List<Integer> res = string2clrs(cmd);
            fwd4.clrImp = res;
            fwd4.clrExp = res;
            fwd4.routerStaticChg();
            return;
        }
        if (a.equals("clr6import")) {
            fwd6.clrImp = string2clrs(cmd);
            fwd6.routerStaticChg();
            return;
        }
        if (a.equals("clr6export")) {
            fwd6.clrExp = string2clrs(cmd);
            fwd6.routerStaticChg();
            return;
        }
        if (a.equals("clr6both")) {
            List<Integer> res = string2clrs(cmd);
            fwd6.clrImp = res;
            fwd6.clrExp = res;
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
            fwd4.mdtMod = string2mdtmod(cmd.word());
            fwd4.routerStaticChg();
            return;
        }
        if (a.equals("mdt6")) {
            fwd6.mdtMod = string2mdtmod(cmd.word());
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
            tabLabelEntry[] ntry = tabLabel.allocate(tabLabelEntry.owner.vrfComm, label4comm, 1);
            if (ntry == null) {
                return;
            }
            tabLabelEntry old = fwd4.commonLabel;
            fwd4.commonLabel = ntry[0];
            fwd4.routerStaticChg();
            tabLabel.release(old, tabLabelEntry.owner.vrfComm);
            return;
        }
        if (a.equals("label6common")) {
            label6comm = bits.str2num(cmd.word());
            tabLabelEntry[] ntry = tabLabel.allocate(tabLabelEntry.owner.vrfComm, label6comm, 1);
            if (ntry == null) {
                return;
            }
            tabLabelEntry old = fwd6.commonLabel;
            fwd6.commonLabel = ntry[0];
            fwd6.routerStaticChg();
            tabLabel.release(old, tabLabelEntry.owner.vrfComm);
            return;
        }
        if (a.equals("unreach-rate")) {
            int res1 = bits.str2num(cmd.word());
            int res2 = bits.str2num(cmd.word());
            fwd4.unreach = new tabRateLimit(res1, res2);
            fwd6.unreach = new tabRateLimit(res1, res2);
            return;
        }
        if (a.equals("unreach4rate")) {
            int res = bits.str2num(cmd.word());
            fwd4.unreach = new tabRateLimit(res, bits.str2num(cmd.word()));
            return;
        }
        if (a.equals("unreach6rate")) {
            int res = bits.str2num(cmd.word());
            fwd6.unreach = new tabRateLimit(res, bits.str2num(cmd.word()));
            return;
        }
        if (a.equals("report-labels")) {
            fwd4.mplsExtRep = true;
            fwd6.mplsExtRep = true;
            return;
        }
        if (a.equals("report4labels")) {
            fwd4.mplsExtRep = true;
            return;
        }
        if (a.equals("report6labels")) {
            fwd6.mplsExtRep = true;
            return;
        }
        if (a.equals("propagate-ttl")) {
            fwd4.mplsPropTtl = true;
            fwd6.mplsPropTtl = true;
            return;
        }
        if (a.equals("propagate4ttl")) {
            fwd4.mplsPropTtl = true;
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
        if (a.equals("incremental6")) {
            fwd6.incrLimit = bits.str2num(cmd.word());
            return;
        }
        if (a.equals("threshold4traffic")) {
            fwd4.thresholdT = bits.str2num(cmd.word());
            return;
        }
        if (a.equals("threshold6traffic")) {
            fwd6.thresholdT = bits.str2num(cmd.word());
            return;
        }
        if (a.equals("threshold-traffic")) {
            int res = bits.str2num(cmd.word());
            fwd4.thresholdT = res;
            fwd6.thresholdT = res;
            return;
        }
        if (a.equals("threshold-route")) {
            int res = bits.str2num(cmd.word());
            fwd4.thresholdR = res;
            fwd6.thresholdR = res;
            return;
        }
        if (a.equals("threshold4route")) {
            fwd4.thresholdR = bits.str2num(cmd.word());
            return;
        }
        if (a.equals("threshold6route")) {
            fwd6.thresholdR = bits.str2num(cmd.word());
            return;
        }
        if (!a.equals(cmds.negated)) {
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
        if (a.equals("clr-import")) {
            fwd4.clrImp = new ArrayList<Integer>();
            fwd6.clrImp = new ArrayList<Integer>();
            fwd4.routerStaticChg();
            fwd6.routerStaticChg();
            return;
        }
        if (a.equals("clr-export")) {
            fwd4.clrExp = new ArrayList<Integer>();
            fwd6.clrExp = new ArrayList<Integer>();
            fwd4.routerStaticChg();
            fwd6.routerStaticChg();
            return;
        }
        if (a.equals("clr-both")) {
            fwd4.clrImp = new ArrayList<Integer>();
            fwd6.clrImp = new ArrayList<Integer>();
            fwd4.clrExp = new ArrayList<Integer>();
            fwd6.clrExp = new ArrayList<Integer>();
            fwd4.routerStaticChg();
            fwd6.routerStaticChg();
            return;
        }
        if (a.equals("optimize-lookup")) {
            fwd4.optimize = false;
            fwd6.optimize = false;
            fwd4.routerStaticChg();
            return;
        }
        if (a.equals("optimize4lookup")) {
            fwd4.optimize = false;
            fwd4.routerStaticChg();
            return;
        }
        if (a.equals("optimize6lookup")) {
            fwd6.optimize = false;
            fwd6.routerStaticChg();
            return;
        }
        if (a.equals("update-interval")) {
            fwd4.updateInterval = 0;
            fwd6.updateInterval = 0;
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
        if (a.equals("clr4import")) {
            fwd4.clrImp = new ArrayList<Integer>();
            fwd4.routerStaticChg();
            return;
        }
        if (a.equals("clr4export")) {
            fwd4.clrExp = new ArrayList<Integer>();
            fwd4.routerStaticChg();
            return;
        }
        if (a.equals("clr4both")) {
            fwd4.clrImp = new ArrayList<Integer>();
            fwd4.clrExp = new ArrayList<Integer>();
            fwd4.routerStaticChg();
            return;
        }
        if (a.equals("clr6import")) {
            fwd6.clrImp = new ArrayList<Integer>();
            fwd6.routerStaticChg();
            return;
        }
        if (a.equals("clr6export")) {
            fwd6.clrExp = new ArrayList<Integer>();
            fwd6.routerStaticChg();
            return;
        }
        if (a.equals("clr6both")) {
            fwd6.clrImp = new ArrayList<Integer>();
            fwd6.clrExp = new ArrayList<Integer>();
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
            fwd4.mdtMod = ipFwd.mdtMode.none;
            fwd4.routerStaticChg();
            return;
        }
        if (a.equals("mdt6")) {
            fwd6.mdtMod = ipFwd.mdtMode.none;
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
        if (a.equals("unreach-rate")) {
            fwd4.unreach = null;
            fwd6.unreach = null;
            return;
        }
        if (a.equals("unreach4rate")) {
            fwd4.unreach = null;
            return;
        }
        if (a.equals("unreach6rate")) {
            fwd6.unreach = null;
            return;
        }
        if (a.equals("report-labels")) {
            fwd4.mplsExtRep = false;
            fwd6.mplsExtRep = false;
            return;
        }
        if (a.equals("report4labels")) {
            fwd4.mplsExtRep = false;
            return;
        }
        if (a.equals("report6labels")) {
            fwd6.mplsExtRep = false;
            return;
        }
        if (a.equals("propagate-ttl")) {
            fwd4.mplsPropTtl = false;
            fwd6.mplsPropTtl = false;
            return;
        }
        if (a.equals("propagate4ttl")) {
            fwd4.mplsPropTtl = false;
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
        if (a.equals("threshold4traffic")) {
            fwd4.thresholdT = 0;
            return;
        }
        if (a.equals("threshold6traffic")) {
            fwd6.thresholdT = 0;
            return;
        }
        if (a.equals("threshold-traffic")) {
            fwd4.thresholdT = 0;
            fwd6.thresholdT = 0;
            return;
        }
        if (a.equals("threshold-route")) {
            fwd4.thresholdR = 0;
            fwd6.thresholdR = 0;
            return;
        }
        if (a.equals("threshold4route")) {
            fwd4.thresholdR = 0;
            return;
        }
        if (a.equals("threshold6route")) {
            fwd6.thresholdR = 0;
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
