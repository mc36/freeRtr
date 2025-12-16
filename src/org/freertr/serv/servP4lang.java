package org.freertr.serv;

import org.freertr.util.keyword;
import java.util.List;
import org.freertr.addr.addrIP;
import org.freertr.addr.addrMac;
import org.freertr.cfg.cfgAceslst;
import org.freertr.cfg.cfgAll;
import org.freertr.cfg.cfgBndl;
import org.freertr.cfg.cfgBrdg;
import org.freertr.cfg.cfgIfc;
import org.freertr.cfg.cfgPrfxlst;
import org.freertr.cfg.cfgRoump;
import org.freertr.cfg.cfgRouplc;
import org.freertr.cfg.cfgVrf;
import org.freertr.ifc.ifcBridge;
import org.freertr.ifc.ifcBridgeIfc;
import org.freertr.ifc.ifcDn;
import org.freertr.ifc.ifcEthTyp;
import org.freertr.ifc.ifcEther;
import org.freertr.ifc.ifcPolka;
import org.freertr.ifc.ifcUp;
import org.freertr.ip.ipFwd;
import org.freertr.ip.ipIfc4;
import org.freertr.ip.ipIfc4arp;
import org.freertr.ip.ipIfc6;
import org.freertr.ip.ipMpls;
import org.freertr.pack.packHolder;
import org.freertr.pipe.pipeLine;
import org.freertr.pipe.pipeSide;
import org.freertr.prt.prtGenConn;
import org.freertr.prt.prtServS;
import org.freertr.tab.tabGen;
import org.freertr.tab.tabLabelEntry;
import org.freertr.tab.tabRoute;
import org.freertr.tab.tabRouteIface;
import org.freertr.user.userFilter;
import org.freertr.user.userFormat;
import org.freertr.user.userHelp;
import org.freertr.util.bits;
import org.freertr.util.cmds;
import org.freertr.util.logger;
import org.freertr.tab.tabNshEntry;
import org.freertr.tab.tabQos;
import org.freertr.tab.tabRouteEntry;
import org.freertr.tab.tabSessionEntry;
import org.freertr.util.counter;
import org.freertr.util.debugger;
import org.freertr.util.history;
import org.freertr.util.notifier;
import org.freertr.util.state;

/**
 * p4lang server
 *
 * @author matecsaba
 */
public class servP4lang extends servGeneric implements prtServS, servGenFwdr, ifcUp {

    private final static int magic1 = 0x00010000 | ipIfc4.type;

    private final static int magic2 = 0x0604beef;

    /**
     * create instance
     */
    public servP4lang() {
        pipeLine pl = new pipeLine(1024, false);
        pl.setClose();
        conn = new servP4langConn(pl.getSide(), this);
        parent = new servStack();
        parid = new servStackFwd(parent);
    }

    public String toString() {
        return "p4lang";
    }

    /**
     * get hardware forwarder info
     *
     * @return offload info
     */
    public String getShGenOneLiner() {
        return platform + ",brd=" + expBr.size() + ",vrf=" + expVrf.size() + ",prt=" + expIfc.size() + ",rec=" + reconns + ",cls=" + conn.pipe.isClosed() + ",rdy=" + conn.pipe.isReady();
    }

    /**
     * port
     */
    public final static int port = 9080;

    /**
     * buffer size
     */
    protected int bufSiz = 65536;

    /**
     * current connection
     */
    protected servP4langConn conn;

    /**
     * current stack
     */
    protected servStack parent;

    /**
     * current stack id
     */
    protected servStackFwd parid;

    /**
     * exported vrfs
     */
    protected tabGen<servP4langVrf> expVrf = new tabGen<servP4langVrf>();

    /**
     * export sockets
     */
    protected boolean expSck;

    /**
     * use magic names
     */
    protected boolean expMgc;

    /**
     * exported interfaces
     */
    protected tabGen<servP4langIfc> expIfc = new tabGen<servP4langIfc>();

    /**
     * exported neighbors
     */
    protected tabGen<servP4langNei> neighs = new tabGen<servP4langNei>();

    /**
     * exported srv6
     */
    protected cfgIfc expSrv6 = null;

    /**
     * exported bridges
     */
    protected tabGen<servP4langBr> expBr = new tabGen<servP4langBr>();

    /**
     * export interval
     */
    protected int expDelay = 1000;

    /**
     * last peer
     */
    protected addrIP remote = new addrIP();

    /**
     * minimum buffer size
     */
    protected int minBuf = 0;

    /**
     * last capability
     */
    protected String capability = null;

    /**
     * last platform
     */
    protected String platform = null;

    /**
     * last cpuport
     */
    protected int cpuPort;

    /**
     * last activity
     */
    protected long cpuLast;

    /**
     * first dynamic range
     */
    protected int ifcRngBeg = 0x10000;

    /**
     * last dynamic range
     */
    protected int ifcRngEnd = 0x20000;

    /**
     * first dynamic range
     */
    protected int vrfRngBeg = 0x10000;

    /**
     * last dynamic range
     */
    protected int vrfRngEnd = 0x20000;

    /**
     * first dynamic range
     */
    protected int neiRngBeg = 0x10000;

    /**
     * last dynamic range
     */
    protected int neiRngEnd = 0x20000;

    /**
     * last front panel
     */
    protected tabGen<servP4langMgcN> frontnam = new tabGen<servP4langMgcN>();

    /**
     * last fec mapping
     */
    protected tabGen<servP4langMgcN> fwderrcr = new tabGen<servP4langMgcN>();

    /**
     * last autoneg mapping
     */
    protected tabGen<servP4langMgcN> autonegs = new tabGen<servP4langMgcN>();

    /**
     * last flowcontrol mapping
     */
    protected tabGen<servP4langMgcN> flwctrls = new tabGen<servP4langMgcN>();

    /**
     * connection start
     */
    protected long started = 0;

    /**
     * connections accepted
     */
    protected int reconns = 0;

    /**
     * interconnection interface
     */
    protected ifcEthTyp interconn = null;

    /**
     * downlink interfaces
     */
    protected tabGen<servP4langDlnk> downLinks = new tabGen<servP4langDlnk>();
    /**
     * counter
     */
    protected counter cntr = new counter();

    /**
     * counter
     */
    protected notifier notif = new notifier();

    /**
     * controller text
     */
    protected List<String> statsTxt;

    /**
     * controller notifier
     */
    protected notifier statsNtf;

    /**
     * controller port
     */
    protected int statsPrt;

    /**
     * rounds done
     */
    protected int rndDoneNum;

    /**
     * rounds skiped
     */
    protected int rndSkipNum;

    /**
     * rounds time
     */
    protected int rndDoneTime;

    /**
     * rounds time
     */
    protected long rndDoneLast;

    /**
     * rounds time
     */
    protected long rndSkipLast;

    /**
     * messages sent
     */
    protected int msgsSent;

    /**
     * messages got
     */
    protected int msgsGot;

    /**
     * transmitted message statistics
     */
    protected tabGen<keyword> apiStatTx;

    /**
     * received message statistics
     */
    protected tabGen<keyword> apiStatRx;

    /**
     * interconnect interface
     */
    protected ifcDn intercon;

    /**
     * defaults text
     */
    public final static userFilter[] defaultF = {
        new userFilter("server p4lang .*", cmds.tabulator + "port " + port, null),
        new userFilter("server p4lang .*", cmds.tabulator + "protocol " + proto2string(protoAllStrm), null),
        new userFilter("server p4lang .*", cmds.tabulator + "buffer 65536", null),
        new userFilter("server p4lang .*", cmds.tabulator + cmds.negated + cmds.tabulator + "api-stat", null),
        new userFilter("server p4lang .*", cmds.tabulator + cmds.negated + cmds.tabulator + "export-names", null),
        new userFilter("server p4lang .*", cmds.tabulator + cmds.negated + cmds.tabulator + "export-srv6", null),
        new userFilter("server p4lang .*", cmds.tabulator + cmds.negated + cmds.tabulator + "export-copp4 .*", null),
        new userFilter("server p4lang .*", cmds.tabulator + cmds.negated + cmds.tabulator + "export-copp6 .*", null),
        new userFilter("server p4lang .*", cmds.tabulator + cmds.negated + cmds.tabulator + "filter-list4 .*", null),
        new userFilter("server p4lang .*", cmds.tabulator + cmds.negated + cmds.tabulator + "filter-list6 .*", null),
        new userFilter("server p4lang .*", cmds.tabulator + cmds.negated + cmds.tabulator + "filter-map4 .*", null),
        new userFilter("server p4lang .*", cmds.tabulator + cmds.negated + cmds.tabulator + "filter-map6 .*", null),
        new userFilter("server p4lang .*", cmds.tabulator + cmds.negated + cmds.tabulator + "filter-policy4 .*", null),
        new userFilter("server p4lang .*", cmds.tabulator + cmds.negated + cmds.tabulator + "filter-policy6 .*", null),
        new userFilter("server p4lang .*", cmds.tabulator + cmds.negated + cmds.tabulator + "filter-compress4 .*", null),
        new userFilter("server p4lang .*", cmds.tabulator + cmds.negated + cmds.tabulator + "filter-compress6 .*", null),
        new userFilter("server p4lang .*", cmds.tabulator + cmds.negated + cmds.tabulator + "export-socket", null),
        new userFilter("server p4lang .*", cmds.tabulator + cmds.negated + cmds.tabulator + "interconnect", null),
        new userFilter("server p4lang .*", cmds.tabulator + "export-interval 1000", null)
    };

    public userFilter[] srvDefFlt() {
        return defaultF;
    }

    public void srvShRun(String beg, List<String> l, int filter) {
        l.add(beg + "buffer " + bufSiz);
        cmds.cfgLine(l, !expMgc, beg, "export-names", "");
        cmds.cfgLine(l, apiStatTx == null, beg, "api-stat", "");
        for (int i = 0; i < expVrf.size(); i++) {
            servP4langVrf ntry = expVrf.get(i);
            l.add(beg + "export-vrf " + ntry.vrf.name);
            cmds.cfgLine(l, ntry.copp4c == null, beg, "export-copp4 " + ntry.vrf.name, "" + ntry.copp4c);
            cmds.cfgLine(l, ntry.copp6c == null, beg, "export-copp6 " + ntry.vrf.name, "" + ntry.copp6c);
            cmds.cfgLine(l, ntry.prflst4 == null, beg, "filter-list4 " + ntry.vrf.name, "" + ntry.prflst4);
            cmds.cfgLine(l, ntry.prflst6 == null, beg, "filter-list6 " + ntry.vrf.name, "" + ntry.prflst6);
            cmds.cfgLine(l, ntry.roumap4 == null, beg, "filter-map4 " + ntry.vrf.name, "" + ntry.roumap4);
            cmds.cfgLine(l, ntry.roumap6 == null, beg, "filter-map6 " + ntry.vrf.name, "" + ntry.roumap6);
            cmds.cfgLine(l, ntry.roupol4 == null, beg, "filter-policy4 " + ntry.vrf.name, "" + ntry.roupol4);
            cmds.cfgLine(l, ntry.roupol6 == null, beg, "filter-policy6 " + ntry.vrf.name, "" + ntry.roupol6);
            cmds.cfgLine(l, !ntry.compress4, beg, "filter-compress4 " + ntry.vrf.name, "");
            cmds.cfgLine(l, !ntry.compress6, beg, "filter-compress6 " + ntry.vrf.name, "");
        }
        for (int i = 0; i < expBr.size(); i++) {
            servP4langBr ntry = expBr.get(i);
            l.add(beg + "export-bridge " + ntry.br.number);
        }
        tabGen<servP4langMgcI> frnt = servP4langUtil.convTab(frontnam, expMgc);
        tabGen<servP4langMgcI> errs = servP4langUtil.convTab(fwderrcr, expMgc);
        tabGen<servP4langMgcI> aung = servP4langUtil.convTab(autonegs, expMgc);
        tabGen<servP4langMgcI> flwc = servP4langUtil.convTab(flwctrls, expMgc);
        for (int i = 0; i < expIfc.size(); i++) {
            servP4langIfc ntry = expIfc.get(i);
            if (ntry.hidden) {
                continue;
            }
            String a;
            if (ntry.dynamic) {
                a = "dynamic";
            } else {
                a = servP4langUtil.convId(ntry.id, frnt);
            }
            a += " " + ntry.speed + " " + servP4langUtil.convId(ntry.errCorr, errs) + " " + servP4langUtil.convId(ntry.autoNeg, aung) + " " + servP4langUtil.convId(ntry.flowCtrl, flwc);
            if (ntry.reinit != null) {
                a = ntry.reinit;
            }
            l.add(beg + "export-port " + ntry.ifc.name + " " + a);
        }
        if (expSrv6 == null) {
            l.add(beg + cmds.negated + cmds.tabulator + "export-srv6");
        } else {
            l.add(beg + "export-srv6 " + expSrv6.name);
        }
        cmds.cfgLine(l, !expSck, beg, "export-socket", "");
        l.add(beg + "export-interval " + expDelay);
        cmds.cfgLine(l, interconn == null, beg, "interconnect", "" + interconn);
        for (int i = 0; i < downLinks.size(); i++) {
            servP4langDlnk ntry = downLinks.get(i);
            l.add(beg + "downlink " + ntry.id + " " + ntry.ifc);
        }
    }

    public boolean srvCfgStr(cmds cmd) {
        String s = cmd.word();
        if (s.equals("buffer")) {
            bufSiz = bits.str2num(cmd.word());
            return false;
        }
        if (s.equals("export-vrf")) {
            cfgVrf vrf = cfgAll.vrfFind(cmd.word(), false);
            if (vrf == null) {
                cmd.error("no such vrf");
                return false;
            }
            if (findVrf(vrf.fwd4) != null) {
                cmd.error("already exported");
                return false;
            }
            int i = getNextDynVrf();
            if (i < 0) {
                return false;
            }
            servP4langVrf ntry = new servP4langVrf(i);
            ntry.doClear();
            ntry.vrf = vrf;
            expVrf.put(ntry);
            vrf.fwd4.tableChanged = notif;
            vrf.fwd6.tableChanged = notif;
            return false;
        }
        if (s.equals("export-bridge")) {
            cfgBrdg br = cfgAll.brdgFind(cmd.word(), false);
            if (br == null) {
                cmd.error("no such bridge");
                return false;
            }
            servP4langBr ntry = new servP4langBr(br.number);
            ntry.doClear();
            ntry.br = br;
            expBr.put(ntry);
            return false;
        }
        if (s.equals("downlink")) {
            int i = front2id(null, cmd.word(), false);
            if (i < 0) {
                cmd.error("no such frontpanel port");
                return false;
            }
            servP4langDlnk ntry = new servP4langDlnk(this, i);
            cfgIfc ifc = cfgAll.ifcFind(cmd.word(), 0);
            if (ifc == null) {
                cmd.error("no such interface");
                return false;
            }
            ntry.ifc = ifc.ethtyp;
            ntry.ifc.addET(-1, "p4lang", ntry);
            ntry.ifc.updateET(-1, ntry);
            ntry.parent.setFilter(true);
            downLinks.add(ntry);
            return false;
        }
        if (s.equals("interconnect")) {
            cfgIfc ifc = cfgAll.ifcFind(cmd.word(), 0);
            if (ifc == null) {
                cmd.error("no such interface");
                return false;
            }
            interconn = ifc.ethtyp;
            interconn.addET(-1, "p4lang", this);
            interconn.updateET(-1, this);
            interconn.setFilter(true);
            return false;
        }
        if (s.equals("export-copp4")) {
            cfgVrf vrf = cfgAll.vrfFind(cmd.word(), false);
            if (vrf == null) {
                cmd.error("no such vrf");
                return false;
            }
            servP4langVrf pv = findVrf(vrf.fwd4);
            if (pv == null) {
                cmd.error("vrf not exported");
                return false;
            }
            cfgAceslst acl = cfgAll.aclsFind(cmd.word(), false);
            if (acl == null) {
                cmd.error("no such access list");
                return false;
            }
            pv.copp4c = acl.aceslst;
            return false;
        }
        if (s.equals("export-copp6")) {
            cfgVrf vrf = cfgAll.vrfFind(cmd.word(), false);
            if (vrf == null) {
                cmd.error("no such vrf");
                return false;
            }
            servP4langVrf pv = findVrf(vrf.fwd4);
            if (pv == null) {
                cmd.error("vrf not exported");
                return false;
            }
            cfgAceslst acl = cfgAll.aclsFind(cmd.word(), false);
            if (acl == null) {
                cmd.error("no such access list");
                return false;
            }
            pv.copp6c = acl.aceslst;
            return false;
        }
        if (s.equals("filter-compress4")) {
            cfgVrf vrf = cfgAll.vrfFind(cmd.word(), false);
            if (vrf == null) {
                cmd.error("no such vrf");
                return false;
            }
            servP4langVrf pv = findVrf(vrf.fwd4);
            if (pv == null) {
                cmd.error("vrf not exported");
                return false;
            }
            pv.compress4 = true;
            return false;
        }
        if (s.equals("filter-compress6")) {
            cfgVrf vrf = cfgAll.vrfFind(cmd.word(), false);
            if (vrf == null) {
                cmd.error("no such vrf");
                return false;
            }
            servP4langVrf pv = findVrf(vrf.fwd4);
            if (pv == null) {
                cmd.error("vrf not exported");
                return false;
            }
            pv.compress6 = true;
            return false;
        }
        if (s.equals("filter-list4")) {
            cfgVrf vrf = cfgAll.vrfFind(cmd.word(), false);
            if (vrf == null) {
                cmd.error("no such vrf");
                return false;
            }
            servP4langVrf pv = findVrf(vrf.fwd4);
            if (pv == null) {
                cmd.error("vrf not exported");
                return false;
            }
            cfgPrfxlst pfx = cfgAll.prfxFind(cmd.word(), false);
            if (pfx == null) {
                cmd.error("no such prefix list");
                return false;
            }
            pv.prflst4 = pfx.prflst;
            return false;
        }
        if (s.equals("filter-list6")) {
            cfgVrf vrf = cfgAll.vrfFind(cmd.word(), false);
            if (vrf == null) {
                cmd.error("no such vrf");
                return false;
            }
            servP4langVrf pv = findVrf(vrf.fwd4);
            if (pv == null) {
                cmd.error("vrf not exported");
                return false;
            }
            cfgPrfxlst pfx = cfgAll.prfxFind(cmd.word(), false);
            if (pfx == null) {
                cmd.error("no such prefix list");
                return false;
            }
            pv.prflst6 = pfx.prflst;
            return false;
        }
        if (s.equals("filter-map4")) {
            cfgVrf vrf = cfgAll.vrfFind(cmd.word(), false);
            if (vrf == null) {
                cmd.error("no such vrf");
                return false;
            }
            servP4langVrf pv = findVrf(vrf.fwd4);
            if (pv == null) {
                cmd.error("vrf not exported");
                return false;
            }
            cfgRoump rm = cfgAll.rtmpFind(cmd.word(), false);
            if (rm == null) {
                cmd.error("no such route map");
                return false;
            }
            pv.roumap4 = rm.roumap;
            return false;
        }
        if (s.equals("filter-map6")) {
            cfgVrf vrf = cfgAll.vrfFind(cmd.word(), false);
            if (vrf == null) {
                cmd.error("no such vrf");
                return false;
            }
            servP4langVrf pv = findVrf(vrf.fwd4);
            if (pv == null) {
                cmd.error("vrf not exported");
                return false;
            }
            cfgRoump rm = cfgAll.rtmpFind(cmd.word(), false);
            if (rm == null) {
                cmd.error("no such route map");
                return false;
            }
            pv.roumap6 = rm.roumap;
            return false;
        }
        if (s.equals("filter-policy4")) {
            cfgVrf vrf = cfgAll.vrfFind(cmd.word(), false);
            if (vrf == null) {
                cmd.error("no such vrf");
                return false;
            }
            servP4langVrf pv = findVrf(vrf.fwd4);
            if (pv == null) {
                cmd.error("vrf not exported");
                return false;
            }
            cfgRouplc rp = cfgAll.rtplFind(cmd.word(), false);
            if (rp == null) {
                cmd.error("no such route policy");
                return false;
            }
            pv.roupol4 = rp.rouplc;
            return false;
        }
        if (s.equals("filter-policy6")) {
            cfgVrf vrf = cfgAll.vrfFind(cmd.word(), false);
            if (vrf == null) {
                cmd.error("no such vrf");
                return false;
            }
            servP4langVrf pv = findVrf(vrf.fwd4);
            if (pv == null) {
                cmd.error("vrf not exported");
                return false;
            }
            cfgRouplc rp = cfgAll.rtplFind(cmd.word(), false);
            if (rp == null) {
                cmd.error("no such route policy");
                return false;
            }
            pv.roupol6 = rp.rouplc;
            return false;
        }
        if (s.equals("api-stat")) {
            apiStatTx = new tabGen<keyword>();
            apiStatRx = new tabGen<keyword>();
            return false;
        }
        if (s.equals("export-socket")) {
            expSck = true;
            return false;
        }
        if (s.equals("export-names")) {
            expMgc = true;
            return false;
        }
        if (s.equals("export-interval")) {
            expDelay = bits.str2num(cmd.word());
            return false;
        }
        if (s.equals("export-srv6")) {
            expSrv6 = cfgAll.ifcFind(cmd.word(), 0);
            if (expSrv6 == null) {
                cmd.error("no such interface");
                return false;
            }
            return false;
        }
        if (s.equals("export-port")) {
            cfgIfc ifc = cfgAll.ifcFind(cmd.word(), 0);
            if (ifc == null) {
                cmd.error("no such interface");
                return false;
            }
            if (ifc.vlanNum != 0) {
                cmd.error("no need to export subinterface");
                return false;
            }
            if ((ifc.type != tabRouteIface.ifaceType.sdn) && (ifc.type != tabRouteIface.ifaceType.bundle) && (ifc.type != tabRouteIface.ifaceType.bridge) && (ifc.type != tabRouteIface.ifaceType.dialer) && (ifc.type != tabRouteIface.ifaceType.hairpin) && (ifc.type != tabRouteIface.ifaceType.tunnel) && (ifc.type != tabRouteIface.ifaceType.virtppp) && (ifc.type != tabRouteIface.ifaceType.pweth) && (ifc.type != tabRouteIface.ifaceType.template)) {
                cmd.error("not p4lang interface");
                return false;
            }
            s = cmd.word();
            int i = front2id(ifc, s, true);
            if (i < 0) {
                if (!expMgc) {
                    cmd.error("no such frontpanel port");
                    return false;
                }
                i = getNextDynIfc();
                if (i < 0) {
                    return false;
                }
            } else {
                s = null;
            }
            servP4langIfc ntry = new servP4langIfc(this, i);
            if (s != null) {
                ntry.reinit = s + " " + cmd.getRemaining();
            }
            ntry.ifc = ifc;
            ntry.doClear();
            servP4langIfc old = expIfc.find(ntry);
            servP4langIfc orig = old;
            if (old != null) {
                if (ntry.ifc != old.ifc) {
                    cmd.error("port number already exported as " + old.ifc.name);
                    return false;
                }
                ntry = old;
            }
            for (i = 0; i < expIfc.size(); i++) {
                old = expIfc.get(i);
                if ((ifc == old.ifc) && (old.id != ntry.id)) {
                    cmd.error("interface already exported as port " + old.id);
                    return false;
                }
            }
            s = cmd.word();
            if (s.length() < 1) {
                s = "0";
            }
            ntry.speed = s;
            ntry.spdNum = bits.str2num(s);
            ntry.errCorr = servP4langUtil.toNum(fwderrcr, cmd.word(), 0);
            ntry.autoNeg = servP4langUtil.toNum(autonegs, cmd.word(), 0);
            ntry.flowCtrl = servP4langUtil.toNum(flwctrls, cmd.word(), 0);
            boolean need = ifc.type == tabRouteIface.ifaceType.sdn;
            ntry.dynamic = !need;
            if (ntry.spdNum == -1) {
                switch (ifc.type) {
                    case hairpin:
                    case bundle:
                    case sdn:
                        need = true;
                        break;
                    default:
                        break;
                }
            }
            if (need) {
                ntry.setUpper(ifc.ethtyp);
            }
            if (orig != null) {
                if (ntry.suppressState()) {
                    return false;
                }
                sendLine("state " + ntry.id + " 0 " + orig.getStateEnding());
                sendLine("ports_del " + ntry.id + " " + orig.getStateEnding());
                sendLine("ports_add " + ntry.id + " " + ntry.getStateEnding());
                ntry.sentState = state.states.close;
                ntry.sentMtu = 0;
                return false;
            }
            if (!ntry.suppressState()) {
                sendLine("ports_add " + ntry.id + " " + ntry.getStateEnding());
            }
            if (ntry.dynamic) {
                i = getNextDynIfc();
                if (i < 0) {
                    return false;
                }
                ntry.id = i;
            }
            ifc.ethtyp.hwHstry = new history();
            ifc.ethtyp.hwCntr = new counter();
            expIfc.put(ntry);
            ntry.setup2apiPack();
            return false;
        }
        if (!s.equals(cmds.negated)) {
            return true;
        }
        s = cmd.word();
        if (s.equals("export-vrf")) {
            cfgVrf vrf = cfgAll.vrfFind(cmd.word(), false);
            if (vrf == null) {
                cmd.error("no such vrf");
                return false;
            }
            servP4langVrf ntry = findVrf(vrf.fwd4);
            if (ntry == null) {
                cmd.error("not exported");
                return false;
            }
            expVrf.del(ntry);
            ntry.vrf.fwd4.tableChanged = null;
            ntry.vrf.fwd6.tableChanged = null;
            return false;
        }
        if (s.equals("export-copp4")) {
            cfgVrf vrf = cfgAll.vrfFind(cmd.word(), false);
            if (vrf == null) {
                cmd.error("no such vrf");
                return false;
            }
            servP4langVrf pv = findVrf(vrf.fwd4);
            if (pv == null) {
                cmd.error("vrf not exported");
                return false;
            }
            pv.copp4c = null;
            return false;
        }
        if (s.equals("export-copp6")) {
            cfgVrf vrf = cfgAll.vrfFind(cmd.word(), false);
            if (vrf == null) {
                cmd.error("no such vrf");
                return false;
            }
            servP4langVrf pv = findVrf(vrf.fwd4);
            if (pv == null) {
                cmd.error("vrf not exported");
                return false;
            }
            pv.copp6c = null;
            return false;
        }
        if (s.equals("filter-compress4")) {
            cfgVrf vrf = cfgAll.vrfFind(cmd.word(), false);
            if (vrf == null) {
                cmd.error("no such vrf");
                return false;
            }
            servP4langVrf pv = findVrf(vrf.fwd4);
            if (pv == null) {
                cmd.error("vrf not exported");
                return false;
            }
            pv.compress4 = false;
            return false;
        }
        if (s.equals("filter-compress6")) {
            cfgVrf vrf = cfgAll.vrfFind(cmd.word(), false);
            if (vrf == null) {
                cmd.error("no such vrf");
                return false;
            }
            servP4langVrf pv = findVrf(vrf.fwd4);
            if (pv == null) {
                cmd.error("vrf not exported");
                return false;
            }
            pv.compress6 = false;
            return false;
        }
        if (s.equals("filter-list4")) {
            cfgVrf vrf = cfgAll.vrfFind(cmd.word(), false);
            if (vrf == null) {
                cmd.error("no such vrf");
                return false;
            }
            servP4langVrf pv = findVrf(vrf.fwd4);
            if (pv == null) {
                cmd.error("vrf not exported");
                return false;
            }
            pv.prflst4 = null;
            return false;
        }
        if (s.equals("filter-list6")) {
            cfgVrf vrf = cfgAll.vrfFind(cmd.word(), false);
            if (vrf == null) {
                cmd.error("no such vrf");
                return false;
            }
            servP4langVrf pv = findVrf(vrf.fwd4);
            if (pv == null) {
                cmd.error("vrf not exported");
                return false;
            }
            pv.prflst6 = null;
            return false;
        }
        if (s.equals("filter-map4")) {
            cfgVrf vrf = cfgAll.vrfFind(cmd.word(), false);
            if (vrf == null) {
                cmd.error("no such vrf");
                return false;
            }
            servP4langVrf pv = findVrf(vrf.fwd4);
            if (pv == null) {
                cmd.error("vrf not exported");
                return false;
            }
            pv.roumap4 = null;
            return false;
        }
        if (s.equals("filter-map6")) {
            cfgVrf vrf = cfgAll.vrfFind(cmd.word(), false);
            if (vrf == null) {
                cmd.error("no such vrf");
                return false;
            }
            servP4langVrf pv = findVrf(vrf.fwd4);
            if (pv == null) {
                cmd.error("vrf not exported");
                return false;
            }
            pv.roumap6 = null;
            return false;
        }
        if (s.equals("filter-policy4")) {
            cfgVrf vrf = cfgAll.vrfFind(cmd.word(), false);
            if (vrf == null) {
                cmd.error("no such vrf");
                return false;
            }
            servP4langVrf pv = findVrf(vrf.fwd4);
            if (pv == null) {
                cmd.error("vrf not exported");
                return false;
            }
            pv.roupol4 = null;
            return false;
        }
        if (s.equals("filter-policy6")) {
            cfgVrf vrf = cfgAll.vrfFind(cmd.word(), false);
            if (vrf == null) {
                cmd.error("no such vrf");
                return false;
            }
            servP4langVrf pv = findVrf(vrf.fwd4);
            if (pv == null) {
                cmd.error("vrf not exported");
                return false;
            }
            pv.roupol6 = null;
            return false;
        }
        if (s.equals("api-stat")) {
            apiStatTx = null;
            apiStatRx = null;
            return false;
        }
        if (s.equals("export-names")) {
            expMgc = false;
            return false;
        }
        if (s.equals("export-socket")) {
            expSck = false;
            return false;
        }
        if (s.equals("export-bridge")) {
            servP4langBr ntry = new servP4langBr(bits.str2num(cmd.word()));
            ntry = expBr.del(ntry);
            if (ntry == null) {
                cmd.error("no such export");
                return false;
            }
            return false;
        }
        if (s.equals("downlink")) {
            int i = front2id(null, cmd.word(), false);
            if (i < 0) {
                cmd.error("no such frontpanel port");
                return false;
            }
            servP4langDlnk ntry = new servP4langDlnk(this, i);
            ntry = downLinks.del(ntry);
            if (ntry == null) {
                cmd.error("no such downlink");
                return false;
            }
            ntry.ifc.delET(-1);
            return false;
        }
        if (s.equals("interconnect")) {
            if (interconn == null) {
                return false;
            }
            interconn.delET(-1);
            interconn = null;
            return false;
        }
        if (s.equals("export-srv6")) {
            expSrv6 = null;
            return false;
        }
        if (s.equals("export-port")) {
            cfgIfc ifc = cfgAll.ifcFind(cmd.word(), 0);
            if (ifc == null) {
                cmd.error("no such intreface");
                return false;
            }
            servP4langIfc pif = findIfc(ifc);
            if (pif == null) {
                cmd.error("no such export");
                return false;
            }
            expIfc.del(pif);
            pif.tearDown();
            return false;
        }
        return true;
    }

    public void srvHelp(userHelp l) {
        l.add(null, false, 1, new int[]{2}, "buffer", "set buffer size on connection");
        l.add(null, false, 2, new int[]{-1}, "<num>", "buffer in bytes");
        List<String> lst = servP4langUtil.toHelp(frontnam);
        l.add(null, false, 1, new int[]{2}, "export-vrf", "specify vrf to export");
        l.add(null, false, 2, new int[]{3, -1}, "<name:vrf>", "vrf name");
        l.add(null, false, 3, new int[]{-1}, "<num>", "p4lang vrf number");
        l.add(null, false, 1, new int[]{2}, "export-bridge", "specify bridge to export");
        l.add(null, false, 2, new int[]{-1}, "<num>", "bridge number");
        l.add(null, false, 1, new int[]{2}, "export-port", "specify port to export");
        l.add(null, false, 2, new int[]{3}, "<name:ifc>", "interface name");
        l.add(null, false, 3, new int[]{4, -1}, "dynamic", "dynamic port number");
        l.add(lst, false, 3, new int[]{4, -1}, "<num:loc>", "port number");
        l.add(null, false, 4, new int[]{5, -1}, "<num>", "speed in gbps, see hw vendor manual");
        l.add(servP4langUtil.toHelp(fwderrcr), false, 5, new int[]{6, -1}, "<num:loc>", "fec, see hw vendor manual");
        l.add(servP4langUtil.toHelp(autonegs), false, 6, new int[]{7, -1}, "<num:loc>", "autoneg, see hw vendor manual");
        l.add(servP4langUtil.toHelp(flwctrls), false, 7, new int[]{-1}, "<num:loc>", "flowctrl, see hw vendor manual");
        l.add(null, false, 1, new int[]{2}, "export-srv6", "specify srv6 to export");
        l.add(null, false, 2, new int[]{-1}, "<name:ifc>", "interface name");
        l.add(null, false, 1, new int[]{-1}, "export-socket", "specify sockets to be exported");
        l.add(null, false, 1, new int[]{2}, "filter-compress4", "enable compressed fib export");
        l.add(null, false, 2, new int[]{-1}, "<name:vrf>", "vrf name");
        l.add(null, false, 1, new int[]{2}, "filter-compress6", "enable compressed fib export");
        l.add(null, false, 2, new int[]{-1}, "<name:vrf>", "vrf name");
        l.add(null, false, 1, new int[]{-1}, "export-names", "specify names to be exported");
        l.add(null, false, 1, new int[]{-1}, "api-stat", "count the sent api messages");
        l.add(null, false, 1, new int[]{2}, "export-copp4", "specify copp acl to export");
        l.add(null, false, 2, new int[]{3}, "<name:vrf>", "vrf name");
        l.add(null, false, 3, new int[]{-1}, "<name:acl>", "acl name");
        l.add(null, false, 1, new int[]{2}, "export-copp6", "specify copp acl to export");
        l.add(null, false, 2, new int[]{3}, "<name:vrf>", "vrf name");
        l.add(null, false, 3, new int[]{-1}, "<name:acl>", "acl name");
        l.add(null, false, 1, new int[]{2}, "filter-list4", "specify prefixes to export");
        l.add(null, false, 2, new int[]{3}, "<name:vrf>", "vrf name");
        l.add(null, false, 3, new int[]{-1}, "<name:pl>", "prefix list name");
        l.add(null, false, 1, new int[]{2}, "filter-list6", "specify prefixes to export");
        l.add(null, false, 2, new int[]{3}, "<name:vrf>", "vrf name");
        l.add(null, false, 3, new int[]{-1}, "<name:pl>", "prefix list name");
        l.add(null, false, 1, new int[]{2}, "filter-map4", "specify prefixes to export");
        l.add(null, false, 2, new int[]{3}, "<name:vrf>", "vrf name");
        l.add(null, false, 3, new int[]{-1}, "<name:rm>", "route map name");
        l.add(null, false, 1, new int[]{2}, "filter-map6", "specify prefixes to export");
        l.add(null, false, 2, new int[]{3}, "<name:vrf>", "vrf name");
        l.add(null, false, 3, new int[]{-1}, "<name:rm>", "route map name");
        l.add(null, false, 1, new int[]{2}, "filter-policy4", "specify prefixes to export");
        l.add(null, false, 2, new int[]{3}, "<name:vrf>", "vrf name");
        l.add(null, false, 3, new int[]{-1}, "<name:rpl>", "route policy name");
        l.add(null, false, 1, new int[]{2}, "filter-policy6", "specify prefixes to export");
        l.add(null, false, 2, new int[]{3}, "<name:vrf>", "vrf name");
        l.add(null, false, 3, new int[]{-1}, "<name:rpl>", "route policy name");
        l.add(null, false, 1, new int[]{2}, "export-interval", "specify export interval");
        l.add(null, false, 2, new int[]{-1}, "<num>", "time in ms");
        l.add(null, false, 1, new int[]{2}, "downlink", "specify downlink for packetin");
        l.add(lst, false, 2, new int[]{3}, "<num:loc>", "port number");
        l.add(null, false, 3, new int[]{-1}, "<name:ifc>", "interface name");
        l.add(null, false, 1, new int[]{2}, "interconnect", "specify port to for packetin");
        l.add(null, false, 2, new int[]{-1}, "<name:ifc>", "interface name");
    }

    public String srvName() {
        return "p4lang";
    }

    public int srvPort() {
        return port;
    }

    public int srvProto() {
        return protoAllStrm;
    }

    public boolean srvInit() {
        return genStrmStart(this, new pipeLine(bufSiz, false), 0);
    }

    public boolean srvDeinit() {
        return genericStop(0);
    }

    public boolean srvAccept(pipeSide pipe, prtGenConn id) {
        conn.pipe.setClose();
        notif.wakeup();
        id.timeout = 120000;
        pipe.lineRx = pipeSide.modTyp.modeCRorLF;
        pipe.lineTx = pipeSide.modTyp.modeLF;
        remote = id.peerAddr.copyBytes();
        minBuf = bufSiz / 2;
        conn = new servP4langConn(pipe, this);
        conn.startWork();
        return false;
    }

    /**
     * get generic show
     *
     * @return show
     */
    public userFormat getShowApiTx() {
        return keyword.dump(apiStatTx);
    }

    /**
     * get generic show
     *
     * @return show
     */
    public userFormat getShowApiRx() {
        return keyword.dump(apiStatRx);
    }

    /**
     * get route show
     *
     * @param bri bridge
     * @return show
     */
    public userFormat getShowBri(int bri) {
        servP4langBr ntry = expBr.find(new servP4langBr(bri));
        if (ntry == null) {
            return null;
        }
        userFormat res = new userFormat("|", "addr|iface|static|time|tx|rx|drop|tx|rx|drop", "3|3packet|3byte");
        for (int i = 0; i < ntry.macs.size(); i++) {
            res.add("" + ntry.macs.get(i));
        }
        return res;
    }

    /**
     * get generic show
     *
     * @return show
     */
    public userFormat getShowGen() {
        userFormat res = new userFormat("|", "category|value");
        res.add("peer|" + remote);
        res.add("closed|" + conn.pipe.isClosed());
        res.add("general|" + getShGenOneLiner());
        res.add("reconn|" + reconns);
        res.add("since|" + bits.time2str(cfgAll.timeZoneName, started + cfgAll.timeServerOffset, 3));
        res.add("for|" + bits.timePast(started));
        res.add("capability|" + capability);
        res.add("platform|" + platform);
        res.add("cpuport|" + cpuPort);
        res.add("intercon|" + intercon);
        res.add("dynamic ifc|" + ifcRngBeg + " " + ifcRngEnd);
        res.add("dynamic vrf|" + vrfRngBeg + " " + vrfRngEnd);
        res.add("dynamic nei|" + neiRngBeg + " " + neiRngEnd);
        res.add("messages sent|" + msgsSent);
        res.add("messages got|" + msgsGot);
        res.add("rounds done|" + rndDoneNum);
        res.add("last done|" + bits.time2str(cfgAll.timeZoneName, rndDoneLast + cfgAll.timeServerOffset, 3) + " (" + bits.timePast(rndDoneLast) + " ago)");
        res.add("time took|" + rndDoneTime);
        res.add("rounds skip|" + rndSkipNum);
        res.add("last skip|" + bits.time2str(cfgAll.timeZoneName, rndSkipLast + cfgAll.timeServerOffset, 3) + " (" + bits.timePast(rndSkipLast) + " ago)");
        res.add("interfaces|" + expIfc.size());
        res.add("bridges|" + expBr.size());
        res.add("vrfs|" + expVrf.size());
        return res;
    }

    /**
     * get cpuport show
     *
     * @return show
     */
    public userFormat getShowCpuprt() {
        userFormat res = new userFormat("|", "category|value");
        res.add("cpuport|" + cpuPort);
        res.add("intercon|" + intercon);
        res.add("available|" + (intercon != null));
        res.add("unexported|" + (expIfc.find(new servP4langIfc(this, cpuPort)) == null));
        if (intercon == null) {
            return res;
        }
        if (cpuPort < 0) {
            return res;
        }
        long tim = bits.getTime();
        cpuLast = 0;
        packHolder pck = new packHolder(true, true);
        pck.msbPutW(0, ipIfc4arp.type);
        pck.putSkip(2);
        pck.putFill(0, ipIfc4arp.size, 0);
        pck.msbPutD(0, magic1);
        pck.msbPutD(4, magic2);
        pck.putSkip(ipIfc4arp.size);
        pck.merge2beg();
        pck.ETHsrc.setAddr(intercon.getHwAddr());
        pck.ETHtrg.setAddr(addrMac.getBroadcast());
        pck.ETHtype = ipIfc4arp.type;
        ifcEther.createETHheader(pck, false);
        pck.msbPutD(0, cpuPort);
        pck.msbPutD(4, cpuPort);
        pck.putSkip(8);
        pck.merge2beg();
        ifcEther.parseETHheader(pck, false);
        intercon.sendPack(pck);
        for (int i = 0; i < 10; i++) {
            if (cpuLast > 0) {
                break;
            }
            bits.sleep(100);
        }
        res.add("rtt|" + (cpuLast - tim));
        return res;
    }

    /**
     * get frontpanel show
     *
     * @return show
     */
    public userFormat getShowFront() {
        userFormat res = new userFormat("|", "num|local|name");
        for (int i = 0; i < frontnam.size(); i++) {
            servP4langMgcN ntry = frontnam.get(i);
            servP4langIfc ifcH = findIfc(ntry.id);
            String ifcN = "n/a";
            if (ifcH != null) {
                if (ifcH.ifc != null) {
                    ifcN = "" + ifcH.ifc.name;
                }
            }
            if (ntry.id == cpuPort) {
                ifcN = "cpu-" + interconn;
            }
            res.add(ntry.id + "|" + ifcN + "|" + ntry.nam);
        }
        return res;
    }

    /**
     * get magic number show
     *
     * @return show
     */
    public userFormat getShowMagics() {
        userFormat res = new userFormat("|", "num|name");
        servP4langUtil.toShow("fec", fwderrcr, res);
        servP4langUtil.toShow("an", autonegs, res);
        servP4langUtil.toShow("flwctl", flwctrls, res);
        return res;
    }

    /**
     * get vrfs show
     *
     * @return show
     */
    public userFormat getShowVrfs() {
        userFormat res = new userFormat("|", "sent|name");
        for (int i = 0; i < expVrf.size(); i++) {
            servP4langVrf ntry = expVrf.get(i);
            res.add(ntry.id + "|vrf " + ntry.vrf.name);
        }
        return res;
    }

    /**
     * get interfaces show
     *
     * @return show
     */
    public userFormat getShowIfaces() {
        userFormat res = new userFormat("|", "sent|name|apipak");
        for (int i = 0; i < expIfc.size(); i++) {
            servP4langIfc ntry = expIfc.get(i);
            if (ntry.ifc == null) {
                res.add(ntry.id + "|brif " + ntry.brif.getIfcName() + "|" + ntry.apiPack);
            } else {
                res.add(ntry.id + "|ifc " + ntry.ifc.name + "|" + ntry.apiPack);
            }
        }
        return res;
    }

    /**
     * get mpls show
     *
     * @return show
     */
    public userFormat getShowMpls() {
        userFormat res = new userFormat("|", "label|vrf|iface|hop|label|targets|bytes");
        for (int i = 0; i < conn.labels.size(); i++) {
            tabLabelEntry ntry = conn.labels.get(i);
            res.add(ntry.getList());
        }
        return res;
    }

    /**
     * get mpls show
     *
     * @return show
     */
    public userFormat getShowNsh() {
        userFormat res = new userFormat("|", "service");
        for (int i = 0; i < conn.nshs.size(); i++) {
            tabNshEntry ntry = conn.nshs.get(i);
            res.add("" + ntry);
        }
        return res;
    }

    /**
     * get neighbor show
     *
     * @return show
     */
    public userFormat getShowNeighs() {
        userFormat res = new userFormat("|", "neigh|addr|iface");
        for (int i = 0; i < neighs.size(); i++) {
            servP4langNei ntry = neighs.get(i);
            res.add(ntry.id + "|" + ntry.adr + "|" + ntry.iface);
        }
        return res;
    }

    /**
     * get interfaces show
     *
     * @param ifc interface
     * @return show
     */
    public userFormat getShowIface(cfgIfc ifc) {
        userFormat res = new userFormat("|", "category|value");
        servP4langIfc ntry = findIfc(ifc);
        if (ntry == null) {
            res.add("error|interface not exported");
            return res;
        }
        if (ntry.suppressState()) {
            res.add("error|not a physical port");
            return res;
        }
        statsTxt = null;
        statsNtf = new notifier();
        statsPrt = ntry.id;
        sendLine("stats " + ntry.id);
        statsNtf.misleep(5000);
        List<String> txt = statsTxt;
        statsTxt = null;
        statsNtf = null;
        statsPrt = -4;
        if (txt == null) {
            res.add("error|no answer from dataplane");
            return res;
        }
        for (int i = 0; i < txt.size(); i++) {
            res.add(txt.get(i).replaceAll(" ", "|"));
        }
        return res;
    }

    /**
     * get route show
     *
     * @param prt protocol
     * @param vrf vrf
     * @return show
     */
    public tabRoute<addrIP> getShowRou(int prt, int vrf) {
        servP4langVrf ntry = expVrf.find(new servP4langVrf(vrf));
        if (ntry == null) {
            return new tabRoute<addrIP>("empty");
        }
        if (prt == 4) {
            return ntry.routes4;
        } else {
            return ntry.routes6;
        }
    }

    /**
     * send a packet through the api
     *
     * @param cntr counter to use
     * @param ifcn interface to use
     * @param pck packet to send
     * @return true on error false on success
     */
    public boolean send2apiPack(int cntr, int ifcn, packHolder pck) {
        servP4langIfc ifcc = new servP4langIfc(this, ifcn);
        ifcc = expIfc.find(ifcc);
        if (ifcc == null) {
            return true;
        }
        if (cntr < 1) {
            return false;
        }
        if (cntr != 1) {
            logger.error("sending " + cntr + " of packets to " + ifcc.ifc + " payload=" + pck.dataOffset());
            if (!cfgAll.buggy) {
                ifcc.cntr.drop(pck, counter.reasons.badCmd);
                return true;
            }
            if (cfgAll.evalVdcPrivs()) {
                ifcc.cntr.drop(pck, counter.reasons.badCmd);
                return true;
            }
        }
        if (ifcc.apiPack) {
            ifcc.apiSendPack(cntr, pck);
            return false;
        }
        for (int i = 0; i < cntr; i++) {
            ifcc.sendPack(pck.copyBytes(true, true));
        }
        return false;
    }

    /**
     * find frontpanel id
     *
     * @param ifc interface
     * @param num number or name to find
     * @param create allow creation
     * @return id, -1 if error
     */
    protected int front2id(cfgIfc ifc, String num, boolean create) {
        if (num.equals("dynamic")) {
            servP4langIfc res = findIfc(ifc);
            if (res != null) {
                return res.id;
            }
            if (!create) {
                return -1;
            }
            return getNextDynIfc();
        }
        return servP4langUtil.toNum(frontnam, num, -1);
    }

    /**
     * send packet
     *
     * @param id id
     * @param pckB binary
     */
    protected void sendPack(int id, packHolder pckB) {
        cntr.tx(pckB);
        ifcEther.createETHheader(pckB, false);
        if (intercon == null) {
            return;
        }
        pckB.msbPutD(0, id);
        pckB.putSkip(4);
        pckB.merge2beg();
        ifcEther.parseETHheader(pckB, false);
        intercon.sendPack(pckB);
    }

    /**
     * received packet
     *
     * @param pck packet
     */
    public void recvPack(packHolder pck) {
        cntr.rx(pck);
        ifcEther.createETHheader(pck, false);
        int i = pck.msbGetD(0);
        boolean smp = (i & 0x80000000) != 0;
        i &= 0x7fffffff;
        pck.getSkip(4);
        ifcEther.parseETHheader(pck, false);
        servP4langDlnk dlnk = new servP4langDlnk(this, i);
        dlnk = downLinks.find(dlnk);
        if (dlnk != null) {
            ifcEther.createETHheader(pck, false);
            pck.getSkip(-4);
            ifcEther.parseETHheader(pck, false);
            dlnk.parent.sendPack(pck);
            return;
        }
        servP4langIfc ntry = new servP4langIfc(this, i);
        ntry = expIfc.find(ntry);
        if (ntry == null) {
            if ((i == cpuPort) && (pck.msbGetW(0) == ipIfc4arp.type)) {
                if ((pck.msbGetD(2) == magic1) && (pck.msbGetD(6) == magic2)) {
                    cpuLast = bits.getTime();
                }
            }
            if (debugger.servP4langErr) {
                logger.debug("got unneeded target: " + i);
            }
            cntr.drop(pck, counter.reasons.noIface);
            return;
        }
        if (smp) {
            if (ntry.ifc.vrfFor == null) {
                cntr.drop(pck, counter.reasons.notInTab);
                if (debugger.servP4langErr) {
                    logger.debug("got unneeded target: " + i);
                }
                return;
            }
            pck.ETHtype = pck.msbGetW(0);
            pck.getSkip(2);
            ipFwd fwd = null;
            switch (pck.ETHtype) {
                case ipIfc4.type:
                    if (ntry.ifc.vrfFor.core4.parseIPheader(pck, true)) {
                        cntr.drop(pck, counter.reasons.badHdr);
                        return;
                    }
                    pck.getSkip(pck.IPsiz);
                    fwd = ntry.ifc.vrfFor.fwd4;
                    break;
                case ipIfc6.type:
                    if (ntry.ifc.vrfFor.core6.parseIPheader(pck, true)) {
                        cntr.drop(pck, counter.reasons.badHdr);
                        return;
                    }
                    pck.getSkip(pck.IPsiz);
                    fwd = ntry.ifc.vrfFor.fwd6;
                    break;
                default:
                    cntr.drop(pck, counter.reasons.badProto);
                    return;
            }
            if (fwd.netflow == null) {
                if (debugger.servP4langErr) {
                    logger.debug("got unneeded target: " + i);
                }
                cntr.drop(pck, counter.reasons.noIface);
                return;
            }
            tabQos.classifyLayer4(pck);
            pck.getSkip(-pck.IPsiz);
            tabSessionEntry ses = tabSessionEntry.fromPack(pck, fwd.netflow.session.logMacs);
            tabSessionEntry old = fwd.netflow.session.doSess(ses, pck, true);
            if (old == null) {
                return;
            }
            if (old.hwCntr == null) {
                old.hwCntr = new counter();
            }
            old.hwCntr.rx(pck);
            return;
        }
        if (ntry.brif == null) {
            ntry.ifc.ethtyp.gotFromDataplane(pck);
            return;
        }
        if (pck.msbGetW(0) == ifcBridge.serialType) {
            pck.getSkip(2);
        } else {
            pck.getSkip(-addrMac.sizeX2);
        }
        ntry.brif.recvPack(pck);
    }

    /**
     * set parent
     *
     * @param parent parent
     */
    public void setParent(ifcDn parent) {
        intercon = parent;
    }

    /**
     * set state
     *
     * @param stat state
     */
    public void setState(state.states stat) {
    }

    /**
     * close interface
     */
    public void closeUp() {
    }

    /**
     * get counter
     *
     * @return counter
     */
    public counter getCounter() {
        return cntr;
    }

    /**
     * do clear
     */
    public void doClear() {
        conn.pipe.setClose();
    }

    /**
     * send line
     *
     * @param a line
     */
    public synchronized void sendLine(String a) {
        if (debugger.servP4langTx) {
            logger.debug("tx: " + a);
        }
        conn.pipe.linePut(a + " ");
        msgsSent++;
        keyword.update(apiStatTx, a);
    }

    /**
     * get available subif id
     *
     * @return id, -1 if error
     */
    protected synchronized int getNextDynIfc() {
        if (ifcRngEnd < 1) {
            return -1;
        }
        for (int cnt = 0; cnt < 16; cnt++) {
            int nxt = bits.random(ifcRngBeg, ifcRngEnd);
            servP4langIfc ifc = new servP4langIfc(this, nxt);
            if (expIfc.find(ifc) == null) {
                return nxt;
            }
        }
        logger.error("error allocating dynamic interface");
        return -1;
    }

    /**
     * get available subif id
     *
     * @return id, -1 if error
     */
    protected synchronized int getNextDynVrf() {
        if (vrfRngEnd < 1) {
            return -1;
        }
        for (int cnt = 0; cnt < 16; cnt++) {
            int nxt = bits.random(vrfRngBeg, vrfRngEnd);
            servP4langVrf vrf = new servP4langVrf(nxt);
            if (expVrf.find(vrf) == null) {
                return nxt;
            }
        }
        logger.error("error allocating dynamic vrf");
        return -1;
    }

    /**
     * find interface
     *
     * @param ifc interface
     * @return interface, null if error
     */
    protected servP4langIfc findIfc(cfgIfc ifc) {
        if (ifc == null) {
            return null;
        }
        for (int i = 0; i < expIfc.size(); i++) {
            servP4langIfc old = expIfc.get(i);
            if (old.ifc == null) {
                continue;
            }
            if (old.ifc == ifc) {
                return old;
            }
        }
        return null;
    }

    /**
     * find interface
     *
     * @param id id to find
     * @return interface, null if error
     */
    protected servP4langIfc findIfc(int id) {
        servP4langIfc ntry = new servP4langIfc(this, id);
        ntry = expIfc.find(ntry);
        if (ntry == null) {
            return null;
        }
        if (ntry.ifc == null) {
            return null;
        }
        return ntry;
    }

    /**
     * find interface
     *
     * @param ifc interface
     * @return interface, null if error
     */
    protected servP4langIfc findIfc(ifcEthTyp ifc) {
        if (ifc == null) {
            return null;
        }
        for (int i = 0; i < expIfc.size(); i++) {
            servP4langIfc old = expIfc.get(i);
            if (old.ifc == null) {
                continue;
            }
            if (old.ifc.ethtyp == ifc) {
                return old;
            }
        }
        return null;
    }

    /**
     * find interface
     *
     * @param ifc interface
     * @return interface, null if error
     */
    protected servP4langIfc findIfc(tabRouteIface ifc) {
        if (ifc == null) {
            return null;
        }
        for (int i = 0; i < expIfc.size(); i++) {
            servP4langIfc old = expIfc.get(i);
            if (old.ifc == null) {
                continue;
            }
            if (ifc == old.ifc.fwdIf4) {
                return old;
            }
            if (ifc == old.ifc.fwdIf6) {
                return old;
            }
        }
        return null;
    }

    /**
     * find interface
     *
     * @param ifc interface
     * @return interface, null if error
     */
    protected servP4langIfc findIfc(ifcBridgeIfc ifc) {
        if (ifc == null) {
            return null;
        }
        for (int i = 0; i < expIfc.size(); i++) {
            servP4langIfc old = expIfc.get(i);
            if (old.ifc == null) {
                continue;
            }
            if (ifc == old.ifc.bridgeIfc) {
                return old;
            }
        }
        return null;
    }

    /**
     * find interface
     *
     * @param ifc interface
     * @return interface, null if error
     */
    protected servP4langIfc findIfc(cfgBrdg ifc) {
        for (int i = 0; i < expIfc.size(); i++) {
            servP4langIfc old = expIfc.get(i);
            if (old.ifc == null) {
                continue;
            }
            if (old.ifc.bridgeIfc != null) {
                continue;
            }
            if (old.ifc.bridgeHed == ifc) {
                return old;
            }
        }
        return null;
    }

    /**
     * find interface
     *
     * @param ifc interface
     * @return interface, null if error
     */
    protected servP4langIfc findBundl(cfgBndl ifc) {
        for (int i = 0; i < expIfc.size(); i++) {
            servP4langIfc old = expIfc.get(i);
            if (old.ifc == null) {
                continue;
            }
            if (old.ifc.bundleIfc != null) {
                continue;
            }
            if (old.ifc.bundleHed == ifc) {
                return old;
            }
        }
        return null;
    }

    /**
     * find interface
     *
     * @param ifc interface
     * @return interface, null if error
     */
    protected servP4langIfc findDynBr(ifcBridgeIfc ifc) {
        if (ifc == null) {
            return null;
        }
        for (int i = 0; i < expIfc.size(); i++) {
            servP4langIfc old = expIfc.get(i);
            if (old.brif == ifc) {
                return old;
            }
        }
        return null;
    }

    /**
     * find vrf
     *
     * @param fwd forwarder
     * @return vrf, null if error
     */
    protected servP4langVrf findVrf(ipFwd fwd) {
        if (fwd == null) {
            return null;
        }
        for (int i = 0; i < expVrf.size(); i++) {
            servP4langVrf ntry = expVrf.get(i);
            if (fwd == ntry.vrf.fwd4) {
                return ntry;
            }
            if (fwd == ntry.vrf.fwd6) {
                return ntry;
            }
        }
        return null;
    }

    /**
     * find vrf
     *
     * @param ifc interface
     * @return vrf, null if error
     */
    protected servP4langVrf findVrf(servP4langIfc ifc) {
        if (ifc == null) {
            return null;
        }
        for (int i = 0; i < expVrf.size(); i++) {
            servP4langVrf ntry = expVrf.get(i);
            if (ntry.vrf == ifc.ifc.vrfFor) {
                return ntry;
            }
        }
        return null;
    }

    /**
     * generate neighbor id
     *
     * @param ntry neighbor
     * @return updated, null if error
     */
    protected servP4langNei genNeighId(servP4langNei ntry) {
        ntry.need = 1;
        for (int rnd = 0; rnd < 16; rnd++) {
            ntry.id = bits.random(neiRngBeg, neiRngEnd);
            if (ntry.id < 1) {
                continue;
            }
            if (findNei(ntry.id) != null) {
                continue;
            }
            neighs.put(ntry);
            return ntry;
        }
        logger.error("error allocating dynamic neighbor");
        return null;
    }

    /**
     * find neighbor
     *
     * @param id identifier
     * @return neighbor, null if error
     */
    protected servP4langNei findNei(int id) {
        for (int i = 0; i < neighs.size(); i++) {
            servP4langNei ntry = neighs.get(i);
            if (ntry.id == id) {
                return ntry;
            }
        }
        return null;
    }

    /**
     * find neighbor
     *
     * @param ifc interface
     * @param hop nexthop address
     * @return neighbor, null if error
     */
    protected servP4langNei findNei(servP4langIfc ifc, addrIP hop) {
        if (ifc == null) {
            return null;
        }
        servP4langNei ntry = new servP4langNei(ifc, hop);
        servP4langNei old = neighs.find(ntry);
        if (old != null) {
            old.need++;
            return old;
        }
        return genNeighId(ntry);
    }

    /**
     * find neighbor
     *
     * @param ifc interface
     * @param hop nexthop address
     * @return neighbor, null if error
     */
    protected servP4langNei findNei(tabRouteIface ifc, addrIP hop) {
        servP4langIfc i = findIfc(ifc);
        if (i == null) {
            return null;
        }
        servP4langNei ntry = new servP4langNei(i, hop);
        servP4langNei old = neighs.find(ntry);
        if (old != null) {
            old.need++;
            return old;
        }
        return genNeighId(ntry);
    }

    /**
     * find neighbor
     *
     * @param fwd forwarder
     * @param adr nexthop address
     * @return neighbor, null if error
     */
    protected servP4langNei findHop(ipFwd fwd, addrIP adr) {
        tabRouteEntry<addrIP> rou = fwd.actualU.route(adr);
        rou = convRou(rou, false);
        if (rou == null) {
            return null;
        }
        if (rou.best.iface == null) {
            return null;
        }
        addrIP nh = rou.best.nextHop;
        if (nh == null) {
            nh = adr;
        }
        return findNei(rou.best.iface, nh);
    }

    /**
     * convert route
     *
     * @param rou route
     * @param nhchk nexthop check
     * @return converted, null if error
     */
    protected tabRouteEntry<addrIP> convRou(tabRouteEntry<addrIP> rou, boolean nhchk) {
        if (rou == null) {
            return null;
        }
        rou = rou.copyBytes(tabRoute.addType.notyet);
        rou.best.attribAs = 0;
        if (nhchk) {
            if (rou.best.nextHop == null) {
                return rou;
            }
        }
        if (rou.best.iface == null) {
            return rou;
        }
        servP4langIfc ifc = findIfc(rou.best.iface);
        if (ifc == null) {
            return rou;
        }
        if (ifc.ifc.type != tabRouteIface.ifaceType.tunnel) {
            return rou;
        }
        switch (ifc.ifc.tunMode) {
            case teP2p:
                if (ifc.ifc.tunTeP2p == null) {
                    return null;
                }
                rou.best.attribAs = ipMpls.typeU;
                return ifc.ifc.tunTeP2p.getResultRoute(rou);
            case ldpP2p:
                if (ifc.ifc.tunLdpP2p == null) {
                    return null;
                }
                rou.best.attribAs = ipMpls.typeU;
                return ifc.ifc.tunLdpP2p.getResultRoute(rou);
            case polka:
                if (ifc.ifc.tunPolka == null) {
                    return null;
                }
                rou.best.attribAs = ifcPolka.type;
                return ifc.ifc.tunPolka.getResultRoute(rou);
            case mpolka:
                if (ifc.ifc.tunMpolka == null) {
                    return null;
                }
                rou.best.attribAs = ifcPolka.type + 1;
                return ifc.ifc.tunMpolka.getResultRoute(rou);
            default:
                return rou;
        }
    }

}
