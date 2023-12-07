package net.freertr.serv;

import java.util.List;
import net.freertr.addr.addrIP;
import net.freertr.addr.addrMac;
import net.freertr.cfg.cfgAceslst;
import net.freertr.cfg.cfgAll;
import net.freertr.cfg.cfgBndl;
import net.freertr.cfg.cfgBrdg;
import net.freertr.cfg.cfgIfc;
import net.freertr.cfg.cfgPrfxlst;
import net.freertr.cfg.cfgRoump;
import net.freertr.cfg.cfgRouplc;
import net.freertr.cfg.cfgVrf;
import net.freertr.ifc.ifcBridge;
import net.freertr.ifc.ifcBridgeIfc;
import net.freertr.ifc.ifcDn;
import net.freertr.ifc.ifcEthTyp;
import net.freertr.ifc.ifcEther;
import net.freertr.ifc.ifcMpolka;
import net.freertr.ifc.ifcPolka;
import net.freertr.ifc.ifcUp;
import net.freertr.ip.ipFwd;
import net.freertr.ip.ipMpls;
import net.freertr.pack.packHolder;
import net.freertr.pipe.pipeLine;
import net.freertr.tab.tabGen;
import net.freertr.tab.tabLabel;
import net.freertr.tab.tabLabelEntry;
import net.freertr.tab.tabNshEntry;
import net.freertr.tab.tabRoute;
import net.freertr.tab.tabRouteEntry;
import net.freertr.tab.tabRouteIface;
import net.freertr.user.userFormat;
import net.freertr.user.userHelping;
import net.freertr.util.bits;
import net.freertr.util.cmds;
import net.freertr.util.counter;
import net.freertr.util.debugger;
import net.freertr.util.history;
import net.freertr.util.logger;
import net.freertr.util.notifier;
import net.freertr.spf.spfCalc;
import net.freertr.util.state;

/**
 * one p4lang configuration
 *
 * @author matecsaba
 */
public class servP4langCfg implements ifcUp {

    /**
     * parent
     */
    protected final servP4lang parent;

    /**
     * forwarder number
     */
    protected final int id;

    /**
     * current connection
     */
    protected servP4langConn conn;

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
     * description
     */
    protected String descr;

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
     * backplane interfaces
     */
    protected tabGen<servP4langBkpl> backPlanes = new tabGen<servP4langBkpl>();

    /**
     * backplane spf
     */
    protected spfCalc<addrIP> bckplnSpf;

    /**
     * backplane routes
     */
    protected tabRoute<addrIP> bckplnRou;

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
    protected tabGen<servP4langMsg> apiStatTx;

    /**
     * received message statistics
     */
    protected tabGen<servP4langMsg> apiStatRx;

    /**
     * interconnect interface
     */
    protected ifcDn intercon;

    /**
     * create instance
     *
     * @param lower parent
     * @param i id
     */
    protected servP4langCfg(servP4lang lower, int i) {
        parent = lower;
        id = i;
        pipeLine pl = new pipeLine(1024, false);
        pl.setClose();
        conn = new servP4langConn(pl.getSide(), this);
    }

    public String toString() {
        return "p4lang forwarder " + id;
    }

    /**
     * get hardware forwarder info
     *
     * @return offload info
     */
    public String getShGenOneLiner() {
        String a = platform + ",cpu=" + intercon + ",brd=" + expBr.size() + ",vrf=" + expVrf.size() + ",prt=" + expIfc.size();
        if (conn == null) {
            return a;
        }
        a += conn.getShGenOneLiner();
        if (backPlanes.size() < 1) {
            return a;
        }
        a += ",bcks=";
        for (int i = 0; i < backPlanes.size(); i++) {
            servP4langBkpl cur = backPlanes.get(i);
            a += cur.getShGenOneLiner();
        }
        return a.substring(0, a.length() - 1);
    }

    /**
     * setup for api packet usage
     *
     * @param prnt parent to check against
     */
    protected void setup2apiPack(servP4langIfc prnt) {
        if (prnt == null) {
            return;
        }
        if (prnt.ifc == null) {
            return;
        }
        boolean ned = false;
        if (prnt.speed != null) {
            ned = prnt.spdNum == -2;
        }
        prnt.apiPack = ned;
        prnt.setup2apiPack(ned);
        for (int i = expIfc.size() - 1; i >= 0; i--) {
            servP4langIfc ntry = expIfc.get(i);
            if (ntry == null) {
                continue;
            }
            if (ntry.ifc == null) {
                continue;
            }
            if (ntry.ifc.parent == prnt.ifc) {
                ntry.setup2apiPack(ned);
                continue;
            }
            if (ntry.ifc.cloned == prnt.ifc) {
                ntry.setup2apiPack(ned);
                continue;
            }
        }
    }

    /**
     * get configuration
     *
     * @param beg text to prepend
     * @param mid text to prepend
     * @param l text to append
     */
    protected void getShowRun(String beg, String mid, List<String> l) {
        cmds.cfgLine(l, !expMgc, beg, mid + "export-names", "");
        cmds.cfgLine(l, apiStatTx == null, beg, mid + "api-stat", "");
        for (int i = 0; i < expVrf.size(); i++) {
            servP4langVrf ntry = expVrf.get(i);
            l.add(beg + mid + "export-vrf " + ntry.vrf.name);
            cmds.cfgLine(l, ntry.conn4c == null, beg, mid + "export-copp4 " + ntry.vrf.name, "" + ntry.conn4c);
            cmds.cfgLine(l, ntry.copp6c == null, beg, mid + "export-copp6 " + ntry.vrf.name, "" + ntry.copp6c);
            cmds.cfgLine(l, ntry.prflst4 == null, beg, mid + "filter-list4 " + ntry.vrf.name, "" + ntry.prflst4);
            cmds.cfgLine(l, ntry.prflst6 == null, beg, mid + "filter-list6 " + ntry.vrf.name, "" + ntry.prflst6);
            cmds.cfgLine(l, ntry.roumap4 == null, beg, mid + "filter-map4 " + ntry.vrf.name, "" + ntry.roumap4);
            cmds.cfgLine(l, ntry.roumap6 == null, beg, mid + "filter-map6 " + ntry.vrf.name, "" + ntry.roumap6);
            cmds.cfgLine(l, ntry.roupol4 == null, beg, mid + "filter-policy4 " + ntry.vrf.name, "" + ntry.roupol4);
            cmds.cfgLine(l, ntry.roupol6 == null, beg, mid + "filter-policy6 " + ntry.vrf.name, "" + ntry.roupol6);
            cmds.cfgLine(l, !ntry.compress4, beg, mid + "filter-compress4 " + ntry.vrf.name, "");
            cmds.cfgLine(l, !ntry.compress6, beg, mid + "filter-compress6 " + ntry.vrf.name, "");
        }
        for (int i = 0; i < expBr.size(); i++) {
            servP4langBr ntry = expBr.get(i);
            l.add(beg + mid + "export-bridge " + ntry.br.number);
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
            l.add(beg + mid + "export-port " + ntry.ifc.name + " " + a);
        }
        if (expSrv6 == null) {
            l.add(beg + "no " + mid + "export-srv6");
        } else {
            l.add(beg + mid + "export-srv6 " + expSrv6.name);
        }
        cmds.cfgLine(l, !expSck, beg, mid + "export-socket", "");
        l.add(beg + mid + "export-interval " + expDelay);
        cmds.cfgLine(l, interconn == null, beg, mid + "interconnect", "" + interconn);
        for (int i = 0; i < downLinks.size(); i++) {
            servP4langDlnk ntry = downLinks.get(i);
            l.add(beg + mid + "downlink " + ntry.id + " " + ntry.ifc);
        }
    }

    /**
     * get configuration
     *
     * @param beg text to prepend
     * @param mid text to prepend
     * @param l text to append
     */
    protected void getShowRun2(String beg, String mid, List<String> l) {
        for (int i = 0; i < backPlanes.size(); i++) {
            servP4langBkpl ntry = backPlanes.get(i);
            l.add(beg + mid + "backplane " + ntry.pi.ifc.name + " " + ntry.metric);
        }
        l.add(beg + mid + "remote " + remote);
        cmds.cfgLine(l, descr == null, beg, mid + "name", descr);
    }

    /**
     * do configuration work
     *
     * @param s command
     * @param cmd parameters
     * @return false if success, true on error
     */
    protected boolean doConfig(String s, cmds cmd) {
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
            ntry.lab = tabLabel.allocate(tabLabelEntry.owner.p4langBr);
            ntry.lab.setFwdDrop(tabLabelEntry.owner.p4langBr);
            expBr.put(ntry);
            return false;
        }
        if (s.equals("name")) {
            descr = cmd.getRemaining();
            return false;
        }
        if (s.equals("remote")) {
            addrIP adr = new addrIP();
            if (adr.fromString(cmd.word())) {
                cmd.error("bad address");
                return false;
            }
            if (adr.compare(adr, remote) == 0) {
                return false;
            }
            remote = adr;
            doClear();
            return false;
        }
        if (s.equals("backplane")) {
            cfgIfc rif = cfgAll.ifcFind(cmd.word(), 0);
            if (rif == null) {
                cmd.error("no such interface");
                return false;
            }
            servP4langIfc pif = findIfc(rif.ethtyp);
            if (pif == null) {
                if (rif.vlanNum == 0) {
                    cmd.error("port not exported");
                    return false;
                }
                pif = conn.doSubif(rif);
                if (pif == null) {
                    cmd.error("parent not exported");
                    return false;
                }
            }
            servP4langBkpl ntry = new servP4langBkpl(this, pif);
            ntry.ifc = pif.ifc.ethtyp;
            ntry.ifc.addET(-1, "p4lang", ntry);
            ntry.ifc.updateET(-1, ntry);
            ntry.parent.setFilter(true);
            ntry.metric = bits.str2num(cmd.word());
            backPlanes.add(ntry);
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
            pv.conn4c = acl.aceslst;
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
            apiStatTx = new tabGen<servP4langMsg>();
            apiStatRx = new tabGen<servP4langMsg>();
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
            if ((ifc.type != tabRouteIface.ifaceType.sdn) && (ifc.type != tabRouteIface.ifaceType.bundle) && (ifc.type != tabRouteIface.ifaceType.bridge) && (ifc.type != tabRouteIface.ifaceType.dialer) && (ifc.type != tabRouteIface.ifaceType.hairpin) && (ifc.type != tabRouteIface.ifaceType.tunnel) && (ifc.type != tabRouteIface.ifaceType.virtppp) && (ifc.type != tabRouteIface.ifaceType.template)) {
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
            setup2apiPack(ntry);
            return false;
        }
        return true;
    }

    /**
     * do negated configuration
     *
     * @param s command
     * @param cmd parameters
     * @return false if success, true on error
     */
    protected boolean doUnConfig(String s, cmds cmd) {
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
        if (s.equals("name")) {
            descr = null;
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
            pv.conn4c = null;
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
            tabLabel.release(ntry.lab, tabLabelEntry.owner.p4langBr);
            return false;
        }
        if (s.equals("backplane")) {
            cfgIfc rif = cfgAll.ifcFind(cmd.word(), 0);
            if (rif == null) {
                cmd.error("no such interface");
                return false;
            }
            servP4langIfc pif = findIfc(rif.ethtyp);
            if (pif == null) {
                cmd.error("port not exported");
                return false;
            }
            servP4langBkpl ntry = new servP4langBkpl(this, pif);
            ntry = backPlanes.del(ntry);
            if (ntry == null) {
                cmd.error("no such downlink");
                return false;
            }
            ntry.ifc.delET(-1);
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
     * get help text
     *
     * @param l where to place
     * @param p starting level
     */
    protected void getHelpText(userHelping l, int p) {
        List<String> lst = servP4langUtil.toHelp(frontnam);
        l.add(null, (p + 0) + " " + (p + 1) + "  remote                    address of forwarder");
        l.add(null, (p + 1) + " .    <addr>                  ip address of client");
        l.add(null, (p + 0) + " " + (p + 1) + "  name                      name of forwarder");
        l.add(null, (p + 1) + " " + (p + 1) + ",.  <str>                   description of forwarders");
        l.add(null, (p + 0) + " " + (p + 1) + "  export-vrf                specify vrf to export");
        l.add(null, (p + 1) + " " + (p + 2) + ",.  <name:vrf>              vrf name");
        l.add(null, (p + 2) + " .      <num>                 p4lang vrf number");
        l.add(null, (p + 0) + " " + (p + 1) + "  export-bridge             specify bridge to export");
        l.add(null, (p + 1) + " .    <num>                   bridge number");
        l.add(null, (p + 0) + " " + (p + 1) + "  export-port               specify port to export");
        l.add(null, (p + 1) + " " + (p + 2) + "    <name:ifc>              interface name");
        l.add(null, (p + 2) + " " + (p + 3) + ",.    dynamic               dynamic port number");
        l.add(lst, (p + 2) + " " + (p + 3) + ",.    <num:loc>             port number");
        l.add(null, (p + 3) + " " + (p + 4) + ",.      <num>               speed in gbps, see hw vendor manual");
        l.add(servP4langUtil.toHelp(fwderrcr), (p + 4) + " " + (p + 5) + ",.        <num:loc>         fec, see hw vendor manual");
        l.add(servP4langUtil.toHelp(autonegs), (p + 5) + " " + (p + 6) + ",.          <num:loc>       autoneg, see hw vendor manual");
        l.add(servP4langUtil.toHelp(flwctrls), (p + 6) + " .              <num:loc>     flowctrl, see hw vendor manual");
        l.add(null, (p + 0) + " " + (p + 1) + "  export-srv6               specify srv6 to export");
        l.add(null, (p + 1) + " .    <name:ifc>              interface name");
        l.add(null, (p + 0) + " .  export-socket             specify sockets to be exported");
        l.add(null, (p + 0) + " " + (p + 1) + "  filter-compress4          enable compressed fib export");
        l.add(null, (p + 1) + " .    <name:vrf>              vrf name");
        l.add(null, (p + 0) + " " + (p + 1) + "  filter-compress6          enable compressed fib export");
        l.add(null, (p + 1) + " .    <name:vrf>              vrf name");
        l.add(null, (p + 0) + " .  export-names              specify names to be exported");
        l.add(null, (p + 0) + " .  api-stat                  count the sent api messages");
        l.add(null, (p + 0) + " " + (p + 1) + "  export-copp4              specify copp acl to export");
        l.add(null, (p + 1) + " " + (p + 2) + "    <name:vrf>              vrf name");
        l.add(null, (p + 2) + " .      <name:acl>              acl name");
        l.add(null, (p + 0) + " " + (p + 1) + "  export-copp6              specify copp acl to export");
        l.add(null, (p + 1) + " " + (p + 2) + "    <name:vrf>              vrf name");
        l.add(null, (p + 2) + " .      <name:acl>              acl name");
        l.add(null, (p + 0) + " " + (p + 1) + "  filter-list4              specify prefixes to export");
        l.add(null, (p + 1) + " " + (p + 2) + "    <name:vrf>              vrf name");
        l.add(null, (p + 2) + " .      <name:pl>             prefix list name");
        l.add(null, (p + 0) + " " + (p + 1) + "  filter-list6              specify prefixes to export");
        l.add(null, (p + 1) + " " + (p + 2) + "    <name:vrf>              vrf name");
        l.add(null, (p + 2) + " .      <name:pl>             prefix list name");
        l.add(null, (p + 0) + " " + (p + 1) + "  filter-map4               specify prefixes to export");
        l.add(null, (p + 1) + " " + (p + 2) + "    <name:vrf>              vrf name");
        l.add(null, (p + 2) + " .      <name:rm>             route map name");
        l.add(null, (p + 0) + " " + (p + 1) + "  filter-map6               specify prefixes to export");
        l.add(null, (p + 1) + " " + (p + 2) + "    <name:vrf>              vrf name");
        l.add(null, (p + 2) + " .      <name:rm>             route map name");
        l.add(null, (p + 0) + " " + (p + 1) + "  filter-policy4            specify prefixes to export");
        l.add(null, (p + 1) + " " + (p + 2) + "    <name:vrf>              vrf name");
        l.add(null, (p + 2) + " .      <name:rpl>            route policy name");
        l.add(null, (p + 0) + " " + (p + 1) + "  filter-policy6            specify prefixes to export");
        l.add(null, (p + 1) + " " + (p + 2) + "    <name:vrf>              vrf name");
        l.add(null, (p + 2) + " .      <name:rpl>            route policy name");
        l.add(null, (p + 0) + " " + (p + 1) + "  export-interval           specify export interval");
        l.add(null, (p + 1) + " .    <num>                   time in ms");
        l.add(null, (p + 0) + " " + (p + 1) + "  downlink                  specify downlink for packetin");
        l.add(lst, (p + 1) + " " + (p + 2) + "    <num:loc>               port number");
        l.add(null, (p + 2) + " .      <name:ifc>            interface name");
        l.add(null, (p + 0) + " " + (p + 1) + "  backplane                 specify backplane connection");
        l.add(null, (p + 1) + " " + (p + 2) + "    <name:ifc>              interface name");
        l.add(null, (p + 2) + " .      <num>                 metric");
        l.add(null, (p + 0) + " " + (p + 1) + "  interconnect              specify port to for packetin");
        l.add(null, (p + 1) + " .    <name:ifc>              interface name");
    }

    /**
     * get generic show
     *
     * @return show
     */
    protected userFormat getShowGen() {
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
        res.add("dynamic ifc|" + ifcRngBeg + " " + ifcRngEnd);
        res.add("dynamic vrf|" + vrfRngBeg + " " + vrfRngEnd);
        res.add("messages sent|" + msgsSent);
        res.add("messages got|" + msgsGot);
        res.add("rounds done|" + rndDoneNum);
        res.add("last done|" + bits.time2str(cfgAll.timeZoneName, rndDoneLast + cfgAll.timeServerOffset, 3) + " (" + bits.timePast(rndDoneLast) + " ago)");
        res.add("time took|" + rndDoneTime);
        res.add("rounds skip|" + rndSkipNum);
        res.add("last skip|" + bits.time2str(cfgAll.timeZoneName, rndSkipLast + cfgAll.timeServerOffset, 3) + " (" + bits.timePast(rndSkipLast) + " ago)");
        res.add("backroutes|" + bckplnRou.size());
        res.add("interfaces|" + expIfc.size());
        res.add("bridges|" + expBr.size());
        res.add("vrfs|" + expVrf.size());
        return res;
    }

    /**
     * get frontpanel show
     *
     * @return show
     */
    protected userFormat getShowFront() {
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
    protected userFormat getShowMagics() {
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
    protected userFormat getShowVrfs() {
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
    protected userFormat getShowIfaces() {
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
    protected userFormat getShowMpls() {
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
    protected userFormat getShowNsh() {
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
    protected userFormat getShowNeighs() {
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
    protected userFormat getShowIface(cfgIfc ifc) {
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
        pckB.msbPutW(0, id);
        pckB.putSkip(2);
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
        int i = pck.msbGetW(0);
        pck.getSkip(2);
        ifcEther.parseETHheader(pck, false);
        servP4langDlnk dlnk = new servP4langDlnk(this, i);
        dlnk = downLinks.find(dlnk);
        if (dlnk != null) {
            ifcEther.createETHheader(pck, false);
            pck.getSkip(-2);
            ifcEther.parseETHheader(pck, false);
            dlnk.parent.sendPack(pck);
            return;
        }
        servP4langIfc ntry = new servP4langIfc(this, i);
        ntry = expIfc.find(ntry);
        if (ntry == null) {
            if (debugger.servP4langErr) {
                logger.debug("got unneeded target: " + i);
            }
            cntr.drop(pck, counter.reasons.noIface);
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
    protected void doClear() {
        conn.pipe.setClose();
    }

    /**
     * send line
     *
     * @param a line
     */
    protected synchronized void sendLine(String a) {
        if (debugger.servP4langTx) {
            logger.debug("fwd" + id + " tx: " + a);
        }
        conn.pipe.linePut(a + " ");
        msgsSent++;
        if (apiStatTx == null) {
            return;
        }
        int i = a.indexOf(" ");
        if (i > 0) {
            a = a.substring(0, i);
        }
        servP4langUtil.updateApiStats(apiStatTx, a);
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
            ntry.id = bits.random(0x1000, 0xfff0);
            if (ntry.id < 1) {
                continue;
            }
            boolean fnd = false;
            for (int i = 0; i < neighs.size(); i++) {
                if (neighs.get(i).id != ntry.id) {
                    continue;
                }
                fnd = true;
                break;
            }
            if (fnd) {
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
            case srMpls:
                if (ifc.ifc.tunSrMpls == null) {
                    return null;
                }
                rou.best.attribAs = ipMpls.typeU;
                return ifc.ifc.tunSrMpls.getResultRoute(rou);
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
                rou.best.attribAs = ifcMpolka.type;
                return ifc.ifc.tunMpolka.getResultRoute(rou);
            default:
                return rou;
        }
    }

}
