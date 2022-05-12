package net.freertr.serv;

import java.util.ArrayList;
import java.util.List;
import net.freertr.addr.addrIP;
import net.freertr.addr.addrMac;
import net.freertr.cfg.cfgAceslst;
import net.freertr.cfg.cfgAll;
import net.freertr.cfg.cfgBndl;
import net.freertr.cfg.cfgBrdg;
import net.freertr.cfg.cfgIfc;
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
import net.freertr.tab.tabAceslstN;
import net.freertr.tab.tabGen;
import net.freertr.tab.tabListing;
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
import net.freertr.util.state;

/**
 * one p4lang configuration
 *
 * @author matecsaba
 */
public class servP4langCfg implements ifcUp {

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
     * exported copp
     */
    protected tabListing<tabAceslstN<addrIP>, addrIP> expCopp4 = null;

    /**
     * exported copp
     */
    protected tabListing<tabAceslstN<addrIP>, addrIP> expCopp6 = null;

    /**
     * export interval
     */
    protected int expDelay = 1000;

    /**
     * last peer
     */
    protected addrIP remote = null;

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
    protected int dynRngBeg = 0x10000;

    /**
     * last dynamic range
     */
    protected int dynRngEnd = 0x20000;

    /**
     * last front panel
     */
    protected tabGen<servP4langFrnt> fronts = new tabGen<servP4langFrnt>();

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
     */
    protected servP4langCfg() {
        pipeLine pl = new pipeLine(1024, false);
        pl.setClose();
        conn = new servP4langConn(pl.getSide(), this);
    }

    /**
     * get configuration
     *
     * @param beg text to prepend
     * @param l text to append
     */
    protected void getShowRun(String beg, List<String> l) {
        cmds.cfgLine(l, apiStatTx == null, beg, "api-stat", "");
        for (int i = 0; i < expVrf.size(); i++) {
            servP4langVrf ntry = expVrf.get(i);
            l.add(beg + "export-vrf " + ntry.vrf.name + " " + ntry.id);
        }
        for (int i = 0; i < expBr.size(); i++) {
            servP4langBr ntry = expBr.get(i);
            l.add(beg + "export-bridge " + ntry.br.num);
        }
        for (int i = 0; i < expIfc.size(); i++) {
            servP4langIfc ntry = expIfc.get(i);
            if (ntry.hidden) {
                continue;
            }
            String a = ntry.getCfgLine();
            l.add(beg + "export-port " + a);
        }
        if (expSrv6 == null) {
            l.add(beg + "no export-srv6");
        } else {
            l.add(beg + "export-srv6 " + expSrv6.name);
        }
        if (expCopp4 == null) {
            l.add(beg + "no export-copp4");
        } else {
            l.add(beg + "export-copp4 " + expCopp4.listName);
        }
        if (expCopp6 == null) {
            l.add(beg + "no export-copp6");
        } else {
            l.add(beg + "export-copp6 " + expCopp6.listName);
        }
        cmds.cfgLine(l, !expSck, beg, "export-socket", "");
        l.add(beg + "export-interval " + expDelay);
        cmds.cfgLine(l, interconn == null, beg, "interconnect", "" + interconn);
        for (int i = 0; i < downLinks.size(); i++) {
            servP4langDlnk ntry = downLinks.get(i);
            l.add(beg + "downlink " + ntry.id + " " + ntry.ifc);
        }
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
            servP4langVrf ntry = new servP4langVrf(bits.str2num(cmd.word()));
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
            servP4langBr ntry = new servP4langBr(br.num);
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
            cfgIfc ifc = cfgAll.ifcFind(cmd.word(), false);
            if (ifc == null) {
                cmd.error("no such interface");
                return false;
            }
            ntry.ifc = ifc.ethtyp;
            ntry.ifc.addET(-1, "p4lang", ntry);
            ntry.ifc.updateET(-1, ntry);
            downLinks.add(ntry);
            return false;
        }
        if (s.equals("interconnect")) {
            cfgIfc ifc = cfgAll.ifcFind(cmd.word(), false);
            if (ifc == null) {
                cmd.error("no such interface");
                return false;
            }
            interconn = ifc.ethtyp;
            interconn.addET(-1, "p4lang", this);
            interconn.updateET(-1, this);
            return false;
        }
        if (s.equals("export-copp4")) {
            cfgAceslst acl = cfgAll.aclsFind(cmd.word(), false);
            if (acl == null) {
                cmd.error("no such access list");
                return false;
            }
            expCopp4 = acl.aceslst;
            return false;
        }
        if (s.equals("export-copp6")) {
            cfgAceslst acl = cfgAll.aclsFind(cmd.word(), false);
            if (acl == null) {
                cmd.error("no such access list");
                return false;
            }
            expCopp6 = acl.aceslst;
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
        if (s.equals("export-interval")) {
            expDelay = bits.str2num(cmd.word());
            return false;
        }
        if (s.equals("export-srv6")) {
            expSrv6 = cfgAll.ifcFind(cmd.word(), false);
            if (expSrv6 == null) {
                cmd.error("no such interface");
                return false;
            }
            return false;
        }
        if (s.equals("export-port")) {
            cfgIfc ifc = cfgAll.ifcFind(cmd.word(), false);
            if (ifc == null) {
                cmd.error("no such interface");
                return false;
            }
            if ((ifc.type != cfgIfc.ifaceType.sdn) && (ifc.type != cfgIfc.ifaceType.bundle) && (ifc.type != cfgIfc.ifaceType.bridge) && (ifc.type != cfgIfc.ifaceType.dialer) && (ifc.type != cfgIfc.ifaceType.hairpin) && (ifc.type != cfgIfc.ifaceType.tunnel) && (ifc.type != cfgIfc.ifaceType.virtppp) && (ifc.type != cfgIfc.ifaceType.template)) {
                cmd.error("not p4lang interface");
                return false;
            }
            int i = front2id(ifc, cmd.word(), true);
            if (i < 0) {
                cmd.error("no such frontpanel port");
                return false;
            }
            servP4langIfc ntry = new servP4langIfc(this, i);
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
            ntry.speed = bits.str2num(cmd.word());
            ntry.errCorr = bits.str2num(cmd.word());
            ntry.autoNeg = bits.str2num(cmd.word());
            ntry.flowCtrl = bits.str2num(cmd.word());
            boolean need = ifc.type == cfgIfc.ifaceType.sdn;
            if (ifc.vlanNum != 0) {
                cmd.error("no need to export subinterface");
                return false;
            }
            ntry.dynamic = !need;
            if (ntry.speed == -1) {
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
                int id = getNextDynamic();
                if (id < 0) {
                    return false;
                }
                ntry.id = id;
            }
            ifc.ethtyp.hwHstry = new history();
            ifc.ethtyp.hwCntr = new counter();
            expIfc.put(ntry);
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
            cmd.word();
            servP4langVrf ntry = new servP4langVrf(bits.str2num(cmd.word()));
            ntry = expVrf.del(ntry);
            if (ntry == null) {
                cmd.error("no such export");
                return false;
            }
            ntry.vrf.fwd4.tableChanged = null;
            ntry.vrf.fwd6.tableChanged = null;
            return false;
        }
        if (s.equals("export-copp4")) {
            expCopp4 = null;
            return false;
        }
        if (s.equals("export-copp6")) {
            expCopp6 = null;
            return false;
        }
        if (s.equals("api-stat")) {
            apiStatTx = null;
            apiStatRx = null;
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
            cfgIfc ifc = cfgAll.ifcFind(cmd.word(), false);
            int i = front2id(ifc, cmd.word(), false);
            if (i < 0) {
                cmd.error("no such frontpanel port");
                return false;
            }
            servP4langIfc ntry = new servP4langIfc(this, i);
            ntry = expIfc.del(ntry);
            if (ntry == null) {
                cmd.error("no such export");
                return false;
            }
            ntry.tearDown();
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
        int i = bits.str2num(num);
        if (num.equals("" + i)) {
            return i;
        }
        if (num.equals("dynamic")) {
            servP4langIfc res = findIfc(ifc);
            if (res != null) {
                return res.id;
            }
            if (!create) {
                return -1;
            }
            return getNextDynamic();
        }
        servP4langFrnt ntry = new servP4langFrnt(-1, num);
        ntry = fronts.find(ntry);
        if (ntry == null) {
            return -1;
        }
        return ntry.id;
    }

    /**
     * get help text
     *
     * @param l where to place
     * @param p starting level
     */
    protected void getHelpText(userHelping l, int p) {
        List<String> lst = new ArrayList<String>();
        for (int i = 0; i < fronts.size(); i++) {
            lst.add(fronts.get(i).nam);
        }
        l.add(null, (p + 0) + " " + (p + 1) + "  export-vrf                specify vrf to export");
        l.add(null, (p + 1) + " " + (p + 2) + "    <name:vrf>              vrf name");
        l.add(null, (p + 2) + " .      <num>                 p4lang vrf number");
        l.add(null, (p + 0) + " " + (p + 1) + "  export-bridge             specify bridge to export");
        l.add(null, (p + 1) + " .    <num>                   bridge number");
        l.add(null, (p + 0) + " " + (p + 1) + "  export-port               specify port to export");
        l.add(null, (p + 1) + " " + (p + 2) + "    <name:ifc>              interface name");
        l.add(null, (p + 2) + " " + (p + 3) + ",.    dynamic               dynamic port number");
        l.add(lst, (p + 2) + " " + (p + 3) + ",.    <num:loc>             port number");
        l.add(null, (p + 3) + " " + (p + 4) + ",.      <num>               speed in gbps");
        l.add(null, (p + 4) + " " + (p + 5) + ",.        <num>             fec, see hw vendor manual");
        l.add(null, (p + 5) + " " + (p + 6) + ",.          <num>           autoneg, see hw vendor manual");
        l.add(null, (p + 6) + " .              <num>         flowctrl, see hw vendor manual");
        l.add(null, (p + 0) + " " + (p + 1) + "  export-srv6               specify srv6 to export");
        l.add(null, (p + 1) + " .    <name:ifc>              interface name");
        l.add(null, (p + 0) + " .  export-socket             specify sockets to be exported");
        l.add(null, (p + 0) + " .  api-stat                  count the sent api messages");
        l.add(null, (p + 0) + " " + (p + 1) + "  export-copp4              specify copp acl to export");
        l.add(null, (p + 1) + " .    <name:acl>              acl name");
        l.add(null, (p + 0) + " " + (p + 1) + "  export-copp6              specify copp acl to export");
        l.add(null, (p + 1) + " .    <name:acl>              acl name");
        l.add(null, (p + 0) + " " + (p + 1) + "  export-interval           specify export interval");
        l.add(null, (p + 1) + " .    <num>                   time in ms");
        l.add(null, (p + 0) + " " + (p + 1) + "  downlink                  specify downlink for packetin");
        l.add(null, (p + 1) + " " + (p + 2) + "    <num:loc>               port number");
        l.add(null, (p + 2) + " .      <name:ifc>            interface name");
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
        res.add("reconn|" + reconns);
        res.add("since|" + bits.time2str(cfgAll.timeZoneName, started + cfgAll.timeServerOffset, 3));
        res.add("for|" + bits.timePast(started));
        res.add("capability|" + capability);
        res.add("platform|" + platform);
        res.add("cpuport|" + cpuPort);
        res.add("dynamicid|" + dynRngBeg + " " + dynRngEnd);
        res.add("messages sent|" + msgsSent);
        res.add("messages got|" + msgsGot);
        res.add("rounds done|" + rndDoneNum);
        res.add("last done|" + bits.time2str(cfgAll.timeZoneName, rndDoneLast + cfgAll.timeServerOffset, 3) + " (" + bits.timePast(rndDoneLast) + " ago)");
        res.add("time took|" + rndDoneTime);
        res.add("rounds skip|" + rndSkipNum);
        res.add("last skip|" + bits.time2str(cfgAll.timeZoneName, rndSkipLast + cfgAll.timeServerOffset, 3) + " (" + bits.timePast(rndSkipLast) + " ago)");
        return res;
    }

    /**
     * get frontpanel show
     *
     * @return show
     */
    protected userFormat getShowFront() {
        userFormat res = new userFormat("|", "front|name");
        for (int i = 0; i < fronts.size(); i++) {
            servP4langFrnt ntry = fronts.get(i);
            res.add(ntry.id + "|" + ntry.nam);
        }
        return res;
    }

    /**
     * get interfaces show
     *
     * @return show
     */
    protected userFormat getShowIfaces() {
        userFormat res = new userFormat("|", "sent|name");
        for (int i = 0; i < expIfc.size(); i++) {
            servP4langIfc ntry = expIfc.get(i);
            if (ntry.ifc == null) {
                res.add(ntry.id + "|brif " + ntry.brif.getIfcName());
            } else {
                res.add(ntry.id + "|ifc " + ntry.ifc.name);
            }
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
        int id = pck.msbGetW(0);
        pck.getSkip(2);
        ifcEther.parseETHheader(pck, false);
        servP4langDlnk dlnk = new servP4langDlnk(this, id);
        dlnk = downLinks.find(dlnk);
        if (dlnk != null) {
            ifcEther.createETHheader(pck, false);
            pck.getSkip(-2);
            ifcEther.parseETHheader(pck, false);
            dlnk.parent.sendPack(pck);
            return;
        }
        servP4langIfc ntry = new servP4langIfc(this, id);
        ntry = expIfc.find(ntry);
        if (ntry == null) {
            if (debugger.servP4langErr) {
                logger.debug("got unneeded target: " + id);
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
            logger.debug("tx: " + a);
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
    protected synchronized int getNextDynamic() {
        for (int cnt = 0; cnt < 16; cnt++) {
            int dynRngNxt = bits.random(dynRngBeg, dynRngEnd);
            servP4langIfc ifc = new servP4langIfc(this, dynRngNxt);
            if (expIfc.find(ifc) == null) {
                return dynRngNxt;
            }
        }
        logger.error("error allocating dynamic interface");
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
    protected servP4langNei findNei(tabRouteIface ifc, addrIP hop) {
        servP4langIfc id = findIfc(ifc);
        if (id == null) {
            return null;
        }
        servP4langNei ntry = new servP4langNei(id, hop);
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
        if (ifc.ifc.type != cfgIfc.ifaceType.tunnel) {
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
