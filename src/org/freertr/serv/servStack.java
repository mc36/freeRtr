package org.freertr.serv;

import java.util.ArrayList;
import java.util.List;
import org.freertr.addr.addrIP;
import org.freertr.addr.addrPrefix;
import org.freertr.cfg.cfgAll;
import org.freertr.cfg.cfgBrdg;
import org.freertr.cfg.cfgIfc;
import org.freertr.ifc.ifcBridgeIfc;
import org.freertr.ifc.ifcEthTyp;
import org.freertr.ip.ipFwdIface;
import org.freertr.pack.packHolder;
import org.freertr.pipe.pipeLine;
import org.freertr.pipe.pipeSide;
import org.freertr.prt.prtGenConn;
import org.freertr.prt.prtServS;
import org.freertr.spf.spfCalc;
import org.freertr.tab.tabGen;
import org.freertr.tab.tabLabel;
import org.freertr.tab.tabLabelBierN;
import org.freertr.tab.tabLabelEntry;
import org.freertr.tab.tabRoute;
import org.freertr.tab.tabRouteEntry;
import org.freertr.tab.tabRouteIface;
import org.freertr.user.userFilter;
import org.freertr.user.userFormat;
import org.freertr.user.userHelp;
import org.freertr.util.bits;
import org.freertr.util.cmds;
import org.freertr.util.logger;
import org.freertr.util.state;

/**
 * stack handler
 *
 * @author matecsaba
 */
public class servStack extends servGeneric implements prtServS, servGenFwdr {

    /**
     * create instance
     */
    public servStack() {
        fwds = new ArrayList<servStackFwd>();
        bckplnLab = new tabLabelEntry[0];
        dscvry = new servStackDisc(this);
    }

    /**
     * convert forwarder id to address
     *
     * @param i id
     * @return address
     */
    protected final static addrIP forwarder2addr(int i) {
        addrIP adr = new addrIP();
        byte[] buf = adr.getBytes();
        bits.msbPutD(buf, 0, -25162835);
        bits.msbPutD(buf, buf.length - 4, i + 1);
        return adr;
    }

    /**
     * convert address to forwarder id
     *
     * @param adr address
     * @return id
     */
    protected final static int addr2forwarder(addrIP adr) {
        byte[] buf = adr.getBytes();
        return bits.msbGetD(buf, buf.length - 4) - 1;
    }

    /**
     * convert forwarder id to address
     *
     * @param i id
     * @return address
     */
    protected final static tabRouteEntry<addrIP> forwarder2route(int i) {
        addrIP adr = forwarder2addr(i);
        tabRouteEntry<addrIP> rou = new tabRouteEntry<addrIP>();
        rou.prefix = new addrPrefix<addrIP>(adr, addrIP.size * 8);
        return rou;
    }

    /**
     * port
     */
    public final static int port = 9080;

    /**
     * discovery timer
     */
    protected servStackDisc dscvry;

    /**
     * buffer size
     */
    protected int bufSiz = 65536;

    /**
     * discovery interval
     */
    protected int discoInt = 1000;

    /**
     * discovery timeout
     */
    protected int discoTim = 5000;

    /**
     * forwarders
     */
    protected final List<servStackFwd> fwds;

    /**
     * backplane
     */
    protected tabLabelEntry[] bckplnLab;

    /**
     * random id
     */
    protected int randId;

    /**
     * defaults text
     */
    public final static userFilter[] defaultF = {
        new userFilter("server stack .*", cmds.tabulator + "port " + port, null),
        new userFilter("server stack .*", cmds.tabulator + "protocol " + proto2string(protoAllStrm), null),
        new userFilter("server stack .*", cmds.tabulator + "buffer 65536", null),
        new userFilter("server stack .*", cmds.tabulator + "dataplanes 1", null),
        new userFilter("server stack .*", cmds.tabulator + "discovery 1000 5000", null)
    };

    public userFilter[] srvDefFlt() {
        return defaultF;
    }

    public void srvShRun(String beg, List<String> lst, int filter) {
        lst.add(beg + "dataplanes " + fwds.size());
        lst.add(beg + "discovery " + discoInt + " " + discoTim);
        for (int i = 0; i < fwds.size(); i++) {
            lst.add(beg + cmds.comment);
            servStackFwd ntry = fwds.get(i);
            ntry.getShowRun(beg, "forwarder " + i + cmds.tabulator, lst);
        }
        lst.add(beg + cmds.comment);
    }

    public boolean srvCfgStr(cmds cmd) {
        String s = cmd.word();
        boolean neg = s.equals(cmds.negated);
        if (neg) {
            s = cmd.word();
        }
        if (s.equals("buffer")) {
            bufSiz = bits.str2num(cmd.word());
            return false;
        }
        if (s.equals("discovery")) {
            discoInt = bits.str2num(cmd.word());
            discoTim = bits.str2num(cmd.word());
            return false;
        }
        if (s.equals("dataplanes")) {
            int ned = bits.str2num(cmd.word());
            if (neg) {
                ned = 1;
            }
            if (ned < 1) {
                ned = 1;
            }
            for (int i = fwds.size(); i < ned; i++) {
                fwds.add(new servStackFwd(this));
            }
            for (int i = fwds.size() - 1; i >= ned; i--) {
                servStackFwd cur = fwds.remove(i);
            }
            for (int i = 0; i < fwds.size(); i++) {
                servStackFwd cur = fwds.get(i);
                cur.id = i;
            }
            restartDiscovery(true);
            return false;
        }
        if (!s.equals("forwarder")) {
            return true;
        }
        int i = bits.str2num(cmd.word());
        if ((i < 0) || (i >= fwds.size())) {
            cmd.error("bad forwarder number");
            return true;
        }
        servStackFwd cur = fwds.get(i);
        s = cmd.word();
        if (s.equals("remote")) {
            cur.remote.fromString(cmd.word());
            return false;
        }
        if (s.equals("p4lang")) {
            cur.of = null;
            if (neg) {
                cur.p4 = null;
                return false;
            }
            servP4lang f = cfgAll.srvrFind(new servP4lang(), cfgAll.dmnP4lang, cmd.word());
            cur.p4 = f;
            if (f == null) {
                return true;
            }
            f.parent = this;
            f.parid = cur;
            return false;
        }
        if (s.equals("openflow")) {
            cur.p4 = null;
            if (neg) {
                cur.of = null;
                return false;
            }
            servOpenflow f = cfgAll.srvrFind(new servOpenflow(), cfgAll.dmnOpenflow, cmd.word());
            cur.of = f;
            if (f == null) {
                return true;
            }
            f.parent = this;
            f.parid = cur;
            return false;
        }
        if (s.equals("backplane")) {
            cfgIfc rif = cfgAll.ifcFind(cmd.word(), 0);
            if (rif == null) {
                cmd.error("no such interface");
                return false;
            }
            if (!cur.findIfc(rif.ethtyp)) {
                if (rif.vlanNum == 0) {
                    cmd.error("port not exported");
                    return false;
                }
                if (!cur.findIfc(rif.parent.ethtyp)) {
                    cmd.error("parent not exported");
                    return false;
                }
            }
            servStackIfc ntry = new servStackIfc(cur, rif);
            if (neg) {
                ntry = cur.ifaces.del(ntry);
                if (ntry == null) {
                    cmd.error("no such backplane");
                    return false;
                }
                ntry.ifc.delET(-1);
                cur.reindex();
                return false;
            }
            ntry.ifc = rif.ethtyp;
            ntry.ifc.addET(-1, "stack", ntry);
            ntry.ifc.updateET(-1, ntry);
            ntry.parent.setFilter(true);
            ntry.metric = bits.str2num(cmd.word());
            cur.ifaces.add(ntry);
            cur.reindex();
            return false;
        }
        return true;
    }

    public void srvHelp(userHelp l) {
        l.add(null, false, 1, new int[]{2}, "buffer", "set buffer size on connection");
        l.add(null, false, 2, new int[]{-1}, "<num>", "buffer in bytes");
        l.add(null, false, 1, new int[]{2}, "dataplanes", "set number of forwarders");
        l.add(null, false, 2, new int[]{-1}, "<num>", "limit");
        l.add(null, false, 1, new int[]{2}, "discovery", "specify discovery parameters");
        l.add(null, false, 2, new int[]{3}, "<num>", "keepalive in ms");
        l.add(null, false, 3, new int[]{-1}, "<num>", "timeout in ms");
        l.add(null, false, 1, new int[]{2}, "forwarder", "specify forwarder parameters");
        l.add(null, false, 2, new int[]{3}, "<num>", "keepalive in ms");
        l.add(null, false, 3, new int[]{4}, "p4lang", "use p4lang dataplane");
        l.add(cfgAll.dmnP4lang.listServers(), false, 4, new int[]{-1}, "<name:loc>", "name of server");
        l.add(null, false, 3, new int[]{4}, "openflow", "use openflow dataplane");
        l.add(cfgAll.dmnOpenflow.listServers(), false, 4, new int[]{-1}, "<name:loc>", "name of server");
        l.add(null, false, 3, new int[]{4}, "remote", "address of remote");
        l.add(null, false, 4, new int[]{-1}, "<adr>", "address");
        l.add(null, false, 3, new int[]{4}, "backplane", "interface to use");
        l.add(null, false, 4, new int[]{5}, "<name:ifc>", "name of interface");
        l.add(null, false, 5, new int[]{-1}, "<num>", "metric of port");
    }

    public String srvName() {
        return "stack";
    }

    public int srvPort() {
        return port;
    }

    public int srvProto() {
        return protoAllStrm;
    }

    public boolean srvInit() {
        restartDiscovery(true);
        return genStrmStart(this, new pipeLine(bufSiz, false), 0);
    }

    public boolean srvDeinit() {
        restartDiscovery(false);
        return genericStop(0);
    }

    public boolean srvAccept(pipeSide pipe, prtGenConn id) {
        servStackFwd cur = null;
        for (int i = 0; i < fwds.size(); i++) {
            servStackFwd ntry = fwds.get(i);
            if (ntry.remote.compareTo(id.peerAddr) != 0) {
                continue;
            }
            cur = ntry;
            break;
        }
        if (cur == null) {
            logger.error("dropping unknown forwarder " + id);
            return true;
        }
        if (cur.p4 != null) {
            return cur.p4.srvAccept(pipe, id);
        }
        if (cur.of != null) {
            return cur.of.srvAccept(pipe, id);
        }
        return true;
    }

    /**
     * get hardware forwarder
     *
     * @return offload info
     */
    public String getShGenOneLiner() {
        String a = "stack";
        for (int i = 0; i < fwds.size(); i++) {
            servStackFwd fwd = fwds.get(i);
            a += "," + fwd.getShGetName();
        }
        return a;
    }

    public boolean send2apiPack(int cntr, int ifcn, packHolder pck) {
        return false;
    }

    private void restartDiscovery(boolean need) {
        dscvry.need2work = false;
        dscvry = new servStackDisc(this);
        need &= fwds.size() > 1;
        tabLabel.release(bckplnLab, tabLabelEntry.owner.stack);
        bckplnLab = tabLabel.allocate(tabLabelEntry.owner.stack, fwds.size());
        if (srvVrf != null) {
            for (int i = 0; i < bckplnLab.length; i++) {
                bckplnLab[i].setFwdCommon(tabLabelEntry.owner.stack, srvVrf.fwd6);
            }
        }
        for (int i = 0; i < fwds.size(); i++) {
            servStackFwd cur = fwds.get(i);
            cur.spf = new spfCalc<addrIP>(null);
        }
        doCalc();
        if (!need) {
            return;
        }
        dscvry.startWork();
    }

    /**
     * send keepalives and check if spf needed
     *
     * @return true if changed, false if not
     */
    protected boolean doRound() {
        long tim = bits.getTime() - discoTim;
        boolean chg = false;
        for (int o = 0; o < fwds.size(); o++) {
            servStackFwd cur = fwds.get(o);
            for (int i = 0; i < cur.ifaces.size(); i++) {
                servStackIfc ntry = cur.ifaces.get(i);
                boolean res = ntry.ifc.getState() == state.states.up;
                if (res) {
                    ntry.sendHello();
                }
                res &= ntry.lastTime > tim;
                if (ntry.ready == res) {
                    continue;
                }
                ntry.ready = res;
                chg = true;
            }
        }
        return chg;
    }

    /**
     * calculate spf
     */
    protected void doCalc() {
        servStackFwd cur = fwds.get(0);
        spfCalc<addrIP> spf = new spfCalc<addrIP>(cur.spf);
        for (int o = 0; o < fwds.size(); o++) {
            cur = fwds.get(o);
            addrIP adr = forwarder2addr(o);
            for (int i = 0; i < cur.ifaces.size(); i++) {
                servStackIfc ntry = cur.ifaces.get(i);
                if (!ntry.ready) {
                    continue;
                }
                addrIP nei = forwarder2addr(ntry.lastFwdr.id);
                spf.addConn(adr, nei, ntry.metric, true, false, "prt" + ntry.id);
            }
            tabRouteEntry<addrIP> rou = forwarder2route(o);
            spf.addPref(adr, rou, false);
            spf.addIdent(adr, "fwd" + o);
        }
        cur = fwds.get(0);
        cur.spf = spf;
        spf.bidir.set(1);
        for (int o = 1; o < fwds.size(); o++) {
            fwds.get(o).spf = spf.copyBytes();
        }
        for (int o = 0; o < fwds.size(); o++) {
            cur = fwds.get(o);
            addrIP adr = forwarder2addr(o);
            cur.spf.doWork(adr);
        }
        for (int o = 0; o < fwds.size(); o++) {
            cur = fwds.get(o);
            for (int i = 0; i < cur.ifaces.size(); i++) {
                servStackIfc ntry = cur.ifaces.get(i);
                if (!ntry.ready) {
                    continue;
                }
                addrIP nei = forwarder2addr(ntry.lastFwdr.id);
                ipFwdIface ifc = new ipFwdIface(ntry.id, null);
                cur.spf.addNextHop(ntry.metric, nei, nei, ifc, null, null);
            }
            cur.routes = cur.spf.getRoutes(null, null, null, null);
        }
        for (int o = 0; o < fwds.size(); o++) {
            cur = fwds.get(o);
            if (cur.p4 != null) {
                cur.p4.notif.wakeup();
            }
            if (cur.of != null) {
                cur.of.notif.wakeup();
            }
        }
    }

    /**
     * merge bier forwarders
     *
     * @param who querier
     * @param fwd forwarders
     * @param src source interface
     * @return merged forwarders
     */
    public tabGen<tabLabelBierN> mergeBier(servStackFwd who, tabGen<tabLabelBierN> fwd, ipFwdIface src) {
        tabGen<tabLabelBierN> res = new tabGen<tabLabelBierN>();
        addrIP sou = new addrIP();
        servStackFwd oth = findIfc(who, src);
        if (oth != null) {
            tabRouteEntry<addrIP> oru = forwarder2route(oth.id);
            oru = who.routes.find(oru);
            if (oru != null) {
                sou = oru.best.nextHop.copyBytes();
            }
        }
        for (int i = 0; i < fwd.size(); i++) {
            tabLabelBierN ntry = fwd.get(i);
            if (ntry == null) {
                continue;
            }
            oth = findIfc(who, ntry.iface);
            if (oth == null) {
                continue;
            }
            tabRouteEntry<addrIP> oru = forwarder2route(oth.id);
            oru = who.routes.find(oru);
            if (oru == null) {
                continue;
            }
            if (sou.compareTo(oru.best.nextHop) == 0) {
                continue;
            }
            tabLabelBierN curr = new tabLabelBierN(ntry.fwd, oru.best.iface, oru.best.nextHop, 0, ntry.vpnlab);
            tabLabelBierN old = res.find(curr);
            if (old != null) {
                old.mergeNed(ntry.ned);
                continue;
            }
            curr.mergeNed(ntry.ned);
            res.add(curr);
        }
        return res;
    }

    /**
     * merge bier forwarders
     *
     * @param who querier
     * @param fwd forwarders
     * @param src source interface
     * @return merged forwarders
     */
    public tabGen<addrIP> mergeMcast(servStackFwd who, tabGen<ipFwdIface> fwd, ipFwdIface src) {
        tabGen<addrIP> res = new tabGen<addrIP>();
        addrIP sou = new addrIP();
        servStackFwd oth = findIfc(who, src);
        if (oth != null) {
            tabRouteEntry<addrIP> oru = forwarder2route(oth.id);
            oru = who.routes.find(oru);
            if (oru != null) {
                sou = oru.best.nextHop.copyBytes();
            }
        }
        for (int i = 0; i < fwd.size(); i++) {
            ipFwdIface ntry = fwd.get(i);
            if (ntry == null) {
                continue;
            }
            oth = findIfc(who, ntry);
            if (oth == null) {
                continue;
            }
            tabRouteEntry<addrIP> oru = forwarder2route(oth.id);
            oru = who.routes.find(oru);
            if (oru == null) {
                continue;
            }
            if (sou.compareTo(oru.best.nextHop) == 0) {
                continue;
            }
            res.add(oru.best.nextHop);
        }
        return res;
    }

    /**
     * find interface
     *
     * @param who querier
     * @param ifc interface
     * @return owner, null if error
     */
    protected servStackFwd findIfc(servStackFwd who, cfgBrdg ifc) {
        if (ifc == null) {
            return null;
        }
        for (int i = 0; i < fwds.size(); i++) {
            servStackFwd ntry = fwds.get(i);
            if (ntry.id == who.id) {
                continue;
            }
            if (ntry.findIfc(ifc)) {
                return ntry;
            }
        }
        return null;
    }

    /**
     * find interface
     *
     * @param who querier
     * @param ifc interface
     * @return owner, null if error
     */
    protected servStackFwd findIfc(servStackFwd who, ifcBridgeIfc ifc) {
        if (ifc == null) {
            return null;
        }
        for (int i = 0; i < fwds.size(); i++) {
            servStackFwd ntry = fwds.get(i);
            if (ntry.id == who.id) {
                continue;
            }
            if (ntry.findIfc(ifc)) {
                return ntry;
            }
        }
        return null;
    }

    /**
     * find interface
     *
     * @param who querier
     * @param ifc interface
     * @return owner, null if error
     */
    protected servStackFwd findIfc(servStackFwd who, tabRouteIface ifc) {
        if (ifc == null) {
            return null;
        }
        for (int i = 0; i < fwds.size(); i++) {
            servStackFwd ntry = fwds.get(i);
            if (ntry.id == who.id) {
                continue;
            }
            if (ntry.findIfc(ifc)) {
                return ntry;
            }
        }
        return null;
    }

    /**
     * find interface
     *
     * @param who querier
     * @param ifc interface
     * @return owner, null if error
     */
    protected servStackFwd findIfc(servStackFwd who, ifcEthTyp ifc) {
        if (ifc == null) {
            return null;
        }
        for (int i = 0; i < fwds.size(); i++) {
            servStackFwd ntry = fwds.get(i);
            if (ntry.id == who.id) {
                continue;
            }
            if (ntry.findIfc(ifc)) {
                return ntry;
            }
        }
        return null;
    }

    /**
     * get backplane show
     *
     * @param fwd forwarder
     * @return show
     */
    public userFormat getShowPorts(int fwd) {
        if ((fwd < 0) || (fwd >= fwds.size())) {
            return null;
        }
        servStackFwd cur = fwds.get(fwd);
        userFormat res = new userFormat("|", "port|metric|ready|remote|peering");
        for (int i = 0; i < cur.ifaces.size(); i++) {
            servStackIfc ntry = cur.ifaces.get(i);
            res.add(ntry.pi + "|" + ntry.metric + "|" + ntry.ready + "|" + ntry.lastFwdr + "|" + ntry.lastPort);
        }
        return res;
    }

    /**
     * get backplane show
     *
     * @param fwd forwarder
     * @return show
     */
    public userFormat getShowSpf(int fwd) {
        if ((fwd < 0) || (fwd >= fwds.size())) {
            return null;
        }
        servStackFwd cur = fwds.get(fwd);
        return cur.spf.listStatistics();
    }

    /**
     * get backplane show
     *
     * @param fwd forwarder
     * @param cmd masks
     * @return show
     */
    public userFormat getShowTopo(int fwd, cmds cmd) {
        if ((fwd < 0) || (fwd >= fwds.size())) {
            return null;
        }
        servStackFwd cur = fwds.get(fwd);
        return cur.spf.listTopology(new addrIP(), cmd);
    }

    /**
     * get backplane show
     *
     * @param fwd forwarder
     * @param cmd masks
     * @return show
     */
    public List<String> getShowTree(int fwd, cmds cmd) {
        if ((fwd < 0) || (fwd >= fwds.size())) {
            return null;
        }
        servStackFwd cur = fwds.get(fwd);
        return cur.spf.listTree(cmd);
    }

    /**
     * get backplane show
     *
     * @param fwd forwarder
     * @param cmd masks
     * @return show
     */
    public List<String> getShowGraph(int fwd, cmds cmd) {
        if ((fwd < 0) || (fwd >= fwds.size())) {
            return null;
        }
        servStackFwd cur = fwds.get(fwd);
        return cur.spf.listGraphviz(cmd);
    }

    /**
     * get backplane show
     *
     * @param fwd forwarder
     * @return show
     */
    public tabRoute<addrIP> getShowRoute(int fwd) {
        if ((fwd < 0) || (fwd >= fwds.size())) {
            return new tabRoute<addrIP>("bp");
        }
        servStackFwd cur = fwds.get(fwd);
        return cur.routes;
    }

    /**
     * get generic show
     *
     * @return show
     */
    public userFormat getShowBcks() {
        userFormat res = new userFormat("|", "fwd|name|addr|routes|oneliner");
        for (int i = 0; i < fwds.size(); i++) {
            servStackFwd ntry = fwds.get(i);
            res.add(i + "|" + ntry.getShGetName() + "|" + ntry.remote + "|" + ntry.routes.size() + "|" + ntry.getShGet1liner());
        }
        return res;
    }

}

class servStackDisc implements Runnable {

    public boolean need2work = true;

    private final servStack lower;

    public servStackDisc(servStack parent) {
        lower = parent;
    }

    public void startWork() {
        lower.randId = bits.randomD();
        new Thread(this).start();
    }

    public void run() {
        for (;;) {
            if (!need2work) {
                break;
            }
            try {
                if (lower.doRound()) {
                    lower.doCalc();
                    logger.info("recalculated backplane");
                }
            } catch (Exception e) {
                logger.traceback(e);
            }
            bits.sleep(lower.discoInt);
        }
    }

}
