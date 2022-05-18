package net.freertr.serv;

import java.util.ArrayList;
import java.util.List;
import net.freertr.addr.addrIP;
import net.freertr.cfg.cfgIfc;
import net.freertr.ifc.ifcBridgeIfc;
import net.freertr.pipe.pipeLine;
import net.freertr.pipe.pipeSide;
import net.freertr.prt.prtGenConn;
import net.freertr.prt.prtServS;
import net.freertr.tab.tabGen;
import net.freertr.tab.tabIntMatcher;
import net.freertr.tab.tabLabel;
import net.freertr.tab.tabLabelEntry;
import net.freertr.tab.tabRoute;
import net.freertr.tab.tabRouteIface;
import net.freertr.user.userFilter;
import net.freertr.user.userFormat;
import net.freertr.user.userHelping;
import net.freertr.util.bits;
import net.freertr.util.cmds;
import net.freertr.util.logger;
import net.freertr.util.shrtPthFrst;

/**
 * p4lang server
 *
 * @author matecsaba
 */
public class servP4lang extends servGeneric implements prtServS {

    /**
     * create instance
     */
    public servP4lang() {
        fwds = new ArrayList<servP4langCfg>();
        fwds.add(new servP4langCfg(this, 0));
        dscvry = new servP4langDcvr(this);
        restartDiscovery(false);
    }

    /**
     * forwarders
     */
    protected final List<servP4langCfg> fwds;

    /**
     * discovery
     */
    protected servP4langDcvr dscvry;

    /**
     * backplane
     */
    protected tabLabelEntry[] bckplnLab;

    /**
     * port
     */
    public final static int port = 9080;

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
     * defaults text
     */
    public final static String[] defaultL = {
        "server p4lang .*! port " + port,
        "server p4lang .*! protocol " + proto2string(protoAllStrm),
        "server p4lang .*! buffer 65536",
        "server p4lang .*! dataplanes 1",
        "server p4lang .*! discovery 1000 5000",
        "server p4lang .*! no api-stat",
        "server p4lang .*! no export-srv6",
        "server p4lang .*! no export-copp4",
        "server p4lang .*! no export-copp6",
        "server p4lang .*! no export-socket",
        "server p4lang .*! no interconnect",
        "server p4lang .*! export-interval 1000",
        "server p4lang .*! no forwarder .* api-stat",
        "server p4lang .*! no forwarder .* export-srv6",
        "server p4lang .*! no forwarder .* export-copp4",
        "server p4lang .*! no forwarder .* export-copp6",
        "server p4lang .*! no forwarder .* export-socket",
        "server p4lang .*! no forwarder .* interconnect",
        "server p4lang .*! forwarder .* export-interval 1000"
    };

    /**
     * defaults filter
     */
    public static tabGen<userFilter> defaultF;

    public tabGen<userFilter> srvDefFlt() {
        return defaultF;
    }

    public void srvShRun(String beg, List<String> l, int filter) {
        l.add(beg + "buffer " + bufSiz);
        l.add(beg + "dataplanes " + fwds.size());
        l.add(beg + "discovery " + discoInt + " " + discoTim);
        if (fwds.size() <= 1) {
            fwds.get(0).getShowRun(beg, "", l);
            return;
        }
        for (int i = 0; i < fwds.size(); i++) {
            servP4langCfg cur = fwds.get(i);
            String a = "forwarder " + i + " ";
            cur.getShowRun(beg, a, l);
            cur.getShowRun2(beg, a, l);
        }
    }

    public boolean srvCfgStr(cmds cmd) {
        String s = cmd.word();
        boolean neg = s.equals("no");
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
                fwds.add(new servP4langCfg(this, i));
            }
            for (int i = fwds.size() - 1; i >= ned; i--) {
                servP4langCfg cur = fwds.remove(i);
                cur.doClear();
            }
            restartDiscovery(true);
            return false;
        }
        tabIntMatcher mat = new tabIntMatcher();
        mat.setExact(0);
        if (s.equals("forwarder")) {
            if (mat.fromString(cmd.word())) {
                return true;
            }
            s = cmd.word();
        }
        boolean res = false;
        for (int i = 0; i < fwds.size(); i++) {
            if (!mat.matches(i)) {
                continue;
            }
            servP4langCfg cur = fwds.get(i);
            cmds c = cmd.copyBytes(false);
            if (neg) {
                res |= cur.doUnConfig(s, c);
            } else {
                res |= cur.doConfig(s, c);
            }
        }
        return res;
    }

    public void srvHelp(userHelping l) {
        l.add(null, "1 2  buffer                    set buffer size on connection");
        l.add(null, "2 .    <num>                   buffer in bytes");
        l.add(null, "1 2  dataplanes                set number of forwarders");
        l.add(null, "2 .    <num>                   limit");
        l.add(null, "1 2  discovery                 specify discovery parameters");
        l.add(null, "2 3    <num>                   keepalive in ms");
        l.add(null, "3 .      <num>                 timeout in ms");
        if (fwds.size() <= 1) {
            fwds.get(0).getHelpText(l, 1);
            return;
        }
        l.add(null, "1 2  forwarder                 specify one forwarder parameters");
        l.add(null, "2 3    <num>                   forwarder number");
        l.add(null, "2 3    all                     every forwarder");
        fwds.get(0).getHelpText(l, 3);
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
        restartDiscovery(true);
        return genStrmStart(this, new pipeLine(bufSiz, false), 0);
    }

    public boolean srvDeinit() {
        restartDiscovery(false);
        return genericStop(0);
    }

    public boolean srvAccept(pipeSide pipe, prtGenConn id) {
        servP4langCfg cur = null;
        for (int i = 0; i < fwds.size(); i++) {
            servP4langCfg ntry = fwds.get(i);
            if (ntry.remote.compare(ntry.remote, id.peerAddr) != 0) {
                continue;
            }
            cur = ntry;
            break;
        }
        if (fwds.size() <= 1) {
            cur = fwds.get(0);
        }
        if (cur == null) {
            logger.error("dropping unknown forwarder " + id);
            return true;
        }
        cur.conn.pipe.setClose();
        cur.notif.wakeup();
        id.timeout = 120000;
        pipe.lineRx = pipeSide.modTyp.modeCRorLF;
        pipe.lineTx = pipeSide.modTyp.modeLF;
        cur.remote = id.peerAddr.copyBytes();
        cur.minBuf = bufSiz / 2;
        cur.conn = new servP4langConn(pipe, cur);
        cur.conn.startWork();
        return false;
    }

    private void restartDiscovery(boolean need) {
        need &= fwds.size() > 1;
        dscvry.need2work = false;
        tabLabel.release(bckplnLab, 23);
        bckplnLab = tabLabel.allocate(23, fwds.size());
        for (int i = 0; i < bckplnLab.length; i++) {
            bckplnLab[i].setFwdDrop(23);
        }
        for (int i = 0; i < fwds.size(); i++) {
            servP4langCfg cur = fwds.get(i);
            cur.bckplnSpf = new shrtPthFrst<addrIP>(null);
        }
        dscvry = new servP4langDcvr(this);
        dscvry.doCalc();
        if (!need) {
            return;
        }
        dscvry.startWork();
    }

    /**
     * find interface
     *
     * @param who querier
     * @param ifc interface
     * @return owner, null if error
     */
    protected servP4langCfg findIfc(servP4langCfg who, ifcBridgeIfc ifc) {
        if (ifc == null) {
            return null;
        }
        for (int i = 0; i < fwds.size(); i++) {
            servP4langCfg ntry = fwds.get(i);
            if (ntry.id == who.id) {
                continue;
            }
            if (ntry.findIfc(ifc) == null) {
                continue;
            }
            return ntry;
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
    protected servP4langCfg findIfc(servP4langCfg who, tabRouteIface ifc) {
        if (ifc == null) {
            return null;
        }
        for (int i = 0; i < fwds.size(); i++) {
            servP4langCfg ntry = fwds.get(i);
            if (ntry.id == who.id) {
                continue;
            }
            if (ntry.findIfc(ifc) == null) {
                continue;
            }
            return ntry;
        }
        return null;
    }

    /**
     * get backplane show
     *
     * @param fwd forwarder
     * @param mod mode: 1=ports, 2=spf, 3=topology
     * @return show
     */
    public userFormat getShowBp1(int fwd, int mod) {
        if ((fwd < 0) || (fwd >= fwds.size())) {
            return null;
        }
        servP4langCfg cur = fwds.get(fwd);
        switch (mod) {
            case 1:
                userFormat res = new userFormat("|", "port|metric|ready|remote|peering");
                for (int i = 0; i < cur.backPlanes.size(); i++) {
                    servP4langBkpl ntry = cur.backPlanes.get(i);
                    res.add(ntry.pi + "|" + ntry.metric + "|" + ntry.ready + "|" + ntry.lastFwdr + "|" + ntry.lastPort);
                }
                return res;
            case 2:
                return cur.bckplnSpf.listStatistics();
            case 3:
                return cur.bckplnSpf.listTopology();
            default:
                return null;
        }
    }

    /**
     * get backplane show
     *
     * @param fwd forwarder
     * @param mod mode: 1=tree, 2=graph
     * @return show
     */
    public List<String> getShowBp2(int fwd, int mod) {
        if ((fwd < 0) || (fwd >= fwds.size())) {
            return null;
        }
        servP4langCfg cur = fwds.get(fwd);
        switch (mod) {
            case 1:
                return cur.bckplnSpf.listTree();
            case 2:
                return cur.bckplnSpf.listGraphviz(false, false, false);
            default:
                return null;
        }
    }

    /**
     * get backplane show
     *
     * @param fwd forwarder
     * @return show
     */
    public tabRoute<addrIP> getShowBp3(int fwd) {
        if ((fwd < 0) || (fwd >= fwds.size())) {
            return new tabRoute<addrIP>("no");
        }
        servP4langCfg cur = fwds.get(fwd);
        return cur.bckplnRou;
    }

    /**
     * get generic show
     *
     * @param fwd forwarder
     * @param mod mode: 1=generic, 2=apiTx, 3=apiRx, 4=front, 5=ifaces,
     * 6=neighs, 7=mpls, 8=nsh
     * @return show
     */
    public userFormat getShowGen(int fwd, int mod) {
        if ((fwd < 0) || (fwd >= fwds.size())) {
            return null;
        }
        servP4langCfg cur = fwds.get(fwd);
        switch (mod) {
            case 1:
                return cur.getShowGen();
            case 2:
                return servP4langUtil.dumpApiStats(cur.apiStatTx);
            case 3:
                return servP4langUtil.dumpApiStats(cur.apiStatRx);
            case 4:
                return cur.getShowFront();
            case 5:
                return cur.getShowIfaces();
            case 6:
                return cur.getShowNeighs();
            case 7:
                return cur.getShowMpls();
            case 8:
                return cur.getShowNsh();
            default:
                return null;
        }
    }

    /**
     * get route show
     *
     * @param bri bridge
     * @param fwd forwarder
     * @return show
     */
    public userFormat getShowBri(int bri, int fwd) {
        if ((fwd < 0) || (fwd >= fwds.size())) {
            return null;
        }
        servP4langCfg cur = fwds.get(fwd);
        servP4langBr ntry = cur.expBr.find(new servP4langBr(bri));
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
     * get route show
     *
     * @param prt protocol
     * @param vrf vrf
     * @param fwd forwarder
     * @return show
     */
    public tabRoute<addrIP> getShowRou(int prt, int vrf, int fwd) {
        if ((fwd < 0) || (fwd >= fwds.size())) {
            return null;
        }
        servP4langCfg cur = fwds.get(fwd);
        servP4langVrf ntry = cur.expVrf.find(new servP4langVrf(vrf));
        if (ntry == null) {
            return null;
        }
        if (prt == 4) {
            return ntry.routes4;
        } else {
            return ntry.routes6;
        }
    }

    /**
     * get interfaces show
     *
     * @param fwd forwarder
     * @param ifc interface
     * @return show
     */
    public userFormat getShowIface(int fwd, cfgIfc ifc) {
        if ((fwd < 0) || (fwd >= fwds.size())) {
            return null;
        }
        servP4langCfg cur = fwds.get(fwd);
        return cur.getShowIface(ifc);
    }

    /**
     * do clear
     *
     * @param fwd forwarder
     */
    public void doClear(int fwd) {
        if ((fwd < 0) || (fwd >= fwds.size())) {
            return;
        }
        servP4langCfg cur = fwds.get(fwd);
        cur.doClear();
    }

    /**
     * send line
     *
     * @param fwd forwarder
     * @param str line
     */
    public void sendLine(int fwd, String str) {
        if ((fwd < 0) || (fwd >= fwds.size())) {
            return;
        }
        servP4langCfg cur = fwds.get(fwd);
        cur.sendLine(str);
    }

}
