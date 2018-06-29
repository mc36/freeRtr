package serv;

import addr.addrEmpty;
import addr.addrIP;
import addr.addrMac;
import addr.addrPrefix;
import addr.addrType;
import cfg.cfgAll;
import cfg.cfgIfc;
import cfg.cfgVrf;
import ifc.ifcDn;
import ifc.ifcEther;
import ifc.ifcNull;
import ifc.ifcUp;
import ip.ipFwdIface;
import ip.ipFwdMcast;
import ip.ipIfc;
import ip.ipIfc4;
import ip.ipIfc4arp;
import ip.ipIfc6;
import ip.ipMpls;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import pack.packHolder;
import pack.packOpenflow;
import pipe.pipeLine;
import pipe.pipeSide;
import prt.prtGenConn;
import prt.prtServS;
import tab.tabGen;
import tab.tabLabel;
import tab.tabLabelDup;
import tab.tabLabelNtry;
import tab.tabRoute;
import tab.tabRouteEntry;
import user.userFilter;
import user.userHelping;
import util.bits;
import util.cmds;
import util.counter;
import util.debugger;
import util.logger;
import util.notifier;
import util.state;
import util.typLenVal;

/**
 * openflow server
 *
 * @author matecsaba
 */
public class servOpenflow extends servGeneric implements prtServS {

    private servOpenflowRx thrdRx;

    private servOpenflowTx thrdTx;

    /**
     * connection
     */
    protected pipeSide conn;

    /**
     * tx notifier
     */
    protected notifier notif = new notifier();

    /**
     * tx id
     */
    protected int xid = 1;

    /**
     * version number
     */
    public int version = 4;

    /**
     * exported vrf
     */
    public cfgVrf expVrf;

    /**
     * group table
     */
    public static final int tabGrp = -1;

    /**
     * port table
     */
    public static final int tabPort = 0;

    /**
     * mpls table
     */
    public static final int tabMpls = 1;

    /**
     * ipv4 table
     */
    public static final int tabIpv4 = 2;

    /**
     * ipv6 table
     */
    public static final int tabIpv6 = 3;

    /**
     * exported interfaces
     */
    public tabGen<servOpenflowIfc1> expIfc = new tabGen<servOpenflowIfc1>();

    /**
     * defaults text
     */
    public final static String defaultL[] = {
        "server openflow .*! port " + packOpenflow.port,
        "server openflow .*! protocol " + proto2string(protoAllStrm),
        "server openflow .*! version 4"
    };

    /**
     * defaults filter
     */
    public static tabGen<userFilter> defaultF;

    public tabGen<userFilter> srvDefFlt() {
        return defaultF;
    }

    public boolean srvAccept(pipeSide pipe, prtGenConn id) {
        if (conn != null) {
            thrdRx.working = false;
            thrdTx.working = false;
            conn.setClose();
            notif.wakeup();
        }
        pipe.timeout = 120000;
        conn = pipe;
        thrdRx = new servOpenflowRx(conn, this);
        thrdTx = new servOpenflowTx(conn, this);
        logger.warn("neighbor " + id.peerAddr + " up");
        return false;
    }

    public void srvShRun(String beg, List<String> l) {
        l.add(beg + "version " + version);
        if (expVrf == null) {
            l.add(beg + "no export-vrf");
        } else {
            l.add(beg + "export-vrf " + expVrf.name);
        }
        for (int i = 0; i < expIfc.size(); i++) {
            servOpenflowIfc1 ntry = expIfc.get(i);
            l.add(beg + "export-port " + ntry.ifc.name + " " + ntry.id);
        }
    }

    public boolean srvCfgStr(cmds cmd) {
        String s = cmd.word();
        if (s.equals("version")) {
            version = bits.str2num(cmd.word());
            return false;
        }
        if (s.equals("export-vrf")) {
            clearNotif();
            expVrf = cfgAll.vrfFind(cmd.word(), false);
            if (expVrf == null) {
                cmd.error("no such vrf");
                return false;
            }
            expVrf.fwd4.tableChanged = notif;
            expVrf.fwd6.tableChanged = notif;
            notif.wakeup();
            return false;
        }
        if (s.equals("export-port")) {
            cfgIfc ifc = cfgAll.ifcFind(cmd.word(), false);
            if (ifc == null) {
                cmd.error("no such interface");
                return false;
            }
            if ((ifc.type != cfgIfc.ifaceType.sdn) && (ifc.type != cfgIfc.ifaceType.bridge)) {
                cmd.error("not openflow interface");
                return false;
            }
            servOpenflowIfc1 ntry = new servOpenflowIfc1();
            ntry.id = bits.str2num(cmd.word());
            ntry.ifc = ifc;
            ntry.lower = this;
            if (ifc.type == cfgIfc.ifaceType.bridge) {
                ntry.id = tabGrp;
                ntry.grp = ifc.bridgeHed.num;
            }
            expIfc.put(ntry);
            if (conn != null) {
                ntry.sendState(0);
            }
            if (ntry.id != tabGrp) {
                ntry.setUpper(ifc.ethtyp);
            }
            notif.wakeup();
            return false;
        }
        if (!s.equals("no")) {
            return true;
        }
        s = cmd.word();
        if (s.equals("export-vrf")) {
            clearNotif();
            expVrf = null;
            notif.wakeup();
            return false;
        }
        if (s.equals("export-port")) {
            cfgIfc ifc = cfgAll.ifcFind(cmd.word(), false);
            if (ifc == null) {
                cmd.error("no such interface");
                return false;
            }
            servOpenflowIfc1 ntry = new servOpenflowIfc1();
            ntry.id = bits.str2num(cmd.word());
            ntry.ifc = ifc;
            if (ifc.type == cfgIfc.ifaceType.bridge) {
                ntry.id = tabGrp;
                ntry.grp = bits.str2num(ifc.bridgeHed.name);
            }
            ntry = expIfc.del(ntry);
            clearIface(ntry);
            notif.wakeup();
            return false;
        }
        return true;
    }

    public void srvHelp(userHelping l) {
        l.add("1 2  version                   openflow version");
        l.add("2 .    <num>                   version number");
        l.add("1 2  export-vrf                specify vrf to export");
        l.add("2 .    <name>                  vrf name");
        l.add("1 2  export-port               specify port to export");
        l.add("2 3    <name>                  interface name");
        l.add("3 .      <num>                 openflow port number");
    }

    public String srvName() {
        return "openflow";
    }

    public int srvPort() {
        return packOpenflow.port;
    }

    public int srvProto() {
        return protoAllStrm;
    }

    public boolean srvInit() {
        return genStrmStart(this, new pipeLine(65536, false), 0);
    }

    public boolean srvDeinit() {
        if (conn != null) {
            thrdRx.working = false;
            thrdTx.working = false;
            conn.setClose();
        }
        notif.wakeup();
        return genericStop(0);
    }

    private void clearNotif() {
        if (expVrf == null) {
            return;
        }
        expVrf.fwd4.tableChanged = null;
        expVrf.fwd6.tableChanged = null;
    }

    private void clearIface(servOpenflowIfc1 ifc) {
        if (ifc == null) {
            return;
        }
        if (ifc.id == tabGrp) {
            return;
        }
        ifcNull nul = new ifcNull();
        nul.setUpper(ifc.ifc.ethtyp);
    }

    /**
     * send packet
     *
     * @param pckB binary
     * @param pckO header
     */
    protected synchronized void sendPack(packHolder pckB, packOpenflow pckO) {
        if (debugger.servOpenflowTraf) {
            logger.debug("tx " + pckO.dump(pckB));
        }
        pckO.xid = xid++;
        pckO.version = version;
        pckO.sendPack(pckB);
    }

}

class servOpenflowIfc1 implements ifcDn, Comparator<servOpenflowIfc1> {

    public int id;

    public int grp;

    public int cook;

    public cfgIfc ifc;

    public servOpenflow lower;

    public ifcUp upper = new ifcNull();

    public counter cntr = new counter();

    public int compare(servOpenflowIfc1 o1, servOpenflowIfc1 o2) {
        if (o1.id < o2.id) {
            return -1;
        }
        if (o1.id > o2.id) {
            return +1;
        }
        if (o1.id != servOpenflow.tabGrp) {
            return 0;
        }
        if (o1.grp < o2.grp) {
            return -1;
        }
        if (o1.grp > o2.grp) {
            return +1;
        }
        return 0;
    }

    public String toString() {
        return "openflow port " + id;
    }

    public void sendState(int cfg) {
        if (id == servOpenflow.tabGrp) {
            return;
        }
        packHolder pckB = new packHolder(true, true);
        packOpenflow pckO = new packOpenflow();
        pckO.pipe = lower.conn;
        pckO.createPortMod(pckB, id, ifc.ethtyp.getHwAddr(), cfg, 1);
        lower.sendPack(pckB, pckO);
    }

    public addrType getHwAddr() {
        return new addrEmpty();
    }

    public void setFilter(boolean promisc) {
    }

    public state.states getState() {
        return state.states.up;
    }

    public void closeDn() {
    }

    public void flapped() {
    }

    public void setUpper(ifcUp server) {
        upper = server;
        upper.setParent(this);
    }

    public counter getCounter() {
        return cntr;
    }

    public int getMTUsize() {
        return 1500;
    }

    public long getBandwidth() {
        return 8000000;
    }

    public void sendPack(packHolder pck) {
        if (lower.conn == null) {
            return;
        }
        if (id == servOpenflow.tabGrp) {
            return;
        }
        ifcEther.createETHheader(pck, false);
        packOpenflow pckO = new packOpenflow();
        pckO.pipe = lower.conn;
        pckO.createPckOut(pck, id);
        lower.sendPack(pck, pckO);
    }

}

class servOpenflowIfc2 implements Comparator<servOpenflowIfc2> {

    public ipFwdIface ifc;

    public ipIfc ipi;

    public servOpenflowIfc1 ifo;

    public int compare(servOpenflowIfc2 o1, servOpenflowIfc2 o2) {
        return o1.ifc.compare(o1.ifc, o2.ifc);
    }

    public servOpenflowIfc2(ipFwdIface iface, ipIfc ipifc, servOpenflowIfc1 ofifc) {
        ifc = iface;
        ipi = ipifc;
        ifo = ofifc;
    }

}

class servOpenflowFlw implements Comparator<servOpenflowFlw> {

    public byte[] match = new byte[0];

    public byte[] action = new byte[0];

    public int cookie;

    public int prio;

    public int compare(servOpenflowFlw o1, servOpenflowFlw o2) {
        if (o1.match.length < o2.match.length) {
            return -1;
        }
        if (o1.match.length > o2.match.length) {
            return +1;
        }
        return bits.byteComp(o1.match, 0, o2.match, 0, o1.match.length);
    }

    public boolean sameAct(servOpenflowFlw other) {
        if (other.action.length != action.length) {
            return false;
        }
        return bits.byteComp(action, 0, other.action, 0, action.length) == 0;
    }

    public String dump() {
        return "cookie=" + cookie + " prio=" + prio + " match=" + bits.byteDump(match, 0, -1) + " action=" + bits.byteDump(action, 0, -1);
    }

}

class servOpenflowRx implements Runnable {

    public boolean working = true;

    private pipeSide pipe;

    private servOpenflow lower;

    public servOpenflowRx(pipeSide stream, servOpenflow parent) {
        pipe = stream;
        lower = parent;
        new Thread(this).start();
    }

    public void run() {
        try {
            doWord();
        } catch (Exception e) {
            logger.traceback(e);
        }
        pipe.setClose();
        lower.notif.wakeup();
    }

    private void doWord() {
        packHolder pckB = new packHolder(true, true);
        packOpenflow pckO = new packOpenflow();
        pckO.pipe = pipe;
        for (;;) {
            if (!working) {
                return;
            }
            if (pckO.recvPack(pckB)) {
                return;
            }
            if (debugger.servOpenflowTraf) {
                logger.debug("rx " + pckO.dump(pckB));
            }
            switch (pckO.type) {
                case packOpenflow.typHello:
                    break;
                case packOpenflow.typEchoReq:
                    pckO.createEchoRep();
                    lower.sendPack(pckB, pckO);
                    break;
                case packOpenflow.typEchoRep:
                    break;
                case packOpenflow.typPortStat:
                    break;
                case packOpenflow.typPackIn:
                    servOpenflowIfc1 ntry = new servOpenflowIfc1();
                    ntry.id = pckO.parsePckIn(pckB);
                    ntry = lower.expIfc.find(ntry);
                    if (ntry == null) {
                        break;
                    }
                    ifcEther.parseETHheader(pckB, false);
                    ntry.upper.recvPack(pckB);
                    break;
                case packOpenflow.typMultiRep:
                    if (pckB.msbGetW(0) != 13) {
                        break;
                    }
                    pckB.getSkip(8);
                    for (;;) {
                        if (pckB.dataSize() < 64) {
                            break;
                        }
                        int i = pckB.msbGetD(0);
                        addrMac mac = new addrMac();
                        pckB.getAddr(mac, 8);
                        pckB.getSkip(64);
                        if (debugger.servOpenflowTraf) {
                            logger.debug("port #" + i + " - " + mac);
                        }
                    }
                    break;
                case packOpenflow.typFeatRep:
                    if (debugger.servOpenflowTraf) {
                        logger.debug("datapath=" + pckB.msbGetQ(0) + " buffers=" + pckB.msbGetD(8) + " tables=" + pckB.getByte(12));
                    }
                    break;
                default:
                    logger.info("got invalid message");
                    break;
            }
        }
    }

}

class servOpenflowTx implements Runnable {

    public boolean working = true;

    private pipeSide pipe;

    private servOpenflow lower;

    private tabGen<servOpenflowFlw> tabGroup = new tabGen<servOpenflowFlw>();

    private tabGen<servOpenflowFlw> tabPort = new tabGen<servOpenflowFlw>();

    private tabGen<servOpenflowFlw> tabMpls = new tabGen<servOpenflowFlw>();

    private tabGen<servOpenflowFlw> tabIpv4 = new tabGen<servOpenflowFlw>();

    private tabGen<servOpenflowFlw> tabIpv6 = new tabGen<servOpenflowFlw>();

    public servOpenflowTx(pipeSide stream, servOpenflow parent) {
        pipe = stream;
        lower = parent;
        new Thread(this).start();
    }

    public void run() {
        try {
            doWord();
        } catch (Exception e) {
            logger.traceback(e);
        }
        pipe.setClose();
    }

    private void sendTable(packHolder pckB, packOpenflow pckO, int tab, tabGen<servOpenflowFlw> done, tabGen<servOpenflowFlw> need) {
        for (int i = done.size() - 1; i >= 0; i--) {
            servOpenflowFlw ntry = done.get(i);
            if (need.find(ntry) != null) {
                continue;
            }
            done.del(ntry);
            pckB.clear();
            pckO.putMatchBuf(pckB, ntry.match);
            pckO.createFlowMod(pckB, ntry.cookie, tab, packOpenflow.flowCmdDels, ntry.prio);
            lower.sendPack(pckB, pckO);
        }
        for (int i = 0; i < need.size(); i++) {
            servOpenflowFlw ntry = need.get(i);
            servOpenflowFlw old = done.find(ntry);
            if (old == null) {
                done.put(ntry);
                pckB.clear();
                pckB.putCopy(ntry.action, 0, 0, ntry.action.length);
                pckB.putSkip(ntry.action.length);
                pckB.merge2beg();
                pckO.putMatchBuf(pckB, ntry.match);
                pckO.createFlowMod(pckB, ntry.cookie, tab, packOpenflow.flowCmdAdd, ntry.prio);
                lower.sendPack(pckB, pckO);
                continue;
            }
            if (ntry.sameAct(old)) {
                continue;
            }
            done.put(ntry);
            pckB.clear();
            pckB.putCopy(ntry.action, 0, 0, ntry.action.length);
            pckB.putSkip(ntry.action.length);
            pckB.merge2beg();
            pckO.putMatchBuf(pckB, ntry.match);
            pckO.createFlowMod(pckB, ntry.cookie, tab, packOpenflow.flowCmdMdfs, ntry.prio);
            lower.sendPack(pckB, pckO);
        }
    }

    private void sendGroup(packHolder pckB, packOpenflow pckO, tabGen<servOpenflowFlw> done, tabGen<servOpenflowFlw> need) {
        for (int i = done.size() - 1; i >= 0; i--) {
            servOpenflowFlw ntry = done.get(i);
            if (need.find(ntry) != null) {
                continue;
            }
            done.del(ntry);
            pckB.clear();
            pckO.createGroupMod(pckB, packOpenflow.groupCmdDel, packOpenflow.groupTypAll, ntry.cookie);
            lower.sendPack(pckB, pckO);
        }
        for (int i = 0; i < need.size(); i++) {
            servOpenflowFlw ntry = need.get(i);
            servOpenflowFlw old = done.find(ntry);
            if (old == null) {
                done.put(ntry);
                pckB.clear();
                pckB.putCopy(ntry.action, 0, 0, ntry.action.length);
                pckB.putSkip(ntry.action.length);
                pckB.merge2beg();
                pckO.createGroupMod(pckB, packOpenflow.groupCmdAdd, packOpenflow.groupTypAll, ntry.cookie);
                lower.sendPack(pckB, pckO);
                continue;
            }
            if (ntry.sameAct(old)) {
                continue;
            }
            done.put(ntry);
            pckB.clear();
            pckB.putCopy(ntry.action, 0, 0, ntry.action.length);
            pckB.putSkip(ntry.action.length);
            pckB.merge2beg();
            pckO.createGroupMod(pckB, packOpenflow.groupCmdMdf, packOpenflow.groupTypAll, ntry.cookie);
            lower.sendPack(pckB, pckO);
        }
    }

    private void addTable(tabGen<servOpenflowFlw> trg, tabGen<servOpenflowFlw> src, servOpenflowFlw ntry) {
        servOpenflowFlw old = src.find(ntry);
        if (old != null) {
            ntry.cookie = old.cookie;
            trg.put(ntry);
            return;
        }
        for (;;) {
            ntry.cookie = bits.random(8, 0x7ffffff0);
            boolean fnd = false;
            for (int i = 0; i < src.size(); i++) {
                if (src.get(i).cookie != ntry.cookie) {
                    continue;
                }
                fnd = true;
                break;
            }
            if (!fnd) {
                break;
            }
        }
        trg.put(ntry);
    }

    private int getLabel(List<Integer> lab) {
        if (lab == null) {
            return -1;
        }
        if (lab.size() < 1) {
            return -1;
        }
        int i = lab.get(0);
        if (i == ipMpls.labelImp) {
            return -1;
        }
        return i;
    }

    private void createGroupMcast(packHolder pckB, packHolder pckT, packOpenflow pckO, tabGen<servOpenflowFlw> n, boolean ipv4, tabGen<servOpenflowIfc2> ifcs) {
        tabGen<ipFwdMcast> groups;
        if (ipv4) {
            groups = lower.expVrf.fwd4.groups;
        } else {
            groups = lower.expVrf.fwd6.groups;
        }
        for (int i = 0; i < groups.size(); i++) {
            ipFwdMcast grp = groups.get(i);
            if (grp == null) {
                continue;
            }
            if (grp.local) {
                continue;
            }
            if (grp.iface == null) {
                continue;
            }
            servOpenflowFlw ntry = new servOpenflowFlw();
            ntry.match = new byte[33];
            ntry.match[0] = 2;
            grp.group.toBuffer(ntry.match, 1);
            grp.source.toBuffer(ntry.match, 17);
            pckB.clear();
            for (int o = 0; o < grp.flood.size(); o++) {
                servOpenflowIfc2 ifc = new servOpenflowIfc2(grp.flood.get(o), null, null);
                if (ifc.ifc == null) {
                    continue;
                }
                ifc = ifcs.find(ifc);
                if (ifc == null) {
                    continue;
                }
                if (ifc.ifo.id == servOpenflow.tabGrp) {
                    continue;
                }
                List<typLenVal> tlvs = new ArrayList<typLenVal>();
                pckT.clear();
                pckO.createMatchMac(pckT, true, (addrMac) ifc.ifo.ifc.ethtyp.getHwAddr(), null);
                tlvs.add(pckO.getActionSetField(pckT));
                tlvs.add(pckO.getActionOutput(ifc.ifo.id));
                pckO.createBucketAct(pckB, tlvs);
                pckB.merge2end();
            }
            ntry.action = pckB.getCopy();
            addTable(n, tabGroup, ntry);
        }
    }

    private tabGen<servOpenflowFlw> createGroup(packHolder pckB, packHolder pckT, packOpenflow pckO, tabGen<servOpenflowIfc2> ifcs4, tabGen<servOpenflowIfc2> ifcs6) {
        tabGen<servOpenflowFlw> n = new tabGen<servOpenflowFlw>();
        if (lower.expVrf == null) {
            return n;
        }
        for (int i = 0; i < lower.expIfc.size(); i++) {
            servOpenflowIfc1 ifc = lower.expIfc.get(i);
            if (ifc.id != servOpenflow.tabGrp) {
                continue;
            }
            servOpenflowFlw ntry = new servOpenflowFlw();
            ntry.match = new byte[5];
            ntry.match[0] = 1;
            bits.msbPutD(ntry.match, 1, ifc.grp);
            pckB.clear();
            for (int o = 0; o < lower.expIfc.size(); o++) {
                servOpenflowIfc1 ic = lower.expIfc.get(o);
                if (ic.ifc.bridgeIfc == null) {
                    continue;
                }
                if (ic.ifc.bridgeIfc.lowerBr != ifc.ifc.bridgeHed.bridgeHed) {
                    continue;
                }
                if (ic.ifc.bridgeIfc.blocked) {
                    continue;
                }
                List<typLenVal> tlvs = new ArrayList<typLenVal>();
                tlvs.add(pckO.getActionOutput(ic.id));
                pckO.createBucketAct(pckB, tlvs);
                pckB.merge2end();
            }
            ntry.action = pckB.getCopy();
            addTable(n, tabGroup, ntry);
            ifc.cook = ntry.cookie;
        }
        createGroupMcast(pckB, pckT, pckO, n, true, ifcs4);
        createGroupMcast(pckB, pckT, pckO, n, false, ifcs6);
        for (int i = tabLabel.labels.size() - 1; i >= 0; i--) {
            tabLabelNtry lab = tabLabel.labels.get(i);
            if (lab == null) {
                continue;
            }
            if (lab.forwarder == null) {
                continue;
            }
            if (lab.duplicate == null) {
                continue;
            }
            if (lab.needLocal) {
                continue;
            }
            tabGen<servOpenflowIfc2> ifcs = null;
            if (lab.forwarder == lower.expVrf.fwd4) {
                ifcs = ifcs4;
            }
            if (lab.forwarder == lower.expVrf.fwd6) {
                ifcs = ifcs6;
            }
            if (ifcs == null) {
                continue;
            }
            servOpenflowFlw ntry = new servOpenflowFlw();
            ntry.match = new byte[5];
            ntry.match[0] = 3;
            bits.msbPutD(ntry.match, 1, lab.getValue());
            pckB.clear();
            for (int o = 0; o < lab.duplicate.size(); o++) {
                tabLabelDup dup = lab.duplicate.get(o);
                if (dup == null) {
                    continue;
                }
                int lr = getLabel(dup.lab);
                if (lr < 0) {
                    continue;
                }
                servOpenflowIfc2 ifc = new servOpenflowIfc2(dup.ifc, null, null);
                if (ifc.ifc == null) {
                    continue;
                }
                ifc = ifcs.find(ifc);
                if (ifc == null) {
                    continue;
                }
                if (ifc.ifo.id == servOpenflow.tabGrp) {
                    continue;
                }
                addrMac mac = (addrMac) ifc.ipi.getL2info(dup.hop);
                if (mac == null) {
                    continue;
                }
                List<typLenVal> tlvs = new ArrayList<typLenVal>();
                pckT.clear();
                pckO.createMatchMac(pckT, true, (addrMac) ifc.ifo.ifc.ethtyp.getHwAddr(), null);
                tlvs.add(pckO.getActionSetField(pckT));
                pckT.clear();
                pckO.createMatchMac(pckT, false, mac, null);
                tlvs.add(pckO.getActionSetField(pckT));
                pckT.clear();
                pckO.createMatchMplsLab(pckT, lr);
                tlvs.add(pckO.getActionSetField(pckT));
                tlvs.add(pckO.getActionOutput(ifc.ifo.id));
                pckO.createBucketAct(pckB, tlvs);
                pckB.merge2end();
            }
            ntry.action = pckB.getCopy();
            addTable(n, tabGroup, ntry);
        }
        return n;
    }

    private void createPortPunt(packHolder pckB, packOpenflow pckO, servOpenflowIfc1 ifc, servOpenflowFlw ntry) {
        ntry.prio = 1;
        pckB.clear();
        pckO.createMatchPort(pckB, ifc.id);
        pckB.merge2beg();
        ntry.match = pckB.getCopy();
        List<typLenVal> tlvs = new ArrayList<typLenVal>();
        tlvs.add(pckO.getActionOutput(packOpenflow.cntrlPort));
        pckB.clear();
        pckO.createInstrAct(pckB, tlvs);
        pckB.merge2beg();
        ntry.action = pckB.getCopy();
    }

    private void createPortMcast(tabGen<servOpenflowFlw> n, packHolder pckB, packOpenflow pckO, boolean ipv4, tabGen<servOpenflowIfc2> ifcs) {
        tabGen<ipFwdMcast> groups;
        if (ipv4) {
            groups = lower.expVrf.fwd4.groups;
        } else {
            groups = lower.expVrf.fwd6.groups;
        }
        for (int i = 0; i < groups.size(); i++) {
            ipFwdMcast grp = groups.get(i);
            if (grp == null) {
                continue;
            }
            if (grp.iface == null) {
                continue;
            }
            servOpenflowIfc2 ifc = new servOpenflowIfc2(grp.iface, null, null);
            ifc = ifcs.find(ifc);
            if (ifc == null) {
                continue;
            }
            if (ifc.ifo.id == servOpenflow.tabGrp) {
                continue;
            }
            servOpenflowFlw ntry = new servOpenflowFlw();
            ntry.match = new byte[33];
            ntry.match[0] = 2;
            grp.group.toBuffer(ntry.match, 1);
            grp.source.toBuffer(ntry.match, 17);
            ntry = tabGroup.find(ntry);
            if (ntry == null) {
                continue;
            }
            int cook = ntry.cookie;
            ntry = new servOpenflowFlw();
            ntry.prio = 3;
            pckB.clear();
            pckO.createMatchPort(pckB, ifc.ifo.id);
            if (ipv4) {
                pckO.createMatchEthTyp(pckB, ipIfc4.type);
                pckO.createMatchIpv4(pckB, true, grp.source, null);
                pckO.createMatchIpv4(pckB, false, grp.group, null);
            } else {
                pckO.createMatchEthTyp(pckB, ipIfc6.type);
                pckO.createMatchIpv6(pckB, true, grp.source, null);
                pckO.createMatchIpv6(pckB, false, grp.group, null);
            }
            pckB.merge2beg();
            ntry.match = pckB.getCopy();
            pckB.clear();
            List<typLenVal> tlvs = new ArrayList<typLenVal>();
            tlvs.add(pckO.getActionGroup(cook));
            pckO.createInstrAct(pckB, tlvs);
            pckB.merge2beg();
            ntry.action = pckB.getCopy();
            addTable(n, tabPort, ntry);
        }
    }

    private tabGen<servOpenflowFlw> createPort(packHolder pckB, packOpenflow pckO, tabGen<servOpenflowIfc2> ifcs4, tabGen<servOpenflowIfc2> ifcs6) {
        tabGen<servOpenflowFlw> n = new tabGen<servOpenflowFlw>();
        for (int i = 0; i < lower.expIfc.size(); i++) {
            servOpenflowIfc1 ifc = lower.expIfc.get(i);
            if (ifc.id == servOpenflow.tabGrp) {
                continue;
            }
            servOpenflowFlw ntry = new servOpenflowFlw();
            if (lower.expVrf == null) {
                createPortPunt(pckB, pckO, ifc, ntry);
                addTable(n, tabPort, ntry);
                continue;
            }
            if (ifc.ifc.bridgeIfc != null) {
                servOpenflowIfc1 ic = new servOpenflowIfc1();
                ic.id = servOpenflow.tabGrp;
                ic.grp = ifc.ifc.bridgeHed.num;
                ic = lower.expIfc.find(ic);
                if (ic == null) {
                    createPortPunt(pckB, pckO, ifc, ntry);
                    addTable(n, tabPort, ntry);
                    continue;
                }
                if (ifc.ifc.bridgeIfc.blocked) {
                    createPortPunt(pckB, pckO, ifc, ntry);
                    addTable(n, tabPort, ntry);
                    continue;
                }
                ntry.prio = 2;
                pckB.clear();
                pckO.createMatchPort(pckB, ifc.id);
                pckO.createMatchMac(pckB, false, addrMac.getMultiBase(), addrMac.getMultiBase());
                pckB.merge2beg();
                ntry.match = pckB.getCopy();
                pckB.clear();
                List<typLenVal> tlvs = new ArrayList<typLenVal>();
                tlvs.add(pckO.getActionOutput(packOpenflow.cntrlPort));
                pckO.createInstrAct(pckB, tlvs);
                pckB.merge2beg();
                ntry.action = pckB.getCopy();
                addTable(n, tabPort, ntry);
                ntry = new servOpenflowFlw();
                ntry.prio = 2;
                pckB.clear();
                pckO.createMatchPort(pckB, ifc.id);
                pckO.createMatchEthTyp(pckB, ipMpls.typeU);
                pckO.createMatchMac(pckB, false, (addrMac) ic.ifc.ethtyp.getHwAddr(), null);
                pckB.merge2beg();
                ntry.match = pckB.getCopy();
                pckB.clear();
                pckO.createInstrGoto(pckB, servOpenflow.tabMpls);
                pckB.merge2beg();
                ntry.action = pckB.getCopy();
                addTable(n, tabPort, ntry);
                ntry = new servOpenflowFlw();
                ntry.prio = 2;
                pckB.clear();
                pckO.createMatchPort(pckB, ifc.id);
                pckO.createMatchEthTyp(pckB, ipIfc4.type);
                pckO.createMatchMac(pckB, false, (addrMac) ic.ifc.ethtyp.getHwAddr(), null);
                pckB.merge2beg();
                ntry.match = pckB.getCopy();
                pckB.clear();
                pckO.createInstrGoto(pckB, servOpenflow.tabIpv4);
                pckB.merge2beg();
                ntry.action = pckB.getCopy();
                addTable(n, tabPort, ntry);
                ntry = new servOpenflowFlw();
                ntry.prio = 2;
                pckB.clear();
                pckO.createMatchPort(pckB, ifc.id);
                pckO.createMatchEthTyp(pckB, ipIfc4arp.type);
                pckO.createMatchMac(pckB, false, (addrMac) ic.ifc.ethtyp.getHwAddr(), null);
                pckB.merge2beg();
                ntry.match = pckB.getCopy();
                pckB.clear();
                tlvs = new ArrayList<typLenVal>();
                tlvs.add(pckO.getActionOutput(packOpenflow.cntrlPort));
                pckO.createInstrAct(pckB, tlvs);
                pckB.merge2beg();
                ntry.action = pckB.getCopy();
                addTable(n, tabPort, ntry);
                ntry = new servOpenflowFlw();
                ntry.prio = 2;
                pckB.clear();
                pckO.createMatchPort(pckB, ifc.id);
                pckO.createMatchEthTyp(pckB, ipIfc6.type);
                pckO.createMatchMac(pckB, false, (addrMac) ic.ifc.ethtyp.getHwAddr(), null);
                pckB.merge2beg();
                ntry.match = pckB.getCopy();
                pckB.clear();
                pckO.createInstrGoto(pckB, servOpenflow.tabIpv6);
                pckB.merge2beg();
                ntry.action = pckB.getCopy();
                addTable(n, tabPort, ntry);
                ntry = new servOpenflowFlw();
                ntry.prio = 1;
                pckB.clear();
                pckO.createMatchPort(pckB, ifc.id);
                pckB.merge2beg();
                ntry.match = pckB.getCopy();
                pckB.clear();
                tlvs = new ArrayList<typLenVal>();
                tlvs.add(pckO.getActionGroup(ic.cook));
                pckO.createInstrAct(pckB, tlvs);
                pckB.merge2beg();
                ntry.action = pckB.getCopy();
                addTable(n, tabPort, ntry);
                continue;
            }
            if (ifc.ifc.vrfFor == null) {
                createPortPunt(pckB, pckO, ifc, ntry);
                addTable(n, tabPort, ntry);
                continue;
            }
            if (lower.expVrf.compare(lower.expVrf, ifc.ifc.vrfFor) != 0) {
                createPortPunt(pckB, pckO, ifc, ntry);
                addTable(n, tabPort, ntry);
                continue;
            }
            ntry.prio = 2;
            pckB.clear();
            pckO.createMatchPort(pckB, ifc.id);
            pckO.createMatchMac(pckB, false, addrMac.getMultiBase(), addrMac.getMultiBase());
            pckB.merge2beg();
            ntry.match = pckB.getCopy();
            pckB.clear();
            List<typLenVal> tlvs = new ArrayList<typLenVal>();
            tlvs.add(pckO.getActionOutput(packOpenflow.cntrlPort));
            pckO.createInstrAct(pckB, tlvs);
            pckB.merge2beg();
            ntry.action = pckB.getCopy();
            addTable(n, tabPort, ntry);
            if (ifc.ifc.mplsPack != null) {
                ntry = new servOpenflowFlw();
                ntry.prio = 1;
                pckB.clear();
                pckO.createMatchPort(pckB, ifc.id);
                pckO.createMatchEthTyp(pckB, ipMpls.typeU);
                pckB.merge2beg();
                ntry.match = pckB.getCopy();
                pckB.clear();
                pckO.createInstrGoto(pckB, servOpenflow.tabMpls);
                pckB.merge2beg();
                ntry.action = pckB.getCopy();
                addTable(n, tabPort, ntry);
            }
            if (ifc.ifc.addr4 != null) {
                ntry = new servOpenflowFlw();
                ntry.prio = 1;
                pckB.clear();
                pckO.createMatchPort(pckB, ifc.id);
                pckO.createMatchEthTyp(pckB, ipIfc4.type);
                pckB.merge2beg();
                ntry.match = pckB.getCopy();
                pckB.clear();
                pckO.createInstrGoto(pckB, servOpenflow.tabIpv4);
                pckB.merge2beg();
                ntry.action = pckB.getCopy();
                addTable(n, tabPort, ntry);
                ntry = new servOpenflowFlw();
                ntry.prio = 3;
                pckB.clear();
                pckO.createMatchPort(pckB, ifc.id);
                pckO.createMatchEthTyp(pckB, ipIfc4arp.type);
                pckB.merge2beg();
                ntry.match = pckB.getCopy();
                pckB.clear();
                tlvs = new ArrayList<typLenVal>();
                tlvs.add(pckO.getActionOutput(packOpenflow.cntrlPort));
                pckO.createInstrAct(pckB, tlvs);
                pckB.merge2beg();
                ntry.action = pckB.getCopy();
                addTable(n, tabPort, ntry);
            }
            if (ifc.ifc.addr6 != null) {
                ntry = new servOpenflowFlw();
                ntry.prio = 1;
                pckB.clear();
                pckO.createMatchPort(pckB, ifc.id);
                pckO.createMatchEthTyp(pckB, ipIfc6.type);
                pckB.merge2beg();
                ntry.match = pckB.getCopy();
                pckB.clear();
                pckO.createInstrGoto(pckB, servOpenflow.tabIpv6);
                pckB.merge2beg();
                ntry.action = pckB.getCopy();
                addTable(n, tabPort, ntry);
            }
        }
        if (lower.expVrf != null) {
            createPortMcast(n, pckB, pckO, true, ifcs4);
            createPortMcast(n, pckB, pckO, false, ifcs6);
        }
        addTable(n, tabPort, new servOpenflowFlw());
        return n;
    }

    private void createIpvXpunt(packHolder pckB, packOpenflow pckO, boolean ipv4, tabRouteEntry<addrIP> rou, servOpenflowFlw ntry) {
        ntry.prio = 100 + rou.prefix.maskLen;
        pckB.clear();
        if (ipv4) {
            pckO.createMatchEthTyp(pckB, ipIfc4.type);
            pckO.createMatchIpv4(pckB, false, rou.prefix.network, rou.prefix.mask);
        } else {
            pckO.createMatchEthTyp(pckB, ipIfc6.type);
            pckO.createMatchIpv6(pckB, false, rou.prefix.network, rou.prefix.mask);
        }
        pckB.merge2beg();
        ntry.match = pckB.getCopy();
        List<typLenVal> tlvs = new ArrayList<typLenVal>();
        tlvs.add(pckO.getActionOutput(packOpenflow.cntrlPort));
        pckB.clear();
        pckO.createInstrAct(pckB, tlvs);
        pckB.merge2beg();
        ntry.action = pckB.getCopy();
    }

    private tabGen<servOpenflowFlw> createIpvX(packHolder pckB, packOpenflow pckO, boolean ipv4, tabGen<servOpenflowIfc2> ifcs) {
        tabGen<servOpenflowFlw> n = new tabGen<servOpenflowFlw>();
        if (lower.expVrf == null) {
            if (ipv4) {
                addTable(n, tabIpv4, new servOpenflowFlw());
            } else {
                addTable(n, tabIpv6, new servOpenflowFlw());
            }
            return n;
        }
        tabRoute<addrIP> rous;
        if (ipv4) {
            rous = lower.expVrf.fwd4.actualU;
        } else {
            rous = lower.expVrf.fwd6.actualU;
        }
        for (int i = 0; i < rous.size(); i++) {
            tabRouteEntry<addrIP> rou = rous.get(i);
            if (rou == null) {
                continue;
            }
            servOpenflowFlw ntry = new servOpenflowFlw();
            if (rou.iface == null) {
                createIpvXpunt(pckB, pckO, ipv4, rou, ntry);
                if (ipv4) {
                    addTable(n, tabIpv4, ntry);
                } else {
                    addTable(n, tabIpv6, ntry);
                }
                continue;
            }
            servOpenflowIfc2 ifc = new servOpenflowIfc2((ipFwdIface) rou.iface, null, null);
            ifc = ifcs.find(ifc);
            if (ifc == null) {
                createIpvXpunt(pckB, pckO, ipv4, rou, ntry);
                if (ipv4) {
                    addTable(n, tabIpv4, ntry);
                } else {
                    addTable(n, tabIpv6, ntry);
                }
                continue;
            }
            if (rou.nextHop == null) {
                createIpvXpunt(pckB, pckO, ipv4, rou, ntry);
                if (ipv4) {
                    addTable(n, tabIpv4, ntry);
                } else {
                    addTable(n, tabIpv6, ntry);
                }
                continue;
            }
            addrMac mac = (addrMac) ifc.ipi.getL2info(rou.nextHop);
            if (mac == null) {
                createIpvXpunt(pckB, pckO, ipv4, rou, ntry);
                if (ipv4) {
                    addTable(n, tabIpv4, ntry);
                } else {
                    addTable(n, tabIpv6, ntry);
                }
                continue;
            }
            ntry.prio = 100 + rou.prefix.maskLen;
            pckB.clear();
            if (ipv4) {
                pckO.createMatchEthTyp(pckB, ipIfc4.type);
                pckO.createMatchIpv4(pckB, false, rou.prefix.network, rou.prefix.mask);
            } else {
                pckO.createMatchEthTyp(pckB, ipIfc6.type);
                pckO.createMatchIpv6(pckB, false, rou.prefix.network, rou.prefix.mask);
            }
            pckB.merge2beg();
            ntry.match = pckB.getCopy();
            List<typLenVal> tlvs = new ArrayList<typLenVal>();
            pckB.clear();
            pckO.createMatchMac(pckB, true, (addrMac) ifc.ifo.ifc.ethtyp.getHwAddr(), null);
            tlvs.add(pckO.getActionSetField(pckB));
            pckB.clear();
            pckO.createMatchMac(pckB, false, mac, null);
            tlvs.add(pckO.getActionSetField(pckB));
            int o = getLabel(rou.labelRem);
            if (o >= 0) {
                tlvs.add(pckO.getActionPush(packOpenflow.actionMplsPush, ipMpls.typeU));
                pckB.clear();
                pckO.createMatchMplsLab(pckB, o);
                tlvs.add(pckO.getActionSetField(pckB));
                tlvs.add(pckO.getActionTtl(packOpenflow.actionMplsTtlSet, 255));
            } else {
                tlvs.add(pckO.getActionTtl(packOpenflow.actionIpTtlDec, 0));
            }
            if (ifc.ifo.id == servOpenflow.tabGrp) {
                tlvs.add(pckO.getActionGroup(ifc.ifo.cook));
            } else {
                tlvs.add(pckO.getActionOutput(ifc.ifo.id));
            }
            pckB.clear();
            pckO.createInstrAct(pckB, tlvs);
            pckB.merge2beg();
            ntry.action = pckB.getCopy();
            if (ipv4) {
                addTable(n, tabIpv4, ntry);
            } else {
                addTable(n, tabIpv6, ntry);
            }
        }
        for (int i = 0; i < ifcs.size(); i++) {
            servOpenflowIfc2 ifc = ifcs.get(i);
            if (!ifc.ifc.ready) {
                continue;
            }
            if (!ipv4) {
                addrIP adr = ifc.ipi.getLinkLocalAddr();
                tabRouteEntry<addrIP> rou = new tabRouteEntry<addrIP>();
                rou.prefix = new addrPrefix<addrIP>(adr, addrIP.size * 8);
                servOpenflowFlw ntry = new servOpenflowFlw();
                createIpvXpunt(pckB, pckO, false, rou, ntry);
                addTable(n, tabIpv6, ntry);
            }
            for (int o = 0;; o++) {
                addrIP per = new addrIP();
                addrMac mac = new addrMac();
                if (ifc.ipi.getL2info(o, per, mac)) {
                    break;
                }
                if (!ifc.ipi.checkConnected(per)) {
                    continue;
                }
                servOpenflowFlw ntry = new servOpenflowFlw();
                ntry.prio = 300;
                pckB.clear();
                if (ipv4) {
                    pckO.createMatchEthTyp(pckB, ipIfc4.type);
                    pckO.createMatchIpv4(pckB, false, per, null);
                } else {
                    pckO.createMatchEthTyp(pckB, ipIfc6.type);
                    pckO.createMatchIpv6(pckB, false, per, null);
                }
                pckB.merge2beg();
                ntry.match = pckB.getCopy();
                List<typLenVal> tlvs = new ArrayList<typLenVal>();
                pckB.clear();
                pckO.createMatchMac(pckB, true, (addrMac) ifc.ifo.ifc.ethtyp.getHwAddr(), null);
                tlvs.add(pckO.getActionSetField(pckB));
                pckB.clear();
                pckO.createMatchMac(pckB, false, mac, null);
                tlvs.add(pckO.getActionSetField(pckB));
                if (ifc.ifo.id == servOpenflow.tabGrp) {
                    tlvs.add(pckO.getActionGroup(ifc.ifo.cook));
                } else {
                    tlvs.add(pckO.getActionOutput(ifc.ifo.id));
                }
                pckB.clear();
                pckO.createInstrAct(pckB, tlvs);
                pckB.merge2beg();
                ntry.action = pckB.getCopy();
                if (ipv4) {
                    addTable(n, tabIpv4, ntry);
                } else {
                    addTable(n, tabIpv6, ntry);
                }
            }
        }
        if (ipv4) {
            addTable(n, tabIpv4, new servOpenflowFlw());
        } else {
            addTable(n, tabIpv6, new servOpenflowFlw());
        }
        return n;
    }

    private servOpenflowFlw createMplsPop(packHolder pckB, packOpenflow pckO, int lab, boolean bottom, int tab, int typ) {
        servOpenflowFlw ntry = new servOpenflowFlw();
        ntry.prio = 1;
        if (bottom) {
            ntry.prio++;
        }
        pckB.clear();
        pckO.createMatchEthTyp(pckB, ipMpls.typeU);
        pckO.createMatchMplsLab(pckB, lab);
        if (bottom) {
            pckO.createMatchMplsBos(pckB, bottom);
        }
        pckB.merge2beg();
        ntry.match = pckB.getCopy();
        List<typLenVal> tlvs = new ArrayList<typLenVal>();
        tlvs.add(pckO.getActionPush(packOpenflow.actionMplsPop, typ));
        pckB.clear();
        pckO.createInstrAct(pckB, tlvs);
        pckO.createInstrGoto(pckB, tab);
        pckB.merge2beg();
        ntry.action = pckB.getCopy();
        return ntry;
    }

    private void createMplsPunt(packHolder pckB, packOpenflow pckO, tabLabelNtry lab, servOpenflowFlw ntry) {
        pckB.clear();
        pckO.createMatchEthTyp(pckB, ipMpls.typeU);
        pckO.createMatchMplsLab(pckB, lab.getValue());
        pckB.merge2beg();
        ntry.match = pckB.getCopy();
        List<typLenVal> tlvs = new ArrayList<typLenVal>();
        tlvs.add(pckO.getActionOutput(packOpenflow.cntrlPort));
        pckB.clear();
        pckO.createInstrAct(pckB, tlvs);
        pckB.merge2beg();
        ntry.action = pckB.getCopy();
    }

    private tabGen<servOpenflowFlw> createMpls(packHolder pckB, packOpenflow pckO, tabGen<servOpenflowIfc2> ifcs4, tabGen<servOpenflowIfc2> ifcs6) {
        tabGen<servOpenflowFlw> n = new tabGen<servOpenflowFlw>();
        if (lower.expVrf == null) {
            addTable(n, tabMpls, new servOpenflowFlw());
            return n;
        }
        addTable(n, tabMpls, createMplsPop(pckB, pckO, ipMpls.labelExp4, true, servOpenflow.tabIpv4, ipIfc4.type));
        addTable(n, tabMpls, createMplsPop(pckB, pckO, ipMpls.labelExp6, true, servOpenflow.tabIpv6, ipIfc6.type));
        addTable(n, tabMpls, createMplsPop(pckB, pckO, lower.expVrf.fwd4.commonLabel.getValue(), true, servOpenflow.tabIpv4, ipIfc4.type));
        addTable(n, tabMpls, createMplsPop(pckB, pckO, lower.expVrf.fwd6.commonLabel.getValue(), true, servOpenflow.tabIpv6, ipIfc6.type));
        for (int i = tabLabel.labels.size() - 1; i >= 0; i--) {
            tabLabelNtry lab = tabLabel.labels.get(i);
            if (lab == null) {
                continue;
            }
            if (lab.forwarder == null) {
                continue;
            }
            servOpenflowFlw ntry = new servOpenflowFlw();
            ntry.prio = 1;
            int proto = 0;
            int typ = 0;
            if (lab.forwarder == lower.expVrf.fwd4) {
                if (lab.compare(lab, lower.expVrf.fwd4.commonLabel) == 0) {
                    continue;
                }
                proto = 1;
                typ = ipIfc4.type;
            }
            if (lab.forwarder == lower.expVrf.fwd6) {
                if (lab.compare(lab, lower.expVrf.fwd6.commonLabel) == 0) {
                    continue;
                }
                proto = 2;
                typ = ipIfc6.type;
            }
            if (proto < 1) {
                createMplsPunt(pckB, pckO, lab, ntry);
                addTable(n, tabMpls, ntry);
                continue;
            }
            if (lab.duplicate != null) {
                ntry.match = new byte[5];
                ntry.match[0] = 3;
                bits.msbPutD(ntry.match, 1, lab.getValue());
                ntry = tabGroup.find(ntry);
                if (ntry == null) {
                    createMplsPunt(pckB, pckO, lab, ntry);
                    addTable(n, tabMpls, ntry);
                }
                int cook = ntry.cookie;
                ntry = new servOpenflowFlw();
                ntry.prio = 1;
                pckB.clear();
                pckO.createMatchEthTyp(pckB, ipMpls.typeU);
                pckO.createMatchMplsLab(pckB, lab.getValue());
                pckB.merge2beg();
                ntry.match = pckB.getCopy();
                List<typLenVal> tlvs = new ArrayList<typLenVal>();
                tlvs.add(pckO.getActionGroup(cook));
                pckB.clear();
                pckO.createInstrAct(pckB, tlvs);
                pckB.merge2beg();
                ntry.action = pckB.getCopy();
                addTable(n, tabMpls, ntry);
                continue;
            }
            if (lab.iface == null) {
                createMplsPunt(pckB, pckO, lab, ntry);
                addTable(n, tabMpls, ntry);
                continue;
            }
            if (lab.nextHop == null) {
                createMplsPunt(pckB, pckO, lab, ntry);
                addTable(n, tabMpls, ntry);
                continue;
            }
            servOpenflowIfc2 ifc = new servOpenflowIfc2(lab.iface, null, null);
            if (proto == 1) {
                ifc = ifcs4.find(ifc);
            } else {
                ifc = ifcs6.find(ifc);
            }
            if (ifc == null) {
                createMplsPunt(pckB, pckO, lab, ntry);
                addTable(n, tabMpls, ntry);
                continue;
            }
            addrMac macR = (addrMac) ifc.ipi.getL2info(lab.nextHop);
            if (macR == null) {
                createMplsPunt(pckB, pckO, lab, ntry);
                addTable(n, tabMpls, ntry);
                continue;
            }
            int lr = getLabel(lab.remoteLab);
            addrMac macL = (addrMac) ifc.ifo.ifc.ethtyp.getHwAddr();
            ntry.prio = 2;
            pckB.clear();
            pckO.createMatchEthTyp(pckB, ipMpls.typeU);
            pckO.createMatchMplsLab(pckB, lab.getValue());
            pckO.createMatchMplsBos(pckB, true);
            pckB.merge2beg();
            ntry.match = pckB.getCopy();
            List<typLenVal> tlvs = new ArrayList<typLenVal>();
            pckB.clear();
            pckO.createMatchMac(pckB, true, macL, null);
            tlvs.add(pckO.getActionSetField(pckB));
            pckB.clear();
            pckO.createMatchMac(pckB, false, macR, null);
            tlvs.add(pckO.getActionSetField(pckB));
            if (lr >= 0) {
                pckB.clear();
                pckO.createMatchMplsLab(pckB, lr);
                tlvs.add(pckO.getActionSetField(pckB));
                tlvs.add(pckO.getActionTtl(packOpenflow.actionMplsTtlDec, 0));
            } else {
                tlvs.add(pckO.getActionPush(packOpenflow.actionMplsPop, typ));
            }
            if (ifc.ifo.id == servOpenflow.tabGrp) {
                tlvs.add(pckO.getActionGroup(ifc.ifo.cook));
            } else {
                tlvs.add(pckO.getActionOutput(ifc.ifo.id));
            }
            pckB.clear();
            pckO.createInstrAct(pckB, tlvs);
            pckB.merge2beg();
            ntry.action = pckB.getCopy();
            addTable(n, tabMpls, ntry);
            ntry = new servOpenflowFlw();
            ntry.prio = 1;
            pckB.clear();
            pckO.createMatchEthTyp(pckB, ipMpls.typeU);
            pckO.createMatchMplsLab(pckB, lab.getValue());
            pckB.merge2beg();
            ntry.match = pckB.getCopy();
            tlvs = new ArrayList<typLenVal>();
            pckB.clear();
            pckO.createMatchMac(pckB, true, macL, null);
            tlvs.add(pckO.getActionSetField(pckB));
            pckB.clear();
            pckO.createMatchMac(pckB, false, macR, null);
            tlvs.add(pckO.getActionSetField(pckB));
            if (lr >= 0) {
                pckB.clear();
                pckO.createMatchMplsLab(pckB, lr);
                tlvs.add(pckO.getActionSetField(pckB));
                tlvs.add(pckO.getActionTtl(packOpenflow.actionMplsTtlDec, 0));
            } else {
                tlvs.add(pckO.getActionPush(packOpenflow.actionMplsPop, ipMpls.typeU));
            }
            if (ifc.ifo.id == servOpenflow.tabGrp) {
                tlvs.add(pckO.getActionGroup(ifc.ifo.cook));
            } else {
                tlvs.add(pckO.getActionOutput(ifc.ifo.id));
            }
            pckB.clear();
            pckO.createInstrAct(pckB, tlvs);
            pckB.merge2beg();
            ntry.action = pckB.getCopy();
            addTable(n, tabMpls, ntry);
        }
        addTable(n, tabMpls, new servOpenflowFlw());
        return n;
    }

    private void doWord() {
        packHolder pckB = new packHolder(true, true);
        packHolder pckT = new packHolder(true, true);
        packOpenflow pckO = new packOpenflow();
        pckO.pipe = pipe;
        pckO.createHello();
        lower.sendPack(pckB, pckO);
        pckB.clear();
        pckO.createFeatures(pckB);
        lower.sendPack(pckB, pckO);
        pckB.clear();
        pckO.createMultipart(pckB, 13, 0);
        lower.sendPack(pckB, pckO);
        pckB.clear();
        pckO.createGroupMod(pckB, packOpenflow.groupCmdDel, packOpenflow.groupTypAll, 0xfffffffc);
        lower.sendPack(pckB, pckO);
        pckB.clear();
        pckO.putMatchBuf(pckB, new byte[0]);
        pckO.createFlowMod(pckB, 0, 0xff, packOpenflow.flowCmdDel, 0);
        lower.sendPack(pckB, pckO);
        for (int i = 0; i < lower.expIfc.size(); i++) {
            lower.expIfc.get(i).sendState(0);
        }
        for (;;) {
            if (!working) {
                return;
            }
            if (pipe.isClosed() != 0) {
                return;
            }
            tabGen<servOpenflowIfc2> ifc4 = new tabGen<servOpenflowIfc2>();
            tabGen<servOpenflowIfc2> ifc6 = new tabGen<servOpenflowIfc2>();
            if (lower.expVrf != null) {
                for (int i = 0; i < lower.expIfc.size(); i++) {
                    servOpenflowIfc1 ifc = lower.expIfc.get(i);
                    if (ifc.ifc.vrfFor == null) {
                        continue;
                    }
                    if (lower.expVrf.compare(lower.expVrf, ifc.ifc.vrfFor) != 0) {
                        continue;
                    }
                    if (ifc.ifc.addr4 != null) {
                        ifc4.add(new servOpenflowIfc2(ifc.ifc.fwdIf4, ifc.ifc.ipIf4, ifc));
                    }
                    if (ifc.ifc.addr6 != null) {
                        ifc6.add(new servOpenflowIfc2(ifc.ifc.fwdIf6, ifc.ifc.ipIf6, ifc));
                    }
                }
            }
            sendGroup(pckB, pckO, tabGroup, createGroup(pckB, pckT, pckO, ifc4, ifc6));
            sendTable(pckB, pckO, servOpenflow.tabPort, tabPort, createPort(pckB, pckO, ifc4, ifc6));
            sendTable(pckB, pckO, servOpenflow.tabMpls, tabMpls, createMpls(pckB, pckO, ifc4, ifc6));
            sendTable(pckB, pckO, servOpenflow.tabIpv4, tabIpv4, createIpvX(pckB, pckO, true, ifc4));
            sendTable(pckB, pckO, servOpenflow.tabIpv6, tabIpv6, createIpvX(pckB, pckO, false, ifc6));
            lower.notif.misleep(0);
        }
    }
}
