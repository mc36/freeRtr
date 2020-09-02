package serv;

import java.util.Comparator;
import addr.addrType;
import addr.addrIP;
import addr.addrIPv4;
import addr.addrIPv6;
import addr.addrMac;
import addr.addrPrefix;
import cfg.cfgAceslst;
import cfg.cfgAll;
import cfg.cfgBndl;
import cfg.cfgBrdg;
import cfg.cfgIfc;
import cfg.cfgVrf;
import clnt.clntMplsPwe;
import clnt.clntVxlan;
import ifc.ifcBridge;
import ifc.ifcBridgeAdr;
import ifc.ifcBridgeIfc;
import ifc.ifcUp;
import ifc.ifcDn;
import ifc.ifcEther;
import ifc.ifcEthTyp;
import ifc.ifcNull;
import ip.ipFwd;
import ip.ipIfc;
import ip.ipMpls;
import java.util.ArrayList;
import java.util.List;
import pack.packHolder;
import pipe.pipeLine;
import pipe.pipeSide;
import prt.prtGenConn;
import prt.prtServS;
import prt.prtTcp;
import prt.prtUdp;
import rtr.rtrBgpEvpnPeer;
import tab.tabAceslstN;
import tab.tabGen;
import tab.tabIntMatcher;
import tab.tabLabel;
import tab.tabLabelNtry;
import tab.tabListing;
import tab.tabListingEntry;
import tab.tabNatCfgN;
import tab.tabNatTraN;
import tab.tabRoute;
import tab.tabRouteEntry;
import tab.tabRouteIface;
import user.userFilter;
import user.userHelping;
import util.cmds;
import util.counter;
import util.debugger;
import util.logger;
import util.state;
import util.bits;
import util.history;
import util.notifier;

/**
 * p4lang
 *
 * @author matecsaba
 */
public class servP4lang extends servGeneric implements ifcUp, prtServS {

    /**
     * port
     */
    public final static int port = 9080;

    /**
     * exported vrfs
     */
    public tabGen<servP4langVrf> expVrf = new tabGen<servP4langVrf>();

    /**
     * exported interfaces
     */
    public tabGen<servP4langIfc> expIfc = new tabGen<servP4langIfc>();

    /**
     * exported srv6
     */
    public cfgIfc expSrv6 = null;

    /**
     * exported bridges
     */
    public tabGen<servP4langBr> expBr = new tabGen<servP4langBr>();

    /**
     * exported copp
     */
    public tabListing<tabAceslstN<addrIP>, addrIP> expCopp4 = null;

    /**
     * exported copp
     */
    public tabListing<tabAceslstN<addrIP>, addrIP> expCopp6 = null;

    /**
     * exported dynamic interface first
     */
    public int expDyn1st = 0;

    /**
     * exported dynamic range size
     */
    public int expDynSiz = 0;

    /**
     * exported dynamic interfaces
     */
    public ifcBridgeIfc[] expDynIfc;

    /**
     * exported dynamic next
     */
    public int expDynNxt = 0;

    /**
     * export interval
     */
    public int expDelay = 1000;

    /**
     * last connection
     */
    protected servP4langConn conn = null;

    /**
     * interconnection interface
     */
    protected ifcEthTyp interconn = null;

    /**
     * counter
     */
    protected counter cntr = new counter();

    /**
     * counter
     */
    protected notifier notif = new notifier();

    private ifcDn intercon;

    /**
     * defaults text
     */
    public final static String defaultL[] = {
        "server p4lang .*! port " + port,
        "server p4lang .*! protocol " + proto2string(protoAllStrm),
        "server p4lang .*! no export-srv6",
        "server p4lang .*! no export-copp4",
        "server p4lang .*! no export-copp6",
        "server p4lang .*! no export-dynamic",
        "server p4lang .*! no interconnect",
        "server p4lang .*! export-interval 1000",};

    /**
     * defaults filter
     */
    public static tabGen<userFilter> defaultF;

    public tabGen<userFilter> srvDefFlt() {
        return defaultF;
    }

    public void srvShRun(String beg, List<String> l) {
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
            l.add(beg + "export-port " + ntry.ifc.name + " " + ntry.id + " " + ntry.speed);
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
        cmds.cfgLine(l, expDynSiz < 1, beg, "export-dynamic", expDyn1st + " " + expDynSiz);
        l.add(beg + "export-interval " + expDelay);
        cmds.cfgLine(l, interconn == null, beg, "interconnect", "" + interconn);
    }

    public boolean srvCfgStr(cmds cmd) {
        String s = cmd.word();
        if (s.equals("export-vrf")) {
            cfgVrf vrf = cfgAll.vrfFind(cmd.word(), false);
            if (vrf == null) {
                cmd.error("no such vrf");
                return false;
            }
            servP4langVrf ntry = new servP4langVrf();
            ntry.vrf = vrf;
            ntry.id = bits.str2num(cmd.word());
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
            servP4langBr ntry = new servP4langBr();
            ntry.id = br.num;
            ntry.br = br;
            expBr.put(ntry);
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
        if (s.equals("export-dynamic")) {
            expDyn1st = bits.str2num(cmd.word());
            expDynSiz = bits.str2num(cmd.word());
            expDynIfc = new ifcBridgeIfc[expDynSiz];
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
            if ((ifc.type != cfgIfc.ifaceType.sdn) && (ifc.type != cfgIfc.ifaceType.bundle) && (ifc.type != cfgIfc.ifaceType.bridge) && (ifc.type != cfgIfc.ifaceType.dialer) && (ifc.type != cfgIfc.ifaceType.hairpin) && (ifc.type != cfgIfc.ifaceType.tunnel) && (ifc.type != cfgIfc.ifaceType.virtppp)) {
                cmd.error("not p4lang interface");
                return false;
            }
            servP4langIfc ntry = new servP4langIfc();
            ntry.id = bits.str2num(cmd.word());
            ntry.ifc = ifc;
            ntry.speed = bits.str2num(cmd.word());
            ntry.lower = this;
            boolean need = ifc.type == cfgIfc.ifaceType.sdn;
            if (ifc.vlanNum > 0) {
                need = false;
                for (int i = 0; i < expIfc.size(); i++) {
                    servP4langIfc old = expIfc.get(i);
                    if (old.master != null) {
                        continue;
                    }
                    if (old.ifc == ifc.parent) {
                        ntry.master = old;
                        break;
                    }
                }
                if (ntry.master == null) {
                    cmd.error("carrier not exported");
                    return false;
                }
            }
            if (need) {
                ntry.setUpper(ifc.ethtyp);
            }
            ntry.ifc.ethtyp.hwHstry = new history();
            ntry.ifc.ethtyp.hwCntr = new counter();
            expIfc.put(ntry);
            return false;
        }
        if (!s.equals("no")) {
            return true;
        }
        s = cmd.word();
        if (s.equals("export-vrf")) {
            cfgVrf vrf = cfgAll.vrfFind(cmd.word(), false);
            if (vrf == null) {
                cmd.error("no such vrf");
                return false;
            }
            vrf.fwd4.tableChanged = null;
            vrf.fwd6.tableChanged = null;
            servP4langVrf ntry = new servP4langVrf();
            ntry.vrf = vrf;
            ntry.id = bits.str2num(cmd.word());
            expVrf.del(ntry);
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
        if (s.equals("export-bridge")) {
            cfgBrdg br = cfgAll.brdgFind(cmd.word(), false);
            if (br == null) {
                cmd.error("no such bridge");
                return false;
            }
            servP4langBr ntry = new servP4langBr();
            ntry.id = br.num;
            ntry.br = br;
            expBr.del(ntry);
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
        if (s.equals("export-dynamic")) {
            expDyn1st = 0;
            expDynSiz = 0;
            expDynIfc = null;
            return false;
        }
        if (s.equals("export-srv6")) {
            expSrv6 = null;
            return false;
        }
        if (s.equals("export-port")) {
            cfgIfc ifc = cfgAll.ifcFind(cmd.word(), false);
            if (ifc == null) {
                cmd.error("no such interface");
                return false;
            }
            servP4langIfc ntry = new servP4langIfc();
            ntry.id = bits.str2num(cmd.word());
            ntry.ifc = ifc;
            ntry = expIfc.del(ntry);
            if (ntry == null) {
                return false;
            }
            if ((ifc.type == cfgIfc.ifaceType.sdn) && (ifc.vlanNum == 0)) {
                ifcNull nul = new ifcNull();
                nul.setUpper(ifc.ethtyp);
            }
            return false;
        }
        return true;
    }

    public void srvHelp(userHelping l) {
        l.add("1 2  export-vrf                specify vrf to export");
        l.add("2 3    <name>                  vrf name");
        l.add("3 .      <num>                 p4lang vrf number");
        l.add("1 2  export-dynamic            specify dynamic port range");
        l.add("2 3    <num>                   first id");
        l.add("3 .      <num>                 number of ids");
        l.add("1 2  export-bridge             specify bridge to export");
        l.add("2 .    <num>                   bridge number");
        l.add("1 2  export-port               specify port to export");
        l.add("2 3    <name>                  interface name");
        l.add("3 4,.    <num>                 p4lang port number");
        l.add("4 .        <num>               p4lang port type");
        l.add("1 2  export-srv6               specify srv6 to export");
        l.add("2 .    <name>                  interface name");
        l.add("1 2  export-copp4              specify copp acl to export");
        l.add("2 .    <name>                  acl name");
        l.add("1 2  export-copp6              specify copp acl to export");
        l.add("2 .    <name>                  acl name");
        l.add("1 2  export-interval           specify export interval");
        l.add("2 .    <num>                   time in ms");
        l.add("1 2  interconnect              specify port to for packetin");
        l.add("2 .    <name>                  interface name");
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
        return genStrmStart(this, new pipeLine(32768, false), 0);
    }

    public boolean srvDeinit() {
        return genericStop(0);
    }

    public boolean srvAccept(pipeSide pipe, prtGenConn id) {
        if (conn != null) {
            conn.pipe.setClose();
            notif.wakeup();
        }
        id.timeout = 120000;
        pipe.lineRx = pipeSide.modTyp.modeCRorLF;
        pipe.lineTx = pipeSide.modTyp.modeLF;
        for (int i = 0; i < expIfc.size(); i++) {
            servP4langIfc ifc = expIfc.get(i);
            ifc.lastState = state.states.up;
            ifc.upper.setState(ifc.lastState);
            ifc.sentVlan = 0;
            ifc.sentBundle = 0;
            ifc.sentHairpin = 0;
            ifc.sentPppoe = -1;
            ifc.sentVrf = 0;
            ifc.sentState = state.states.close;
            ifc.sentMtu = 0;
            ifc.sentLabel = -1;
            ifc.sentAcl4in = null;
            ifc.sentAcl4out = null;
            ifc.sentAcl6in = null;
            ifc.sentAcl6out = null;
        }
        for (int i = 0; i < expVrf.size(); i++) {
            servP4langVrf vrf = expVrf.get(i);
            vrf.routes4.clear();
            vrf.routes6.clear();
            vrf.sentMcast = false;
            vrf.natCfg4 = null;
            vrf.natCfg6 = null;
            vrf.natTrns4.clear();
            vrf.natTrns6.clear();
        }
        for (int i = 0; i < expBr.size(); i++) {
            servP4langBr br = expBr.get(i);
            br.ifcs.clear();
            br.macs.clear();
        }
        expDynNxt = 0;
        conn = new servP4langConn(pipe, this);
        logger.warn("neighbor " + id.peerAddr + " up");
        return false;
    }

    /**
     * send line
     *
     * @param a line
     */
    protected synchronized void sendLine(String a) {
        if (conn == null) {
            return;
        }
        if (debugger.servP4langTx) {
            logger.debug("tx: " + a);
        }
        conn.pipe.linePut(a + " ");
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
        if (intercon != null) {
            pckB.msbPutW(0, id);
            pckB.putSkip(2);
            pckB.merge2beg();
            ifcEther.parseETHheader(pckB, false);
            intercon.sendPack(pckB);
            return;
        }
        String a = "packet " + id + " ";
        byte[] data = pckB.getCopy();
        for (int i = 0; i < data.length; i++) {
            a += bits.toHexB(data[i]);
        }
        sendLine(a);
    }

    /**
     * process cpu packet
     *
     * @param id port id
     * @param pck packet
     */
    protected void gotCpuPack(int id, packHolder pck) {
        ifcEther.parseETHheader(pck, false);
        if (expDynIfc != null) {
            if ((id >= expDyn1st) && (id < (expDyn1st + expDynSiz))) {
                if (pck.msbGetW(0) == ifcBridge.serialType) {
                    pck.getSkip(2);
                } else {
                    pck.getSkip(-addrMac.sizeX2);
                }
                ifcBridgeIfc ifc = expDynIfc[id - expDyn1st];
                if (ifc == null) {
                    cntr.drop(pck, counter.reasons.noIface);
                    return;
                }
                ifc.recvPack(pck);
                return;
            }
        }
        servP4langIfc ntry = new servP4langIfc();
        ntry.id = id;
        ntry = expIfc.find(ntry);
        if (ntry == null) {
            cntr.drop(pck, counter.reasons.noIface);
            return;
        }
        if (ntry.ifc.type != cfgIfc.ifaceType.sdn) {
            ntry.ifc.ethtyp.recvPack(pck);
        } else {
            ntry.upper.recvPack(pck);
        }
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
        gotCpuPack(id, pck);
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

}

class servP4langNei implements Comparator<servP4langNei> {

    public addrIP adr = new addrIP();

    public addrMac mac = null;

    public servP4langIfc iface;

    public servP4langVrf vrf;

    public int sentIfc;

    public int sentTun;

    public int id;

    public int need;

    public int compare(servP4langNei o1, servP4langNei o2) {
        int i = o1.iface.compare(o1.iface, o2.iface);
        if (i != 0) {
            return i;
        }
        if ((o1.iface.ifc.type == cfgIfc.ifaceType.dialer) || (o1.iface.ifc.type == cfgIfc.ifaceType.tunnel) || (o1.iface.ifc.type == cfgIfc.ifaceType.virtppp)) {
            return 0;
        }
        return o1.adr.compare(o1.adr, o2.adr);
    }

}

class servP4langBr implements Comparator<servP4langBr> {

    public int id;

    public cfgBrdg br;

    public boolean routed;

    public tabGen<ifcBridgeAdr> macs = new tabGen<ifcBridgeAdr>();

    public tabGen<ifcBridgeIfc> ifcs = new tabGen<ifcBridgeIfc>();

    public int compare(servP4langBr o1, servP4langBr o2) {
        if (o1.id < o2.id) {
            return -1;
        }
        if (o1.id > o2.id) {
            return +1;
        }
        return 0;
    }

    public boolean findIfc(int lab) {
        for (int i = 0; i < ifcs.size(); i++) {
            try {
                rtrBgpEvpnPeer ifc = (rtrBgpEvpnPeer) ifcs.get(i).lowerIf;
                if (ifc.getLabelLoc() == lab) {
                    return true;
                }
            } catch (Exception e) {
            }
        }
        return false;
    }
}

class servP4langVrf implements Comparator<servP4langVrf> {

    public int id;

    public cfgVrf vrf;

    public boolean sentMcast;

    public tabRoute<addrIP> routes4 = new tabRoute<addrIP>("sent");

    public tabRoute<addrIP> routes6 = new tabRoute<addrIP>("sent");

    public tabListing<tabAceslstN<addrIP>, addrIP> natCfg4;

    public tabListing<tabAceslstN<addrIP>, addrIP> natCfg6;

    public tabGen<tabNatTraN> natTrns4 = new tabGen<tabNatTraN>();

    public tabGen<tabNatTraN> natTrns6 = new tabGen<tabNatTraN>();

    public int compare(servP4langVrf o1, servP4langVrf o2) {
        if (o1.id < o2.id) {
            return -1;
        }
        if (o1.id > o2.id) {
            return +1;
        }
        return 0;
    }

}

class servP4langIfc implements ifcDn, Comparator<servP4langIfc> {

    public servP4lang lower;

    public int id;

    public int speed;

    public int sentVrf;

    public int sentVlan;

    public int sentBundle;

    public int sentHairpin;

    public int sentMtu;

    public int sentPppoe;

    public int sentLabel;

    public state.states sentState = state.states.close;

    public tabListing<tabAceslstN<addrIP>, addrIP> sentAcl4in;

    public tabListing<tabAceslstN<addrIP>, addrIP> sentAcl4out;

    public tabListing<tabAceslstN<addrIP>, addrIP> sentAcl6in;

    public tabListing<tabAceslstN<addrIP>, addrIP> sentAcl6out;

    public servP4langIfc master;

    public servP4langIfc pppoe;

    public cfgIfc ifc;

    public ifcUp upper = new ifcNull();

    public counter cntr = new counter();

    public state.states lastState = state.states.up;

    public int compare(servP4langIfc o1, servP4langIfc o2) {
        if (o1.id < o2.id) {
            return -1;
        }
        if (o1.id > o2.id) {
            return +1;
        }
        return 0;
    }

    public String toString() {
        return "p4lang port " + id;
    }

    public addrType getHwAddr() {
        return addrMac.getRandom();
    }

    public void setFilter(boolean promisc) {
    }

    public state.states getState() {
        return lastState;
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
        lower.sendPack(id, pck);
    }

}

class servP4langConn implements Runnable {

    public pipeSide pipe;

    public servP4lang lower;

    public int keepalive;

    public tabGen<servP4langNei> neighs = new tabGen<servP4langNei>();

    public tabGen<tabLabelNtry> labels = new tabGen<tabLabelNtry>();

    public tabListing<tabAceslstN<addrIP>, addrIP> copp4;

    public tabListing<tabAceslstN<addrIP>, addrIP> copp6;

    public servP4langConn(pipeSide pip, servP4lang upper) {
        pipe = pip;
        lower = upper;
        new Thread(this).start();
    }

    public void run() {
        try {
            for (;;) {
                if (doRound()) {
                    break;
                }
            }
        } catch (Exception e) {
            logger.traceback(e);
        }
        pipe.setClose();
    }

    private void updateTrans(cmds cmd, ipFwd fwd) {
        tabNatTraN ntry = new tabNatTraN();
        ntry.protocol = bits.str2num(cmd.word());
        addrIP adr = new addrIP();
        adr.fromString(cmd.word());
        ntry.origSrcAddr = adr;
        adr = new addrIP();
        adr.fromString(cmd.word());
        ntry.origTrgAddr = adr;
        ntry.origSrcPort = bits.str2num(cmd.word());
        ntry.origTrgPort = bits.str2num(cmd.word());
        ntry = fwd.natTrns.find(ntry);
        if (ntry == null) {
            return;
        }
        counter old = ntry.hwCntr;
        ntry.hwCntr = new counter();
        ntry.hwCntr.packRx = bits.str2long(cmd.word());
        ntry.hwCntr.byteRx = bits.str2long(cmd.word());
        if (old == null) {
            return;
        }
        if (old.compare(old, ntry.hwCntr) == 0) {
            return;
        }
        ntry.lastUsed = bits.getTime();
        ntry.reverse.lastUsed = ntry.lastUsed;
    }

    private boolean doRound() {
        if (pipe.ready2rx() > 0) {
            String s = pipe.lineGet(0x11);
            if (debugger.servP4langRx) {
                logger.debug("rx: " + s);
            }
            cmds cmd = new cmds("p4lang", s);
            s = cmd.word();
            if (s.equals("nattrns4_cnt")) {
                servP4langVrf vrf = new servP4langVrf();
                vrf.id = bits.str2num(cmd.word());
                vrf = lower.expVrf.find(vrf);
                if (vrf == null) {
                    return false;
                }
                updateTrans(cmd, vrf.vrf.fwd4);
                return false;
            }
            if (s.equals("nattrns6_cnt")) {
                servP4langVrf vrf = new servP4langVrf();
                vrf.id = bits.str2num(cmd.word());
                vrf = lower.expVrf.find(vrf);
                if (vrf == null) {
                    return false;
                }
                updateTrans(cmd, vrf.vrf.fwd6);
                return false;
            }
            if (s.equals("bridge_cnt")) {
                servP4langBr br = new servP4langBr();
                br.id = bits.str2num(cmd.word());
                br = lower.expBr.find(br);
                if (br == null) {
                    return false;
                }
                addrMac mac = new addrMac();
                mac.fromString(cmd.word());
                ifcBridgeAdr ntry = br.br.bridgeHed.findMacAddr(mac);
                if (ntry == null) {
                    return false;
                }
                counter old = ntry.hwCntr;
                ntry.hwCntr = new counter();
                ntry.hwCntr.packTx = bits.str2long(cmd.word());
                ntry.hwCntr.byteTx = bits.str2long(cmd.word());
                if (old == null) {
                    return false;
                }
                if (old.compare(old, ntry.hwCntr) == 0) {
                    return false;
                }
                ntry.time = bits.getTime();
                return false;
            }
            if (s.equals("route4_cnt")) {
                servP4langVrf vrf = new servP4langVrf();
                vrf.id = bits.str2num(cmd.word());
                vrf = lower.expVrf.find(vrf);
                if (vrf == null) {
                    return false;
                }
                addrIPv4 adr = new addrIPv4();
                adr.fromString(cmd.word());
                addrPrefix<addrIPv4> prf = new addrPrefix<addrIPv4>(adr, bits.str2num(cmd.word()));
                tabRouteEntry<addrIP> ntry = vrf.vrf.fwd4.actualU.find(addrPrefix.ip4toIP(prf));
                if (ntry == null) {
                    return false;
                }
                ntry.hwCntr = new counter();
                ntry.hwCntr.packTx = bits.str2long(cmd.word());
                ntry.hwCntr.byteTx = bits.str2long(cmd.word());
                return false;
            }
            if (s.equals("route6_cnt")) {
                servP4langVrf vrf = new servP4langVrf();
                vrf.id = bits.str2num(cmd.word());
                vrf = lower.expVrf.find(vrf);
                if (vrf == null) {
                    return false;
                }
                addrIPv6 adr = new addrIPv6();
                adr.fromString(cmd.word());
                addrPrefix<addrIPv6> prf = new addrPrefix<addrIPv6>(adr, bits.str2num(cmd.word()));
                tabRouteEntry<addrIP> ntry = vrf.vrf.fwd6.actualU.find(addrPrefix.ip6toIP(prf));
                if (ntry == null) {
                    return false;
                }
                ntry.hwCntr = new counter();
                ntry.hwCntr.packTx = bits.str2long(cmd.word());
                ntry.hwCntr.byteTx = bits.str2long(cmd.word());
                return false;
            }
            if (s.equals("mpls_cnt")) {
                tabLabelNtry ntry = new tabLabelNtry(bits.str2num(cmd.word()));
                ntry = tabLabel.labels.find(ntry);
                if (ntry == null) {
                    return false;
                }
                ntry.hwCntr = new counter();
                ntry.hwCntr.packRx = bits.str2long(cmd.word());
                ntry.hwCntr.byteRx = bits.str2long(cmd.word());
                return false;
            }
            if (s.equals("counter")) {
                servP4langIfc ntry = new servP4langIfc();
                ntry.id = bits.str2num(cmd.word());
                ntry = lower.expIfc.find(ntry);
                if (ntry == null) {
                    return false;
                }
                ntry.ifc.ethtyp.hwCntr.packRx = bits.str2long(cmd.word());
                ntry.ifc.ethtyp.hwCntr.byteRx = bits.str2long(cmd.word());
                ntry.ifc.ethtyp.hwCntr.packTx = bits.str2long(cmd.word());
                ntry.ifc.ethtyp.hwCntr.byteTx = bits.str2long(cmd.word());
                ntry.ifc.ethtyp.hwCntr.packDr = bits.str2long(cmd.word());
                ntry.ifc.ethtyp.hwCntr.byteDr = bits.str2long(cmd.word());
                return false;
            }
            if (s.equals("state")) {
                servP4langIfc ntry = new servP4langIfc();
                ntry.id = bits.str2num(cmd.word());
                ntry = lower.expIfc.find(ntry);
                if (ntry == null) {
                    return false;
                }
                if (cmd.word().equals("1")) {
                    ntry.lastState = state.states.up;
                } else {
                    ntry.lastState = state.states.down;
                }
                ntry.upper.setState(ntry.lastState);
                return false;
            }
            if (!s.equals("packet")) {
                return false;
            }
            int id = bits.str2num(cmd.word());
            packHolder pck = new packHolder(true, true);
            s = cmd.getRemaining();
            s = s.replaceAll(" ", "");
            for (;;) {
                if (s.length() < 2) {
                    break;
                }
                pck.putByte(0, bits.fromHex(s.substring(0, 2)));
                s = s.substring(2, s.length());
                pck.putSkip(1);
                pck.merge2end();
            }
            lower.gotCpuPack(id, pck);
            return false;
        }
        if (pipe.isClosed() != 0) {
            return true;
        }
        for (int i = 0; i < neighs.size(); i++) {
            neighs.get(i).need = 0;
        }
        keepalive++;
        if (keepalive > 30) {
            String a = "keepalive";
            lower.sendLine(a);
            keepalive = 0;
        }
        if (copp4 != lower.expCopp4) {
            sendAcl("copp4_del ", true, copp4);
            copp4 = lower.expCopp4;
            sendAcl("copp4_add ", true, copp4);
        }
        if (copp6 != lower.expCopp6) {
            sendAcl("copp6_del ", false, copp6);
            copp6 = lower.expCopp6;
            sendAcl("copp6_add ", false, copp6);
        }
        for (int i = 0; i < lower.expBr.size(); i++) {
            doBrdg(lower.expBr.get(i));
        }
        for (int i = 0; i < lower.expVrf.size(); i++) {
            servP4langVrf vrf = lower.expVrf.get(i);
            doVrf(vrf);
            doRoutes(true, vrf.id, vrf.vrf.fwd4.actualU, vrf.routes4);
            doRoutes(false, vrf.id, vrf.vrf.fwd6.actualU, vrf.routes6);
            vrf.natCfg4 = doNatCfg(true, vrf.id, vrf.vrf.fwd4.natCfg, vrf.natCfg4);
            vrf.natCfg6 = doNatCfg(false, vrf.id, vrf.vrf.fwd6.natCfg, vrf.natCfg6);
            doNatTrns(true, vrf.id, vrf.vrf.fwd4.natTrns, vrf.natTrns4);
            doNatTrns(false, vrf.id, vrf.vrf.fwd6.natTrns, vrf.natTrns6);
        }
        for (int i = 0; i < tabLabel.labels.size(); i++) {
            doLab1(tabLabel.labels.get(i));
        }
        for (int i = labels.size() - 1; i >= 0; i--) {
            doLab2(labels.get(i));
        }
        for (int i = 0; i < lower.expIfc.size(); i++) {
            servP4langIfc ifc = lower.expIfc.get(i);
            doIface(ifc);
            doNeighs(true, ifc, ifc.ifc.ipIf4);
            doNeighs(false, ifc, ifc.ifc.ipIf6);
        }
        for (int i = neighs.size() - 1; i >= 0; i--) {
            doNeighs(neighs.get(i));
        }
        lower.notif.sleep(lower.expDelay);
        return false;
    }

    private servP4langNei genNeighId(servP4langNei ntry) {
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
        return null;
    }

    private servP4langIfc findIfc(cfgIfc ifc) {
        if (ifc == null) {
            return null;
        }
        for (int i = 0; i < lower.expIfc.size(); i++) {
            servP4langIfc ntry = lower.expIfc.get(i);
            if (ntry.ifc == ifc) {
                return ntry;
            }
        }
        return null;
    }

    private servP4langNei findIfc(tabRouteIface ifc, addrIP hop) {
        if (ifc == null) {
            return null;
        }
        servP4langIfc id = null;
        for (int i = 0; i < lower.expIfc.size(); i++) {
            servP4langIfc ntry = lower.expIfc.get(i);
            if (ifc == ntry.ifc.fwdIf4) {
                id = ntry;
                break;
            }
            if (ifc == ntry.ifc.fwdIf6) {
                id = ntry;
                break;
            }
        }
        if (id == null) {
            return null;
        }
        servP4langNei ntry = new servP4langNei();
        ntry.iface = id;
        ntry.adr = hop;
        servP4langNei old = neighs.find(ntry);
        if (old != null) {
            old.need++;
            return old;
        }
        return genNeighId(ntry);
    }

    private int findDyn(ifcBridgeIfc ifc) {
        for (int i = 0; i < lower.expDynIfc.length; i++) {
            if (lower.expDynIfc[i] == ifc) {
                return i;
            }
        }
        return -1;
    }

    private servP4langVrf findVrf(ipFwd fwd) {
        if (fwd == null) {
            return null;
        }
        for (int i = 0; i < lower.expVrf.size(); i++) {
            servP4langVrf ntry = lower.expVrf.get(i);
            if (fwd == ntry.vrf.fwd4) {
                return ntry;
            }
            if (fwd == ntry.vrf.fwd6) {
                return ntry;
            }
        }
        return null;
    }

    private servP4langVrf findVrf(servP4langIfc ifc) {
        if (ifc == null) {
            return null;
        }
        for (int i = 0; i < lower.expVrf.size(); i++) {
            servP4langVrf ntry = lower.expVrf.get(i);
            if (ntry.vrf == ifc.ifc.vrfFor) {
                return ntry;
            }
        }
        return null;
    }

    private servP4langIfc findIfc(ifcBridgeIfc ifc) {
        for (int i = 0; i < lower.expIfc.size(); i++) {
            servP4langIfc ntry = lower.expIfc.get(i);
            if (ifc == ntry.ifc.bridgeIfc) {
                return ntry;
            }
        }
        return null;
    }

    private servP4langIfc findIfc(cfgBrdg ifc) {
        for (int i = 0; i < lower.expIfc.size(); i++) {
            servP4langIfc old = lower.expIfc.get(i);
            if (old.ifc.bridgeIfc != null) {
                continue;
            }
            if (old.ifc.bridgeHed == ifc) {
                return old;
            }
        }
        return null;
    }

    private servP4langIfc findBundl(cfgBndl ifc) {
        for (int i = 0; i < lower.expIfc.size(); i++) {
            servP4langIfc old = lower.expIfc.get(i);
            if (old.ifc.bundleIfc != null) {
                continue;
            }
            if (old.ifc.bundleHed == ifc) {
                return old;
            }
        }
        return null;
    }

    private static int getNullLabel(tabRouteEntry<addrIP> ntry) {
        if (ntry.prefix.network.isIPv4()) {
            return ipMpls.labelExp4;
        } else {
            return ipMpls.labelExp6;
        }
    }

    private static int getLabel(List<Integer> labs) {
        if (labs == null) {
            return -1;
        }
        if (labs.size() < 1) {
            return -1;
        }
        int lab = labs.get(0);
        if (lab == ipMpls.labelImp) {
            return -1;
        } else {
            return lab;
        }
    }

    private static int getLabel(tabRouteEntry<addrIP> ntry) {
        if (ntry.labelRem == null) {
            return getNullLabel(ntry);
        }
        if (ntry.labelRem.size() < 1) {
            return getNullLabel(ntry);
        }
        int i = ntry.labelRem.get(0);
        if (i == ipMpls.labelImp) {
            return getNullLabel(ntry);
        }
        return i;
    }

    private void doLab2(tabLabelNtry ntry) {
        if (tabLabel.labels.find(ntry) != null) {
            return;
        }
        labels.del(ntry);
        if ((ntry.duplicate != null) || (ntry.bier != null)) {
            servP4langVrf vrf = findVrf(ntry.forwarder);
            if (vrf == null) {
                return;
            }
            lower.sendLine("cpulabel_del " + ntry.getValue());
            return;
        }
        if (ntry.nextHop == null) {
            servP4langVrf vrf = findVrf(ntry.forwarder);
            if (vrf == null) {
                return;
            }
            lower.sendLine("mylabel" + ntry.forwarder.ipVersion + "_del" + " " + ntry.getValue() + " " + vrf.id);
            if (lower.expSrv6 == null) {
                return;
            }
            if (lower.expSrv6.addr6 == null) {
                return;
            }
            servP4langVrf vr = findVrf(lower.expSrv6.vrfFor.fwd6);
            if (vr == null) {
                return;
            }
            addrIPv6 adr = lower.expSrv6.addr6.copyBytes();
            bits.msbPutD(adr.getBytes(), 12, ntry.getValue());
            lower.sendLine("mysrv" + ntry.forwarder.ipVersion + "_del " + vr.id + " " + adr + " " + vrf.id);
            return;
        }
        String afi;
        if (ntry.nextHop.isIPv4()) {
            afi = "4";
        } else {
            afi = "6";
        }
        servP4langNei hop = findIfc(ntry.iface, ntry.nextHop);
        if (hop == null) {
            return;
        }
        int lab = getLabel(ntry.remoteLab);
        if (lab < 0) {
            lower.sendLine("unlabel" + afi + "_del " + ntry.getValue() + " " + hop.id + " " + ntry.nextHop);
        } else {
            lower.sendLine("label" + afi + "_del " + ntry.getValue() + " " + hop.id + " " + ntry.nextHop + " " + lab);
        }
    }

    private void doLab1(tabLabelNtry ntry) {
        ntry = ntry.copyBytes();
        if (ntry.pweIfc != null) {
            return;
        }
        if ((ntry.duplicate != null) || (ntry.bier != null)) {
            servP4langVrf vrf = findVrf(ntry.forwarder);
            if (vrf == null) {
                return;
            }
            tabLabelNtry old = labels.find(ntry);
            String act = "add";
            if (old != null) {
                if (!old.differs(ntry)) {
                    return;
                }
                act = "mod";
            }
            labels.put(ntry);
            lower.sendLine("cpulabel_" + act + " " + ntry.getValue());
            return;
        }
        if (ntry.nextHop == null) {
            servP4langVrf vrf = findVrf(ntry.forwarder);
            if (vrf == null) {
                return;
            }
            tabLabelNtry old = labels.find(ntry);
            String act = "add";
            if (old != null) {
                if (!old.differs(ntry)) {
                    return;
                }
                act = "mod";
            }
            labels.put(ntry);
            lower.sendLine("mylabel" + ntry.forwarder.ipVersion + "_" + act + " " + ntry.getValue() + " " + vrf.id);
            if (lower.expSrv6 == null) {
                return;
            }
            if (lower.expSrv6.addr6 == null) {
                return;
            }
            servP4langVrf vr = findVrf(lower.expSrv6.vrfFor.fwd6);
            if (vr == null) {
                return;
            }
            addrIPv6 adr = lower.expSrv6.addr6.copyBytes();
            bits.msbPutD(adr.getBytes(), 12, ntry.getValue());
            lower.sendLine("mysrv" + ntry.forwarder.ipVersion + "_" + act + " " + vr.id + " " + adr + " " + vrf.id);
            return;
        }
        servP4langNei hop = findIfc(ntry.iface, ntry.nextHop);
        if (hop == null) {
            return;
        }
        tabLabelNtry old = labels.find(ntry);
        String act = "add";
        if (old != null) {
            if (!old.differs(ntry)) {
                return;
            }
            act = "mod";
        }
        String afi;
        if (ntry.nextHop.isIPv4()) {
            afi = "4";
        } else {
            afi = "6";
        }
        labels.put(ntry);
        int lab = getLabel(ntry.remoteLab);
        if (lab < 0) {
            lower.sendLine("unlabel" + afi + "_" + act + " " + ntry.getValue() + " " + hop.id + " " + ntry.nextHop);
        } else {
            lower.sendLine("label" + afi + "_" + act + " " + ntry.getValue() + " " + hop.id + " " + ntry.nextHop + " " + lab);
        }
    }

    private void doBrdg(servP4langBr br) {
        br.routed = findIfc(br.br) != null;
        if (br.routed) {
            return;
        }
        for (int i = 0;; i++) {
            ifcBridgeIfc ntry = br.br.bridgeHed.getIface(i);
            if (ntry == null) {
                break;
            }
            if (br.ifcs.find(ntry) != null) {
                continue;
            }
            try {
                clntVxlan ifc = (clntVxlan) ntry.lowerIf;
                if (lower.expDynIfc == null) {
                    continue;
                }
                lower.expDynNxt = (lower.expDynNxt + 1) % lower.expDynSiz;
                lower.expDynIfc[lower.expDynNxt] = ntry;
                br.ifcs.put(ntry);
                lower.sendLine("portbridge_add " + (lower.expDyn1st + lower.expDynNxt) + " " + br.br.num);
                continue;
            } catch (Exception e) {
            }
            int l = -1;
            try {
                clntMplsPwe ifc = (clntMplsPwe) ntry.lowerIf;
                if (ifc.getLabelRem() < 0) {
                    continue;
                }
                l = ifc.getLabelLoc();
            } catch (Exception e) {
            }
            try {
                rtrBgpEvpnPeer ifc = (rtrBgpEvpnPeer) ntry.lowerIf;
                if (ifc.getLabelRem() < 0) {
                    continue;
                }
                l = ifc.getLabelLoc();
                if (br.findIfc(l)) {
                    continue;
                }
            } catch (Exception e) {
            }
            if (l < 1) {
                continue;
            }
            br.ifcs.put(ntry);
            lower.sendLine("bridgelabel_add " + br.br.num + " " + l);
            if (lower.expSrv6 == null) {
                continue;
            }
            if (lower.expSrv6.addr6 == null) {
                continue;
            }
            servP4langVrf vr = findVrf(lower.expSrv6.vrfFor.fwd6);
            if (vr == null) {
                continue;
            }
            addrIPv6 adr = lower.expSrv6.addr6.copyBytes();
            bits.msbPutD(adr.getBytes(), 12, l);
            lower.sendLine("bridgesrv_add " + br.br.num + " " + vr.id + " " + adr);
            continue;
        }
        tabGen<ifcBridgeAdr> seen = new tabGen<ifcBridgeAdr>();
        for (int i = 0;; i++) {
            ifcBridgeAdr ntry = br.br.bridgeHed.getMacAddr(i);
            if (ntry == null) {
                break;
            }
            seen.put(ntry);
            ifcBridgeAdr old = br.macs.find(ntry);
            String a = "add";
            if (old != null) {
                if (old.ifc == ntry.ifc) {
                    continue;
                }
                a = "mod";
            }
            br.macs.put(ntry);
            servP4langIfc ifc = findIfc(ntry.ifc);
            if (ifc != null) {
                if ((ifc.ifc.type != cfgIfc.ifaceType.dialer) && (ifc.ifc.type != cfgIfc.ifaceType.tunnel) && (ifc.ifc.type != cfgIfc.ifaceType.virtppp)) {
                    lower.sendLine("bridgemac_" + a + " " + br.br.num + " " + ntry.adr.toEmuStr() + " " + ifc.id);
                    continue;
                }
                servP4langNei nei = findIfc(ifc.ifc.fwdIf4, new addrIP());
                if (nei == null) {
                    nei = findIfc(ifc.ifc.fwdIf6, new addrIP());
                }
                if (nei == null) {
                    continue;
                }
                String p = "0";
                if ((ifc.ifc.type == cfgIfc.ifaceType.dialer) || (ifc.ifc.type == cfgIfc.ifaceType.virtppp)) {
                    p = "1";
                }
                lower.sendLine("routedmac_" + a + " " + br.br.num + " " + ntry.adr.toEmuStr() + " " + nei.id + " " + p);
                continue;
            }
            int l = -1;
            addrIP adr = null;
            addrIP srv = null;
            tabRouteEntry<addrIP> rou = null;
            try {
                clntVxlan iface = (clntVxlan) ntry.ifc.lowerIf;
                if (lower.expDynIfc == null) {
                    continue;
                }
                int brif = findDyn(ntry.ifc);
                if (brif < 0) {
                    continue;
                }
                adr = iface.getRemote();
                if (adr == null) {
                    continue;
                }
                addrIP src = iface.getLocal();
                if (src == null) {
                    continue;
                }
                rou = iface.vrf.getFwd(adr).actualU.route(adr);
                if (rou == null) {
                    continue;
                }
                if (rou.iface == null) {
                    continue;
                }
                addrIP nh = rou.nextHop;
                if (nh == null) {
                    nh = adr;
                }
                servP4langNei hop = findIfc(rou.iface, nh);
                if (hop == null) {
                    continue;
                }
                lower.sendLine("bridgevxlan" + (adr.isIPv4() ? "4" : "6") + "_" + a + " " + br.br.num + " " + ntry.adr.toEmuStr() + " " + src + " " + adr + " " + hop.id + " " + iface.inst + " " + hop.vrf.id + " " + (brif + lower.expDyn1st));
                continue;
            } catch (Exception e) {
            }
            try {
                clntMplsPwe iface = (clntMplsPwe) ntry.ifc.lowerIf;
                l = iface.getLabelRem();
                adr = iface.getRemote();
                if (adr == null) {
                    continue;
                }
                rou = iface.vrf.getFwd(adr).actualU.route(adr);
            } catch (Exception e) {
            }
            try {
                rtrBgpEvpnPeer iface = (rtrBgpEvpnPeer) ntry.ifc.lowerIf;
                l = iface.getLabelRem();
                adr = iface.getRemote();
                srv = iface.getSrvRem();
                if (srv == null) {
                    rou = iface.getForwarder().actualU.route(adr);
                } else {
                    rou = iface.getForwarder().actualU.route(srv);
                }
            } catch (Exception e) {
            }
            if (l < 1) {
                continue;
            }
            if (rou == null) {
                continue;
            }
            servP4langNei hop = findIfc(rou.iface, rou.nextHop);
            if (hop == null) {
                continue;
            }
            if (srv == null) {
                lower.sendLine("bridgevpls_" + a + " " + br.br.num + " " + ntry.adr.toEmuStr() + " " + adr + " " + hop.id + " " + getLabel(rou) + " " + l);
            } else {
                lower.sendLine("bridgesrv6_" + a + " " + br.br.num + " " + ntry.adr.toEmuStr() + " " + adr + " " + hop.id + " " + srv);
            }
        }
        for (int i = br.macs.size() - 1; i >= 0; i--) {
            ifcBridgeAdr ntry = br.macs.get(i);
            if (ntry == null) {
                continue;
            }
            if (seen.find(ntry) != null) {
                continue;
            }
            br.macs.del(ntry);
            servP4langIfc ifc = findIfc(ntry.ifc);
            if (ifc != null) {
                lower.sendLine("bridgemac_del " + br.br.num + " " + ntry.adr.toEmuStr() + " " + ifc.id);
                continue;
            }
        }
    }

    private void doVrf(servP4langVrf vrf) {
        if (vrf.sentMcast) {
            return;
        }
        vrf.sentMcast = true;
        lower.sendLine("myaddr4_add 224.0.0.0/4 0 " + vrf.id);
        lower.sendLine("myaddr4_add 255.255.255.255/32 0 " + vrf.id);
        lower.sendLine("myaddr6_add ff00::/8 0 " + vrf.id);
    }

    private void doIface(servP4langIfc ifc) {
        if (ifc.ifc.pppoeC != null) {
            servP4langIfc res = findIfc(ifc.ifc.pppoeC.clnIfc);
            if (res != null) {
                res.pppoe = ifc;
            }
        }
        if (ifc.ifc.pppoeR != null) {
            servP4langIfc res = findIfc(ifc.ifc.pppoeR.clnIfc);
            if (res != null) {
                res.pppoe = ifc;
            }
        }
        state.states sta;
        if (ifc.ifc.ethtyp.forcedDN != 0) {
            sta = state.states.admin;
        } else {
            sta = state.states.up;
        }
        int i = ifc.ifc.ethtyp.getMTUsize();
        if ((ifc.master != null) || (ifc.ifc.type == cfgIfc.ifaceType.bundle) || (ifc.ifc.type == cfgIfc.ifaceType.bridge) || (ifc.ifc.type == cfgIfc.ifaceType.dialer) || (ifc.ifc.type == cfgIfc.ifaceType.hairpin) || (ifc.ifc.type == cfgIfc.ifaceType.tunnel) || (ifc.ifc.type == cfgIfc.ifaceType.virtppp)) {
            ifc.sentState = sta;
            ifc.sentMtu = i;
        }
        String a;
        if (ifc.sentState != sta) {
            if (sta == state.states.up) {
                a = "1";
            } else {
                a = "0";
            }
            lower.sendLine("state " + ifc.id + " " + a + " " + ifc.speed);
            ifc.sentState = sta;
        }
        if (ifc.sentMtu != i) {
            lower.sendLine("mtu " + ifc.id + " " + i);
            ifc.sentMtu = i;
        }
        if ((ifc.master != null) && (ifc.sentVlan == 0)) {
            lower.sendLine("portvlan_add " + ifc.id + " " + ifc.master.id + " " + ifc.ifc.vlanNum);
            ifc.sentVlan = ifc.ifc.vlanNum;
        }
        if (ifc.ifc.hairpinHed != null) {
            int o = 0;
            for (i = 0; i < lower.expIfc.size(); i++) {
                servP4langIfc ntry = lower.expIfc.get(i);
                if (ntry == ifc) {
                    continue;
                }
                if (ntry == ifc) {
                    continue;
                }
                if (ntry.ifc.hairpinHed != ifc.ifc.hairpinHed) {
                    continue;
                }
                o = ntry.id;
                break;
            }
            if (o != ifc.sentHairpin) {
                String act;
                if (o < 1) {
                    act = "del";
                } else {
                    if (ifc.sentHairpin > 0) {
                        act = "mod";
                    } else {
                        act = "add";
                    }
                }
                lower.sendLine("hairpin_" + act + " " + ifc.id + " " + o);
                ifc.sentHairpin = o;
            }
        }
        if ((ifc.ifc.bundleHed != null) && (ifc.ifc.bundleIfc == null)) {
            List<servP4langIfc> prt = new ArrayList<servP4langIfc>();
            for (i = 0; i < lower.expIfc.size(); i++) {
                servP4langIfc ntry = lower.expIfc.get(i);
                if (ntry == ifc) {
                    continue;
                }
                if (ntry.ifc.bundleHed != ifc.ifc.bundleHed) {
                    continue;
                }
                if (ntry.ifc.ethtyp.getState() != state.states.up) {
                    continue;
                }
                prt.add(ntry);
            }
            List<servP4langIfc> vln = new ArrayList<servP4langIfc>();
            for (i = 0; i < lower.expIfc.size(); i++) {
                servP4langIfc ntry = lower.expIfc.get(i);
                if (ntry == ifc) {
                    continue;
                }
                if ((ntry.master != ifc)) {
                    continue;
                }
                vln.add(ntry);
            }
            if (prt.size() != ifc.sentBundle) {
                if (ifc.sentBundle < 1) {
                    a = "add";
                } else {
                    a = "mod";
                }
                ifc.sentBundle = prt.size();
                if (prt.size() < 1) {
                    a = "del";
                    prt.add(new servP4langIfc());
                }
                for (i = 0; i < 16; i++) {
                    lower.sendLine("portbundle_" + a + " " + ifc.id + " " + i + " " + prt.get(i % prt.size()).id);
                }
                String s = "";
                for (i = 0; i < prt.size(); i++) {
                    s += " " + prt.get(i).id;
                }
                lower.sendLine("bundlelist_" + a + " " + ifc.id + s);
            }
            if (ifc.sentVlan != vln.size()) {
                for (int o = 0; o < prt.size(); o++) {
                    servP4langIfc ntry = prt.get(o);
                    for (i = 0; i < vln.size(); i++) {
                        servP4langIfc sub = vln.get(i);
                        lower.sendLine("bundlevlan_add " + ntry.id + " " + sub.ifc.vlanNum + " " + sub.id);
                    }
                }
                ifc.sentVlan = vln.size();
            }
        }
        if (ifc.sentVrf == 0) {
            a = "add";
        } else {
            a = "mod";
        }
        if ((ifc.ifc.bridgeHed != null) && (ifc.ifc.bridgeIfc != null)) {
            servP4langBr br = new servP4langBr();
            br.id = ifc.ifc.bridgeHed.num;
            br = lower.expBr.find(br);
            if (br == null) {
                br = new servP4langBr();
            }
            if (!br.routed) {
                if (ifc.sentAcl4in != ifc.ifc.bridgeIfc.filter4in) {
                    sendAcl("inacl4_del " + ifc.id + " ", true, ifc.sentAcl4in);
                    ifc.sentAcl4in = ifc.ifc.bridgeIfc.filter4in;
                    sendAcl("inacl4_add " + ifc.id + " ", true, ifc.sentAcl4in);
                }
                if (ifc.sentAcl4out != ifc.ifc.bridgeIfc.filter4out) {
                    sendAcl("outacl4_del " + ifc.id + " ", true, ifc.sentAcl4out);
                    ifc.sentAcl4out = ifc.ifc.bridgeIfc.filter4out;
                    sendAcl("outacl4_add " + ifc.id + " ", true, ifc.sentAcl4out);
                }
                if (ifc.sentAcl6in != ifc.ifc.bridgeIfc.filter6in) {
                    sendAcl("inacl6_del " + ifc.id + " ", false, ifc.sentAcl6in);
                    ifc.sentAcl6in = ifc.ifc.bridgeIfc.filter6in;
                    sendAcl("inacl6_add " + ifc.id + " ", false, ifc.sentAcl6in);
                }
                if (ifc.sentAcl6out != ifc.ifc.bridgeIfc.filter6out) {
                    sendAcl("outacl6_del " + ifc.id + " ", false, ifc.sentAcl6out);
                    ifc.sentAcl6out = ifc.ifc.bridgeIfc.filter6out;
                    sendAcl("outacl6_add " + ifc.id + " ", false, ifc.sentAcl6out);
                }
                if (ifc.sentVrf == -2) {
                    return;
                }
                lower.sendLine("portbridge_" + a + " " + ifc.id + " " + ifc.ifc.bridgeHed.num);
                ifc.sentVrf = -2;
                return;
            }
        }
        if (ifc.ifc.xconn != null) {
            if (ifc.ifc.xconn.pwom == null) {
                return;
            }
            int lr = ifc.ifc.xconn.pwom.getLabelRem();
            if (lr < 0) {
                return;
            }
            if ((ifc.sentVrf == -1) && (lr == ifc.sentLabel)) {
                return;
            }
            int ll = ifc.ifc.xconn.pwom.getLabelLoc();
            if (ll < 0) {
                return;
            }
            ipFwd ofwd = ifc.ifc.xconn.vrf.getFwd(ifc.ifc.xconn.adr);
            servP4langVrf ovrf = findVrf(ofwd);
            if (ovrf == null) {
                return;
            }
            tabRouteEntry<addrIP> ntry = ofwd.actualU.route(ifc.ifc.xconn.adr);
            if (ntry == null) {
                return;
            }
            if (ntry.iface == null) {
                return;
            }
            servP4langNei hop = findIfc(ntry.iface, ntry.nextHop);
            if (hop == null) {
                return;
            }
            lower.sendLine("xconnect_" + a + " " + ifc.id + " " + ifc.ifc.xconn.adr + " " + hop.id + " " + getLabel(ntry) + " " + ll + " " + lr);
            ifc.sentLabel = lr;
            ifc.sentVrf = -1;
            return;
        }
        servP4langVrf vrf;
        servP4langIfc mstr = ifc;
        if (ifc.ifc.bundleIfc != null) {
            mstr = findBundl(ifc.ifc.bundleHed);
        }
        if (ifc.ifc.bridgeIfc != null) {
            mstr = findIfc(ifc.ifc.bridgeHed);
        }
        vrf = findVrf(mstr);
        if (vrf == null) {
            return;
        }
        if (mstr.ifc.fwdIf4 != null) {
            if (ifc.sentAcl4in != mstr.ifc.fwdIf4.filterIn) {
                sendAcl("inacl4_del " + ifc.id + " ", true, ifc.sentAcl4in);
                ifc.sentAcl4in = mstr.ifc.fwdIf4.filterIn;
                sendAcl("inacl4_add " + ifc.id + " ", true, ifc.sentAcl4in);
            }
            if (ifc.sentAcl4out != mstr.ifc.fwdIf4.filterOut) {
                sendAcl("outacl4_del " + ifc.id + " ", true, ifc.sentAcl4out);
                ifc.sentAcl4out = mstr.ifc.fwdIf4.filterOut;
                sendAcl("outacl4_add " + ifc.id + " ", true, ifc.sentAcl4out);
            }
        }
        if (mstr.ifc.fwdIf6 != null) {
            if (ifc.sentAcl6in != mstr.ifc.fwdIf6.filterIn) {
                sendAcl("inacl6_del " + ifc.id + " ", false, ifc.sentAcl6in);
                ifc.sentAcl6in = mstr.ifc.fwdIf6.filterIn;
                sendAcl("inacl6_add " + ifc.id + " ", false, ifc.sentAcl6in);
            }
            if (ifc.sentAcl6out != mstr.ifc.fwdIf6.filterOut) {
                sendAcl("outacl6_del " + ifc.id + " ", false, ifc.sentAcl6out);
                ifc.sentAcl6out = mstr.ifc.fwdIf6.filterOut;
                sendAcl("outacl6_add " + ifc.id + " ", false, ifc.sentAcl6out);
            }
        }
        if (vrf.id == ifc.sentVrf) {
            return;
        }
        lower.sendLine("portvrf_" + a + " " + ifc.id + " " + vrf.id);
        ifc.sentVrf = vrf.id;
    }

    private void doNeighs(servP4langNei ntry) {
        if (ntry.need > 0) {
            return;
        }
        neighs.del(ntry);
        if (ntry.mac == null) {
            return;
        }
        addrType mac = ntry.iface.ifc.ethtyp.getHwAddr();
        if (mac.getSize() != addrMac.size) {
            return;
        }
        lower.sendLine("neigh" + (ntry.adr.isIPv4() ? "4" : "6") + "_del " + ntry.id + " " + ntry.adr + " " + ntry.mac.toEmuStr() + " " + ntry.vrf.id + " " + ((addrMac) mac).toEmuStr() + " " + ntry.sentIfc);
    }

    private void doNeighs(boolean ipv4, servP4langIfc ifc, ipIfc ipi) {
        if (ipi == null) {
            return;
        }
        servP4langVrf vrf = findVrf(ifc);
        if (vrf == null) {
            return;
        }
        if (ifc.ifc.type == cfgIfc.ifaceType.virtppp) {
            servP4langNei nei = findIfc(ifc.ifc.fwdIf4, new addrIP());
            if (nei == null) {
                nei = findIfc(ifc.ifc.fwdIf6, new addrIP());
            }
            if (nei == null) {
                return;
            }
            nei.need++;
            nei.vrf = vrf;
            if (ifc.ifc.pwhe == null) {
                return;
            }
            if (ifc.ifc.pwhe.l2tp2 == null) {
                return;
            }
            addrIP src = ifc.ifc.pwhe.ifc.getLocAddr(ifc.ifc.pwhe.adr);
            if (src == null) {
                return;
            }
            ipFwd ofwd = ifc.ifc.pwhe.vrf.getFwd(ifc.ifc.pwhe.adr);
            servP4langVrf ovrf = findVrf(ofwd);
            if (ovrf == null) {
                return;
            }
            tabRouteEntry<addrIP> ntry = ofwd.actualU.route(ifc.ifc.pwhe.adr);
            if (ntry == null) {
                return;
            }
            if (ntry.iface == null) {
                return;
            }
            addrIP nh = ntry.nextHop;
            if (nh == null) {
                nh = ifc.ifc.pwhe.adr;
            }
            servP4langNei hop = findIfc(ntry.iface, nh);
            if (hop == null) {
                return;
            }
            if (hop.mac == null) {
                return;
            }
            int tun = ifc.ifc.pwhe.l2tp2.getTunnRem();
            if (tun < 1) {
                return;
            }
            int ses = ifc.ifc.pwhe.l2tp2.getSessRem();
            if (ses < 1) {
                return;
            }
            tun = (tun << 16) | ses;
            int lp = ifc.ifc.pwhe.l2tp2.getPortLoc();
            if (lp < 1) {
                return;
            }
            int rp = ifc.ifc.pwhe.l2tp2.getPortRem();
            if (rp < 1) {
                return;
            }
            String act;
            if (nei.mac == null) {
                act = "add";
            } else {
                act = "mod";
                if ((hop.mac.compare(hop.mac, nei.mac) == 0) && (nei.sentIfc == hop.sentIfc) && (nei.sentTun == lp)) {
                    return;
                }
            }
            nei.mac = hop.mac.copyBytes();
            nei.sentIfc = hop.sentIfc;
            nei.sentTun = lp;
            String afi;
            if (ifc.ifc.pwhe.adr.isIPv4()) {
                afi = "4";
            } else {
                afi = "6";
            }
            lower.sendLine("l2tp" + afi + "_" + act + " " + nei.id + " " + ifc.id + " " + hop.sentIfc + " " + src + " " + ifc.ifc.pwhe.adr + " " + hop.mac.toEmuStr() + " " + ovrf.id + " " + ((addrMac) hop.iface.ifc.ethtyp.getHwAddr()).toEmuStr() + " " + lp + " " + rp + " " + tun);
            return;
        }
        if (ifc.ifc.type == cfgIfc.ifaceType.tunnel) {
            servP4langNei nei = findIfc(ifc.ifc.fwdIf4, new addrIP());
            if (nei == null) {
                nei = findIfc(ifc.ifc.fwdIf6, new addrIP());
            }
            if (nei == null) {
                return;
            }
            nei.need++;
            nei.vrf = vrf;
            if (ifc.ifc.tunVrf == null) {
                return;
            }
            if (ifc.ifc.tunTrg == null) {
                return;
            }
            if (ifc.ifc.tunSrc == null) {
                return;
            }
            addrIP src = ifc.ifc.tunSrc.getLocAddr(ifc.ifc.tunTrg);
            if (src == null) {
                return;
            }
            ipFwd ofwd = ifc.ifc.tunVrf.getFwd(ifc.ifc.tunTrg);
            servP4langVrf ovrf = findVrf(ofwd);
            if (ovrf == null) {
                return;
            }
            tabRouteEntry<addrIP> ntry = ofwd.actualU.route(ifc.ifc.tunTrg);
            if (ntry == null) {
                return;
            }
            if (ntry.iface == null) {
                return;
            }
            addrIP nh = ntry.nextHop;
            if (nh == null) {
                nh = ifc.ifc.tunTrg;
            }
            servP4langNei hop = findIfc(ntry.iface, nh);
            if (hop == null) {
                return;
            }
            if (hop.mac == null) {
                return;
            }
            String act;
            if (nei.mac == null) {
                act = "add";
            } else {
                act = "mod";
                if ((hop.mac.compare(hop.mac, nei.mac) == 0) && (nei.sentIfc == hop.sentIfc)) {
                    return;
                }
            }
            nei.mac = hop.mac.copyBytes();
            nei.sentIfc = hop.sentIfc;
            String afi;
            if (ifc.ifc.tunTrg.isIPv4()) {
                afi = "4";
            } else {
                afi = "6";
            }
            lower.sendLine("gre" + afi + "_" + act + " " + nei.id + " " + ifc.id + " " + hop.sentIfc + " " + src + " " + ifc.ifc.tunTrg + " " + hop.mac.toEmuStr() + " " + ovrf.id + " " + ((addrMac) hop.iface.ifc.ethtyp.getHwAddr()).toEmuStr());
            return;
        }
        if (ifc.ifc.type == cfgIfc.ifaceType.dialer) {
            if (ifc.pppoe == null) {
                return;
            }
            servP4langNei nei = findIfc(ifc.ifc.fwdIf4, new addrIP());
            if (nei == null) {
                nei = findIfc(ifc.ifc.fwdIf6, new addrIP());
            }
            if (nei == null) {
                return;
            }
            nei.need++;
            nei.vrf = vrf;
            int ses = -1;
            addrMac mac = new addrMac();
            if (ifc.pppoe.ifc.pppoeC != null) {
                ses = ifc.pppoe.ifc.pppoeC.getSession(mac);
            }
            if (ifc.pppoe.ifc.pppoeR != null) {
                ses = ifc.pppoe.ifc.pppoeR.getSession(mac);
            }
            if (ses == ifc.sentPppoe) {
                return;
            }
            String act;
            int sess;
            if (ses < 0) {
                act = "del";
                sess = ifc.sentPppoe;
            } else {
                if (ifc.sentPppoe < 0) {
                    act = "add";
                } else {
                    act = "mod";
                }
                sess = ses;
            }
            lower.sendLine("pppoe_" + act + " " + ifc.id + " " + ifc.pppoe.id + " " + nei.id + " " + vrf.id + " " + sess + " " + mac.toEmuStr() + " " + ((addrMac) ifc.pppoe.ifc.ethtyp.getHwAddr()).toEmuStr());
            ifc.sentPppoe = ses;
            return;
        }
        String afi;
        if (ipv4) {
            afi = "4";
        } else {
            afi = "6";
        }
        for (int i = 0;; i++) {
            servP4langNei ntry = new servP4langNei();
            ntry.mac = new addrMac();
            if (ipi.getL2info(i, ntry.adr, ntry.mac)) {
                break;
            }
            if (!ipi.checkConnected(ntry.adr)) {
                continue;
            }
            ntry.iface = ifc;
            servP4langNei old = neighs.find(ntry);
            boolean added = old == null;
            if (added) {
                old = genNeighId(ntry);
                if (old == null) {
                    continue;
                }
            }
            old.need++;
            old.vrf = vrf;
            int outIfc = ifc.id;
            if (ifc.ifc.bridgeHed != null) {
                ifcBridgeAdr bra = ifc.ifc.bridgeHed.bridgeHed.findMacAddr(ntry.mac);
                if (bra == null) {
                    continue;
                }
                servP4langIfc oif = findIfc(bra.ifc);
                if (oif == null) {
                    continue;
                }
                outIfc = oif.id;
            }
            String act;
            if (added || (old.mac == null)) {
                act = "add";
            } else {
                act = "mod";
                if ((ntry.mac.compare(ntry.mac, old.mac) == 0) && (old.sentIfc == outIfc)) {
                    continue;
                }
            }
            old.mac = ntry.mac;
            old.sentIfc = outIfc;
            lower.sendLine("neigh" + afi + "_" + act + " " + old.id + " " + old.adr + " " + old.mac.toEmuStr() + " " + vrf.id + " " + ((addrMac) ifc.ifc.ethtyp.getHwAddr()).toEmuStr() + " " + old.sentIfc);
        }
    }

    private void doNatTrns(boolean ipv4, int vrf, tabGen<tabNatTraN> need, tabGen<tabNatTraN> done) {
        String afi;
        if (ipv4) {
            afi = "4";
        } else {
            afi = "6";
        }
        for (int i = 0; i < need.size(); i++) {
            tabNatTraN ntry = need.get(i);
            if (ntry == null) {
                continue;
            }
            if (done.find(ntry) != null) {
                continue;
            }
            switch (ntry.protocol) {
                case prtUdp.protoNum:
                case prtTcp.protoNum:
                    break;
                default:
                    continue;
            }
            lower.sendLine("nattrns" + afi + "_add " + vrf + " " + ntry.protocol + " " + ntry.origSrcAddr + " " + ntry.origSrcPort + " " + ntry.origTrgAddr + " " + ntry.origTrgPort + " " + ntry.newSrcAddr + " " + ntry.newSrcPort + " " + ntry.newTrgAddr + " " + ntry.newTrgPort);
            done.add(ntry);
        }
        for (int i = done.size() - 1; i >= 0; i--) {
            tabNatTraN ntry = done.get(i);
            if (need.find(ntry) != null) {
                continue;
            }
            switch (ntry.protocol) {
                case prtUdp.protoNum:
                case prtTcp.protoNum:
                    break;
                default:
                    continue;
            }
            lower.sendLine("nattrns" + afi + "_del " + vrf + " " + ntry.protocol + " " + ntry.origSrcAddr + " " + ntry.origSrcPort + " " + ntry.origTrgAddr + " " + ntry.origTrgPort + " " + ntry.newSrcAddr + " " + ntry.newSrcPort + " " + ntry.newTrgAddr + " " + ntry.newTrgPort);
            done.del(ntry);
        }
    }

    private tabListing<tabAceslstN<addrIP>, addrIP> doNatCfg(boolean ipv4, int vrf, tabListing<tabNatCfgN, addrIP> curr, tabListing<tabAceslstN<addrIP>, addrIP> old) {
        tabListing<tabAceslstN<addrIP>, addrIP> need;
        if (curr.size() < 1) {
            need = null;
        } else {
            tabNatCfgN ntry = curr.get(0);
            need = ntry.origSrcList;
        }
        if (old == need) {
            return old;
        }
        String afi;
        if (ipv4) {
            afi = "4";
        } else {
            afi = "6";
        }
        sendAcl("natcfg" + afi + "_del " + vrf + " ", ipv4, old);
        sendAcl("natcfg" + afi + "_add " + vrf + " ", ipv4, need);
        return need;
    }

    private void doRoutes(boolean ipv4, int vrf, tabRoute<addrIP> need, tabRoute<addrIP> done) {
        String afi;
        if (ipv4) {
            afi = "4";
        } else {
            afi = "6";
        }
        for (int i = 0; i < need.size(); i++) {
            tabRouteEntry<addrIP> ntry = need.get(i);
            ntry = ntry.copyBytes();
            if ((ntry.iface == null) && (ntry.rouTab != null)) {
                tabRouteEntry<addrIP> old = done.find(ntry);
                String act = "add";
                if (old != null) {
                    if (!ntry.differs(old)) {
                        continue;
                    }
                    act = "mod";
                }
                if (ntry.segrouPrf == null) {
                    old = ntry.rouTab.actualU.route(ntry.nextHop);
                } else {
                    old = ntry.rouTab.actualU.route(ntry.segrouPrf);
                }
                if (old == null) {
                    continue;
                }
                servP4langNei hop = findIfc(old.iface, old.nextHop);
                if (hop == null) {
                    continue;
                }
                done.add(tabRoute.addType.always, ntry, true, true);
                String a;
                if (ipv4) {
                    a = "" + addrPrefix.ip2ip4(ntry.prefix);
                } else {
                    a = "" + addrPrefix.ip2ip6(ntry.prefix);
                }
                if (ntry.segrouPrf == null) {
                    lower.sendLine("vpnroute" + afi + "_" + act + " " + a + " " + hop.id + " " + ntry.nextHop + " " + vrf + " " + getLabel(old) + " " + getLabel(ntry));
                } else {
                    lower.sendLine("srvroute" + afi + "_" + act + " " + a + " " + hop.id + " " + ntry.nextHop + " " + vrf + " " + ntry.segrouPrf);
                }
                continue;
            }
            tabRouteEntry<addrIP> old = done.find(ntry);
            String act = "add";
            if (old != null) {
                if (ntry.nextHop != null) {
                    findIfc(ntry.iface, ntry.nextHop);
                }
                if (!ntry.differs(old)) {
                    continue;
                }
                act = "mod";
            }
            done.add(tabRoute.addType.always, ntry, true, true);
            String a;
            if (ipv4) {
                a = "" + addrPrefix.ip2ip4(ntry.prefix);
            } else {
                a = "" + addrPrefix.ip2ip6(ntry.prefix);
            }
            if (ntry.nextHop == null) {
                lower.sendLine("myaddr" + afi + "_" + act + " " + a + " -1 " + vrf);
                continue;
            }
            servP4langNei hop = findIfc(ntry.iface, ntry.nextHop);
            if (hop == null) {
                continue;
            }
            if (ntry.labelRem != null) {
                lower.sendLine("labroute" + afi + "_" + act + " " + a + " " + hop.id + " " + ntry.nextHop + " " + vrf + " " + getLabel(ntry));
                continue;
            }
            lower.sendLine("route" + afi + "_" + act + " " + a + " " + hop.id + " " + ntry.nextHop + " " + vrf);
        }
        for (int i = done.size() - 1; i >= 0; i--) {
            tabRouteEntry<addrIP> ntry = done.get(i);
            if (need.find(ntry) != null) {
                continue;
            }
            if ((ntry.iface == null) && (ntry.rouTab != null)) {
                tabRouteEntry<addrIP> old;
                if (ntry.segrouPrf == null) {
                    old = ntry.rouTab.actualU.route(ntry.nextHop);
                } else {
                    old = ntry.rouTab.actualU.route(ntry.segrouPrf);
                }
                if (old == null) {
                    continue;
                }
                servP4langNei hop = findIfc(old.iface, old.nextHop);
                if (hop == null) {
                    continue;
                }
                done.del(ntry);
                String a;
                if (ipv4) {
                    a = "" + addrPrefix.ip2ip4(ntry.prefix);
                } else {
                    a = "" + addrPrefix.ip2ip6(ntry.prefix);
                }
                if (ntry.segrouPrf == null) {
                    lower.sendLine("vpnroute" + afi + "_del " + a + " " + hop.id + " " + ntry.nextHop + " " + vrf + " " + getLabel(old) + " " + getLabel(ntry));
                } else {
                    lower.sendLine("srvroute" + afi + "_del " + a + " " + hop.id + " " + ntry.nextHop + " " + vrf + " " + ntry.segrouPrf);
                }
                continue;
            }
            done.del(ntry);
            String a;
            if (ipv4) {
                a = "" + addrPrefix.ip2ip4(ntry.prefix);
            } else {
                a = "" + addrPrefix.ip2ip6(ntry.prefix);
            }
            if (ntry.nextHop == null) {
                lower.sendLine("myaddr" + afi + "_del " + a + " -1 " + vrf);
                continue;
            }
            servP4langNei hop = findIfc(ntry.iface, ntry.nextHop);
            if (hop == null) {
                continue;
            }
            if (ntry.labelRem != null) {
                lower.sendLine("labroute" + afi + "_del " + a + " " + hop.id + " " + ntry.nextHop + " " + vrf + " " + getLabel(ntry));
                continue;
            }
            lower.sendLine("route" + afi + "_del " + a + " " + hop.id + " " + ntry.nextHop + " " + vrf);
        }
    }

    public String numat2str(tabIntMatcher mat, int max) {
        if (mat.action == tabIntMatcher.actionType.xact) {
            return mat.rangeMin + " " + max;
        } else {
            return "0 0";
        }
    }

    public String ip2str(boolean ipv4, addrIP adr) {
        if (ipv4) {
            return "" + adr.toIPv4();
        } else {
            return "" + adr.toIPv6();
        }
    }

    public String ace2str(int seq, boolean ipv4, tabAceslstN<addrIP> ace) {
        return seq + " " + tabListingEntry.action2string(ace.action) + " " + numat2str(ace.proto, 255) + " " + ip2str(ipv4, ace.srcAddr) + " " + ip2str(ipv4, ace.srcMask) + " " + ip2str(ipv4, ace.trgAddr) + " " + ip2str(ipv4, ace.trgMask) + " " + numat2str(ace.srcPort, 65535) + " " + numat2str(ace.trgPort, 65535);
    }

    public void sendAcl(String pre, boolean ipv4, tabListing<tabAceslstN<addrIP>, addrIP> acl) {
        if (acl == null) {
            return;
        }
        for (int i = 0; i < acl.size(); i++) {
            lower.sendLine(pre + ace2str(acl.size() - i, ipv4, acl.get(i)));
        }
    }

}
