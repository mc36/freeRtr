package serv;

import java.util.Comparator;
import addr.addrEmpty;
import addr.addrType;
import addr.addrIP;
import addr.addrIPv6;
import addr.addrMac;
import addr.addrPrefix;
import cfg.cfgAceslst;
import cfg.cfgAll;
import cfg.cfgBrdg;
import cfg.cfgIfc;
import cfg.cfgVrf;
import clnt.clntMplsPwe;
import ifc.ifcBridgeAdr;
import ifc.ifcBridgeIfc;
import ifc.ifcUp;
import ifc.ifcDn;
import ifc.ifcEther;
import ifc.ifcEthTyp;
import ifc.ifcNull;
import ip.ipFwd;
import ip.ipIfc;
import java.util.List;
import pack.packHolder;
import pipe.pipeLine;
import pipe.pipeSide;
import prt.prtGenConn;
import prt.prtServS;
import rtr.rtrBgpEvpnPeer;
import tab.tabAceslstN;
import tab.tabGen;
import tab.tabIntMatcher;
import tab.tabLabel;
import tab.tabLabelNtry;
import tab.tabListing;
import tab.tabListingEntry;
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
    public tabListing<tabAceslstN<addrIP>, addrIP> expCopp4;

    /**
     * exported copp
     */
    public tabListing<tabAceslstN<addrIP>, addrIP> expCopp6;

    /**
     * last connection
     */
    protected servP4langConn conn;

    /**
     * interconnection interface
     */
    protected ifcEthTyp interconn;

    private ifcDn intercon;

    private counter cntr = new counter();

    /**
     * defaults text
     */
    public final static String defaultL[] = {
        "server p4lang .*! port " + port,
        "server p4lang .*! protocol " + proto2string(protoAllStrm),};

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
        for (int i = 0; i < expIfc.size(); i++) {
            servP4langIfc ntry = expIfc.get(i);
            l.add(beg + "export-port " + ntry.ifc.name + " " + ntry.id);
        }
        for (int i = 0; i < expBr.size(); i++) {
            servP4langBr ntry = expBr.get(i);
            l.add(beg + "export-bridge " + ntry.br.num);
        }
        if (expSrv6 != null) {
            l.add(beg + "export-srv6 " + expSrv6.name);
        }
        if (expCopp4 != null) {
            l.add(beg + "export-copp4 " + expCopp4.listName);
        }
        if (expCopp6 != null) {
            l.add(beg + "export-copp6 " + expCopp6.listName);
        }
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
            ntry.lower = this;
            expVrf.put(ntry);
            return false;
        }
        if (s.equals("export-bridge")) {
            cfgBrdg br = cfgAll.brdgFind(cmd.word(), false);
            if (br == null) {
                cmd.error("no such bridge");
                return false;
            }
            servP4langBr ntry = new servP4langBr();
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
            if (ifc.type != cfgIfc.ifaceType.sdn) {
                cmd.error("not p4lang interface");
                return false;
            }
            servP4langIfc ntry = new servP4langIfc();
            ntry.id = bits.str2num(cmd.word());
            ntry.ifc = ifc;
            ntry.lower = this;
            if (ifc.vlanNum > 0) {
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
                    cmd.error("parent not exported");
                    return false;
                }
            } else {
                ntry.setUpper(ifc.ethtyp);
            }
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
            servP4langVrf ntry = new servP4langVrf();
            ntry.vrf = vrf;
            ntry.id = bits.str2num(cmd.word());
            ntry.lower = this;
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
            ifcNull nul = new ifcNull();
            nul.setUpper(ifc.ethtyp);
            return false;
        }
        return true;
    }

    public void srvHelp(userHelping l) {
        l.add("1 2  export-vrf                specify vrf to export");
        l.add("2 3    <name>                  vrf name");
        l.add("3 .      <num>                 p4lang vrf number");
        l.add("1 2  export-bridge             specify bridge to export");
        l.add("2 .    <num>                   bridge number");
        l.add("1 2  export-port               specify port to export");
        l.add("2 3    <name>                  interface name");
        l.add("3 .      <num>                 p4lang port number");
        l.add("1 2  export-srv6               specify srv6 to export");
        l.add("2 .    <name>                  interface name");
        l.add("1 2  export-copp4              specify copp acl to export");
        l.add("2 .    <name>                  acl name");
        l.add("1 2  export-copp6              specify copp acl to export");
        l.add("2 .    <name>                  acl name");
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
        id.timeout = 120000;
        pipe.lineRx = pipeSide.modTyp.modeLF;
        pipe.lineTx = pipeSide.modTyp.modeLF;
        conn = new servP4langConn(pipe, this);
        for (int i = 0; i < expIfc.size(); i++) {
            servP4langIfc ifc = expIfc.get(i);
            ifc.sentVlan = 0;
            ifc.sentVrf = 0;
        }
        for (int i = 0; i < expVrf.size(); i++) {
            servP4langVrf vrf = expVrf.get(i);
            vrf.routes4.clear();
            vrf.routes6.clear();
            vrf.sentMcast = false;
        }
        for (int i = 0; i < expBr.size(); i++) {
            servP4langBr br = expBr.get(i);
            br.ifcs.clear();
            br.macs.clear();
        }
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
        if (debugger.servP4langTraf) {
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
        ifcEther.createETHheader(pckB, false);
        if (intercon != null) {
            pckB.msbPutW(0, id << 7);
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
     * received packet
     *
     * @param pck packet
     */
    public void recvPack(packHolder pck) {
        cntr.rx(pck);
        ifcEther.createETHheader(pck, false);
        int id = pck.msbGetW(0) >>> 7;
        pck.getSkip(2);
        ifcEther.parseETHheader(pck, false);
        servP4langIfc ntry = new servP4langIfc();
        ntry.id = id;
        ntry = expIfc.find(ntry);
        if (ntry == null) {
            return;
        }
        ntry.upper.recvPack(pck);
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

    public int ifc;

    public addrIP adr = new addrIP();

    public addrMac mac = new addrMac();

    public int compare(servP4langNei o1, servP4langNei o2) {
        if (o1.ifc < o2.ifc) {
            return -1;
        }
        if (o1.ifc > o2.ifc) {
            return +1;
        }
        return o1.adr.compare(o1.adr, o2.adr);
    }

}

class servP4langBr implements Comparator<servP4langBr> {

    public cfgBrdg br;

    public tabGen<ifcBridgeAdr> macs = new tabGen<ifcBridgeAdr>();

    public tabGen<ifcBridgeIfc> ifcs = new tabGen<ifcBridgeIfc>();

    public int compare(servP4langBr o1, servP4langBr o2) {
        return o1.br.compare(o1.br, o2.br);
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

    public servP4lang lower;

    public int id;

    public cfgVrf vrf;

    public boolean sentMcast;

    public tabRoute<addrIP> routes4 = new tabRoute<addrIP>("sent");

    public tabRoute<addrIP> routes6 = new tabRoute<addrIP>("sent");

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

    public int sentVrf;

    public int sentVlan;

    public servP4langIfc master;

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
        return new addrEmpty();
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

    public tabGen<servP4langNei> neighs4 = new tabGen<servP4langNei>();

    public tabGen<servP4langNei> neighs6 = new tabGen<servP4langNei>();

    public tabGen<tabLabelNtry> labels = new tabGen<tabLabelNtry>();

    public servP4langConn(pipeSide pip, servP4lang upper) {
        pipe = pip;
        lower = upper;
        new Thread(this).start();
    }

    public void run() {
        try {
            sendCopp(true, lower.expCopp4);
            sendCopp(false, lower.expCopp6);
            for (;;) {
                if (doRound()) {
                    break;
                }
            }
        } catch (Exception e) {
            logger.traceback(e);
        }
    }

    private boolean doRound() {
        if (pipe.isClosed() != 0) {
            return true;
        }
        keepalive++;
        if (keepalive > 30) {
            String a = "keepalive";
            lower.sendLine(a);
            keepalive = 0;
        }
        if (lower.expVrf == null) {
            return false;
        }
        if (pipe.ready2rx() > 0) {
            String s = pipe.lineGet(0x11);
            if (debugger.servP4langTraf) {
                logger.debug("rx: " + s);
            }
            cmds cmd = new cmds("p4lang", s);
            s = cmd.word();
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
            servP4langIfc ntry = new servP4langIfc();
            ntry.id = bits.str2num(cmd.word());
            ntry = lower.expIfc.find(ntry);
            if (ntry == null) {
                return false;
            }
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
            ifcEther.parseETHheader(pck, false);
            ntry.upper.recvPack(pck);
            return false;
        }
        for (int i = 0; i < lower.expIfc.size(); i++) {
            servP4langIfc ifc = lower.expIfc.get(i);
            doIface(ifc);
            doNeighs(true, ifc, ifc.ifc.ipIf4, neighs4);
            doNeighs(false, ifc, ifc.ifc.ipIf6, neighs6);
        }
        for (int i = 0; i < lower.expBr.size(); i++) {
            servP4langBr br = lower.expBr.get(i);
            doBrdg(br);
        }
        for (int i = 0; i < lower.expVrf.size(); i++) {
            servP4langVrf vrf = lower.expVrf.get(i);
            doVrf(vrf);
            doRoutes(true, vrf.id, vrf.vrf.fwd4.actualU, vrf.routes4);
            doRoutes(false, vrf.id, vrf.vrf.fwd6.actualU, vrf.routes6);
        }
        for (int i = 0; i < tabLabel.labels.size(); i++) {
            tabLabelNtry ntry = tabLabel.labels.get(i);
            if (ntry.pweIfc != null) {
                continue;
            }
            if (ntry.nextHop == null) {
                servP4langVrf vrf = findVrf(ntry.forwarder);
                if (vrf == null) {
                    continue;
                }
                tabLabelNtry old = labels.find(ntry);
                String act = "add";
                if (old != null) {
                    if (!old.differs(ntry)) {
                        continue;
                    }
                    act = "mod";
                }
                labels.put(ntry.copyBytes());
                lower.sendLine("mylabel" + ntry.forwarder.ipVersion + "_" + act + " " + ntry.getValue() + " " + vrf.id);
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
                bits.msbPutD(adr.getBytes(), 12, ntry.getValue());
                lower.sendLine("mysrv" + ntry.forwarder.ipVersion + "_" + act + " " + vr.id + " " + adr + " " + vrf.id);
                continue;
            }
            int p = findIface(ntry.iface);
            if (p < 0) {
                continue;
            }
            tabLabelNtry old = labels.find(ntry);
            String act = "add";
            if (old != null) {
                if (!old.differs(ntry)) {
                    continue;
                }
                act = "mod";
            }
            if (ntry.remoteLab == null) {
                continue;
            }
            labels.put(ntry.copyBytes());
            String afi;
            if (ntry.nextHop.isIPv4()) {
                afi = "4";
            } else {
                afi = "6";
            }
            lower.sendLine("label" + afi + "_" + act + " " + ntry.getValue() + " " + p + " " + ntry.nextHop + " " + ntry.remoteLab.get(0));
        }
        for (int i = 0; i < labels.size(); i++) {
            tabLabelNtry ntry = labels.get(i);
            if (tabLabel.labels.find(ntry) != null) {
                continue;
            }
            labels.del(ntry);
            if (ntry.nextHop == null) {
                servP4langVrf vrf = findVrf(ntry.forwarder);
                if (vrf == null) {
                    continue;
                }
                lower.sendLine("mylabel" + ntry.forwarder.ipVersion + "_del" + " " + ntry.getValue() + " " + vrf.id);
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
                bits.msbPutD(adr.getBytes(), 12, ntry.getValue());
                lower.sendLine("mysrv" + ntry.forwarder.ipVersion + "_del " + vr.id + " " + adr + " " + vrf.id);
                continue;
            }
            String afi;
            if (ntry.nextHop.isIPv4()) {
                afi = "4";
            } else {
                afi = "6";
            }
            lower.sendLine("label" + afi + "_del " + ntry.getValue() + " " + findIface(ntry.iface) + " " + ntry.nextHop + " " + ntry.remoteLab.get(0));
        }
        bits.sleep(1000);
        return false;
    }

    private int findIface(tabRouteIface ifc) {
        if (ifc == null) {
            return -2;
        }
        for (int i = 0; i < lower.expIfc.size(); i++) {
            servP4langIfc ntry = lower.expIfc.get(i);
            if (ifc == ntry.ifc.fwdIf4) {
                return ntry.id;
            }
            if (ifc == ntry.ifc.fwdIf6) {
                return ntry.id;
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

    private int getLabel(tabRouteEntry<addrIP> ntry) {
        if (ntry.labelRem == null) {
            return 0;
        }
        if (ntry.labelRem.size() < 1) {
            return 0;
        }
        return ntry.labelRem.get(0);
    }

    private void doBrdg(servP4langBr br) {
        for (int i = 0;; i++) {
            ifcBridgeIfc ntry = br.br.bridgeHed.getIface(i);
            if (ntry == null) {
                break;
            }
            if (br.ifcs.find(ntry) != null) {
                continue;
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
        for (int i = 0;; i++) {
            ifcBridgeAdr ntry = br.br.bridgeHed.getMacAddr(i);
            if (ntry == null) {
                break;
            }
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
                lower.sendLine("bridgemac_" + a + " " + br.br.num + " " + ntry.adr.toEmuStr() + " " + ifc.id);
                continue;
            }
            int l = -1;
            addrIP adr = null;
            addrIP srv = null;
            tabRouteEntry<addrIP> rou = null;
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
            int p = findIface(rou.iface);
            if (p < 0) {
                continue;
            }
            if (srv == null) {
                lower.sendLine("bridgevpls_" + a + " " + br.br.num + " " + ntry.adr.toEmuStr() + " " + adr + " " + p + " " + getLabel(rou) + " " + l);
            } else {
                lower.sendLine("bridgesrv6_" + a + " " + br.br.num + " " + ntry.adr.toEmuStr() + " " + adr + " " + p + " " + srv);
            }
        }
    }

    private void doVrf(servP4langVrf vrf) {
        if (vrf.sentMcast) {
            return;
        }
        vrf.sentMcast = true;
        lower.sendLine("myaddr4_add 224.0.0.0/4 0 " + vrf.id);
        lower.sendLine("myaddr6_add ff00::/8 0 " + vrf.id);
    }

    private void doIface(servP4langIfc ifc) {
        if ((ifc.master != null) && (ifc.sentVlan == 0)) {
            lower.sendLine("portvlan_add " + ifc.id + " " + ifc.master.id + " " + ifc.ifc.vlanNum);
            ifc.sentVlan = ifc.ifc.vlanNum;
        }
        String a;
        if (ifc.sentVrf == 0) {
            a = "add";
        } else {
            a = "mod";
        }
        if (ifc.ifc.bridgeHed != null) {
            if (ifc.sentVrf == -2) {
                return;
            }
            lower.sendLine("portbridge_" + a + " " + ifc.id + " " + ifc.ifc.bridgeHed.num);
            ifc.sentVrf = -2;
            return;
        }
        if (ifc.ifc.xconn == null) {
            servP4langVrf vrf = findVrf(ifc);
            if (vrf == null) {
                return;
            }
            if (vrf.id == ifc.sentVrf) {
                return;
            }
            lower.sendLine("portvrf_" + a + " " + ifc.id + " " + vrf.id);
            ifc.sentVrf = vrf.id;
            return;
        }
        if (ifc.sentVrf == -1) {
            return;
        }
        if (ifc.ifc.xconn.pwom == null) {
            return;
        }
        int ll = ifc.ifc.xconn.pwom.getLabelLoc();
        if (ll < 0) {
            return;
        }
        int lr = ifc.ifc.xconn.pwom.getLabelRem();
        if (lr < 0) {
            return;
        }
        tabRouteEntry<addrIP> ntry = ifc.ifc.xconn.vrf.getFwd(ifc.ifc.xconn.adr).actualU.route(ifc.ifc.xconn.adr);
        if (ntry == null) {
            return;
        }
        int p = findIface(ntry.iface);
        if (p < 0) {
            return;
        }
        lower.sendLine("xconnect_" + a + " " + ifc.id + " " + ifc.ifc.xconn.adr + " " + p + " " + getLabel(ntry) + " " + ll + " " + lr);
        ifc.sentVrf = -1;
    }

    private void doNeighs(boolean ipv4, servP4langIfc ifc, ipIfc ipi, tabGen<servP4langNei> nei) {
        if (ipi == null) {
            return;
        }
        servP4langVrf vrf = findVrf(ifc);
        if (vrf == null) {
            return;
        }
        String afi;
        if (ipv4) {
            afi = "4";
        } else {
            afi = "6";
        }
        tabGen<servP4langNei> seen = new tabGen<servP4langNei>();
        for (int i = 0;; i++) {
            servP4langNei ntry = new servP4langNei();
            if (ipi.getL2info(i, ntry.adr, ntry.mac)) {
                break;
            }
            ntry.ifc = ifc.id;
            seen.add(ntry);
            servP4langNei old = nei.find(ntry);
            String act = "add";
            if (old != null) {
                if (ntry.mac.compare(ntry.mac, old.mac) == 0) {
                    continue;
                }
                act = "mod";
            }
            nei.add(ntry);
            lower.sendLine("neigh" + afi + "_" + act + " " + ifc.id + " " + ntry.adr + " " + ntry.mac.toEmuStr() + " " + vrf.id);
        }
        for (int i = 0; i < nei.size(); i++) {
            servP4langNei ntry = nei.get(i);
            if (ntry.ifc != ifc.id) {
                continue;
            }
            if (seen.find(ntry) != null) {
                continue;
            }
            nei.del(ntry);
            lower.sendLine("neigh" + afi + "_del " + ifc.id + " " + ntry.adr + " " + ntry.mac.toEmuStr() + " " + vrf.id);
        }
    }

    private void doRoutes(boolean ipv4, int id, tabRoute<addrIP> need, tabRoute<addrIP> done) {
        String afi;
        if (ipv4) {
            afi = "4";
        } else {
            afi = "6";
        }
        for (int i = 0; i < need.size(); i++) {
            tabRouteEntry<addrIP> ntry = need.get(i);
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
                int p = findIface(old.iface);
                if (p < 0) {
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
                    lower.sendLine("vpnroute" + afi + "_" + act + " " + a + " " + p + " " + ntry.nextHop + " " + id + " " + getLabel(old) + " " + getLabel(ntry));
                } else {
                    lower.sendLine("srvroute" + afi + "_" + act + " " + a + " " + p + " " + ntry.nextHop + " " + id + " " + ntry.segrouPrf);
                }
                continue;
            }
            tabRouteEntry<addrIP> old = done.find(ntry);
            String act = "add";
            if (old != null) {
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
                lower.sendLine("myaddr" + afi + "_" + act + " " + a + " " + findIface(ntry.iface) + " " + id);
                continue;
            }
            if (ntry.labelRem != null) {
                lower.sendLine("labroute" + afi + "_" + act + " " + a + " " + findIface(ntry.iface) + " " + ntry.nextHop + " " + id + " " + getLabel(ntry));
                continue;
            }
            lower.sendLine("route" + afi + "_" + act + " " + a + " " + findIface(ntry.iface) + " " + ntry.nextHop + " " + id);
        }
        for (int i = 0; i < done.size(); i++) {
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
                int p = findIface(old.iface);
                if (p < 0) {
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
                    lower.sendLine("vpnroute" + afi + "_del " + a + " " + p + " " + ntry.nextHop + " " + id + " " + getLabel(old) + " " + getLabel(ntry));
                } else {
                    lower.sendLine("srvroute" + afi + "_del " + a + " " + p + " " + ntry.nextHop + " " + id + " " + ntry.segrouPrf);
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
                lower.sendLine("myaddr" + afi + "_del " + a + " " + findIface(ntry.iface) + " " + id);
                continue;
            }
            if (ntry.labelRem != null) {
                lower.sendLine("labroute" + afi + "_del " + a + " " + findIface(ntry.iface) + " " + ntry.nextHop + " " + id + " " + getLabel(ntry));
                continue;
            }
            lower.sendLine("route" + afi + "_del " + a + " " + findIface(ntry.iface) + " " + ntry.nextHop + " " + id);
        }
    }

    public String numat2str(tabIntMatcher mat, int max) {
        if (mat.action == tabIntMatcher.actionType.xact) {
            return mat.rangeMin + " " + max;
        } else {
            return "0 0";
        }
    }

    public void sendCopp(boolean ipv4, tabListing<tabAceslstN<addrIP>, addrIP> acl) {
        if (acl == null) {
            return;
        }
        String afi;
        if (ipv4) {
            afi = "4";
        } else {
            afi = "6";
        }
        for (int i = 0; i < acl.size(); i++) {
            tabAceslstN<addrIP> ace = acl.get(i);
            lower.sendLine("copp" + afi + "_add " + ace.sequence + " " + (ace.action == tabListingEntry.actionType.actPermit ? "permit" : "deny") + " " + numat2str(ace.proto, 255) + " " + ace.srcAddr + " " + ace.srcMask + " " + ace.trgAddr + " " + ace.trgMask + " " + numat2str(ace.srcPort, 65535) + " " + numat2str(ace.trgPort, 65535));
        }
    }

}
