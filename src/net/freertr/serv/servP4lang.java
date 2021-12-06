package net.freertr.serv;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import net.freertr.addr.addrIP;
import net.freertr.addr.addrIPv4;
import net.freertr.addr.addrIPv6;
import net.freertr.addr.addrMac;
import net.freertr.addr.addrPrefix;
import net.freertr.addr.addrType;
import net.freertr.cfg.cfgAceslst;
import net.freertr.cfg.cfgAll;
import net.freertr.cfg.cfgBndl;
import net.freertr.cfg.cfgBrdg;
import net.freertr.cfg.cfgIfc;
import net.freertr.cfg.cfgVrf;
import net.freertr.clnt.clntMplsPwe;
import net.freertr.clnt.clntPckOudp;
import net.freertr.clnt.clntVxlan;
import net.freertr.ifc.ifcBridge;
import net.freertr.ifc.ifcBridgeAdr;
import net.freertr.ifc.ifcBridgeIfc;
import net.freertr.ifc.ifcDn;
import net.freertr.ifc.ifcEthTyp;
import net.freertr.ifc.ifcEther;
import net.freertr.ifc.ifcNull;
import net.freertr.ifc.ifcP2pOEservSess;
import net.freertr.ifc.ifcPolka;
import net.freertr.ifc.ifcUp;
import net.freertr.ip.ipFwd;
import net.freertr.ip.ipFwdBier;
import net.freertr.ip.ipFwdIface;
import net.freertr.ip.ipFwdMcast;
import net.freertr.ip.ipFwdMpNe;
import net.freertr.ip.ipFwdMpmp;
import net.freertr.ip.ipIfc;
import net.freertr.ip.ipMpls;
import net.freertr.pack.packEsp;
import net.freertr.pack.packHolder;
import net.freertr.pipe.pipeLine;
import net.freertr.pipe.pipeSide;
import net.freertr.prt.prtDccp;
import net.freertr.prt.prtGen;
import net.freertr.prt.prtGenConn;
import net.freertr.prt.prtGenServ;
import net.freertr.prt.prtLudp;
import net.freertr.prt.prtSctp;
import net.freertr.prt.prtServS;
import net.freertr.prt.prtTcp;
import net.freertr.prt.prtUdp;
import net.freertr.rtr.rtrBgpEvpnPeer;
import net.freertr.sec.secTransform;
import net.freertr.tab.tabAceslstN;
import net.freertr.tab.tabConnect;
import net.freertr.tab.tabConnectEntry;
import net.freertr.tab.tabGen;
import net.freertr.tab.tabIndex;
import net.freertr.tab.tabIntMatcher;
import net.freertr.tab.tabLabel;
import net.freertr.tab.tabLabelBier;
import net.freertr.tab.tabLabelBierN;
import net.freertr.tab.tabLabelDup;
import net.freertr.tab.tabLabelEntry;
import net.freertr.tab.tabListing;
import net.freertr.tab.tabListingEntry;
import net.freertr.tab.tabNatCfgN;
import net.freertr.tab.tabNatTraN;
import net.freertr.tab.tabNshEntry;
import net.freertr.tab.tabPbrN;
import net.freertr.tab.tabQosN;
import net.freertr.tab.tabRoute;
import net.freertr.tab.tabRouteEntry;
import net.freertr.tab.tabRouteIface;
import net.freertr.user.userFilter;
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
 * p4lang
 *
 * @author matecsaba
 */
public class servP4lang extends servGeneric implements ifcUp, prtServS {

    /**
     * create instance
     */
    public servP4lang() {
    }

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
    public int expDynBr1st = 0;

    /**
     * exported dynamic range size
     */
    public int expDynBrSiz = 0;

    /**
     * exported dynamic interfaces
     */
    public ifcBridgeIfc[] expDynBrIfc;

    /**
     * exported dynamic interfaces
     */
    public String[] expDynBrTun;

    /**
     * exported dynamic next
     */
    public int expDynBrNxt = 0;

    /**
     * exported dynamic interface first
     */
    public int expDynAcc1st = 0;

    /**
     * exported dynamic range size
     */
    public int expDynAccSiz = 0;

    /**
     * exported dynamic interfaces
     */
    public servP4langIfc[] expDynAccIfc;

    /**
     * exported dynamic next
     */
    public int expDynAccNxt = 0;

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

    private ifcDn intercon;

    /**
     * defaults text
     */
    public final static String[] defaultL = {
        "server p4lang .*! port " + port,
        "server p4lang .*! protocol " + proto2string(protoAllStrm),
        "server p4lang .*! no export-srv6",
        "server p4lang .*! no export-copp4",
        "server p4lang .*! no export-copp6",
        "server p4lang .*! no export-dynbr",
        "server p4lang .*! no export-dynacc",
        "server p4lang .*! no interconnect",
        "server p4lang .*! export-interval 1000",};

    /**
     * defaults filter
     */
    public static tabGen<userFilter> defaultF;

    public tabGen<userFilter> srvDefFlt() {
        return defaultF;
    }

    public void srvShRun(String beg, List<String> l, int filter) {
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
            l.add(beg + "export-port " + ntry.ifc.name + " " + ntry.id + " " + ntry.speed + " " + ntry.errCorr + " " + ntry.autoNeg + " " + ntry.flowCtrl);
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
        cmds.cfgLine(l, expDynBrSiz < 1, beg, "export-dynbr", expDynBr1st + " " + expDynBrSiz);
        cmds.cfgLine(l, expDynAccSiz < 1, beg, "export-dynacc", expDynAcc1st + " " + expDynAccSiz);
        l.add(beg + "export-interval " + expDelay);
        cmds.cfgLine(l, interconn == null, beg, "interconnect", "" + interconn);
        for (int i = 0; i < downLinks.size(); i++) {
            servP4langDlnk ntry = downLinks.get(i);
            l.add(beg + "downlink " + ntry.id + " " + ntry.ifc);
        }
    }

    public boolean srvCfgStr(cmds cmd) {
        String s = cmd.word();
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
            servP4langDlnk ntry = new servP4langDlnk(this, bits.str2num(cmd.word()));
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
        if (s.equals("export-dynacc")) {
            expDynAcc1st = bits.str2num(cmd.word());
            expDynAccSiz = bits.str2num(cmd.word());
            expDynAccNxt = 0;
            expDynAccIfc = new servP4langIfc[expDynAccSiz];
            return false;
        }
        if (s.equals("export-dynbr")) {
            expDynBr1st = bits.str2num(cmd.word());
            expDynBrSiz = bits.str2num(cmd.word());
            expDynBrNxt = 0;
            expDynBrIfc = new ifcBridgeIfc[expDynBrSiz];
            expDynBrTun = new String[expDynBrSiz];
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
            servP4langIfc ntry = new servP4langIfc(bits.str2num(cmd.word()));
            ntry.lower = this;
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
            for (int i = 0; i < expIfc.size(); i++) {
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
                need = false;
                for (int i = 0; i < expIfc.size(); i++) {
                    old = expIfc.get(i);
                    if (old.master != null) {
                        continue;
                    }
                    if (old.ifc == ifc.parent) {
                        ntry.master = old;
                        break;
                    }
                }
                if (ntry.master == null) {
                    cmd.error("main interface not exported");
                    return false;
                }
            }
            need |= ntry.speed == -42;
            if (need) {
                ntry.setUpper(ifc.ethtyp);
            }
            if (orig == null) {
                ifc.ethtyp.hwHstry = new history();
                ifc.ethtyp.hwCntr = new counter();
                expIfc.put(ntry);
                return false;
            }
            sendLine("state " + old.id + " 0 " + orig.getStateEnding());
            sendLine("state " + ntry.id + " 1 " + ntry.getStateEnding());
            return false;
        }
        if (!s.equals("no")) {
            return true;
        }
        s = cmd.word();
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
            servP4langDlnk ntry = new servP4langDlnk(this, bits.str2num(cmd.word()));
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
        if (s.equals("export-dynacc")) {
            expDynAcc1st = 0;
            expDynAccSiz = 0;
            expDynAccNxt = 0;
            expDynAccIfc = null;
            return false;
        }
        if (s.equals("export-dynbr")) {
            expDynBr1st = 0;
            expDynBrSiz = 0;
            expDynBrNxt = 0;
            expDynBrIfc = null;
            expDynBrTun = null;
            return false;
        }
        if (s.equals("export-srv6")) {
            expSrv6 = null;
            return false;
        }
        if (s.equals("export-port")) {
            cmd.word();
            servP4langIfc ntry = new servP4langIfc(bits.str2num(cmd.word()));
            ntry = expIfc.del(ntry);
            if (ntry == null) {
                cmd.error("no such export");
                return false;
            }
            sendLine("state " + ntry.id + " 0 " + ntry.getStateEnding());
            if ((ntry.ifc.type == cfgIfc.ifaceType.sdn) && (ntry.ifc.vlanNum == 0)) {
                ifcNull nul = new ifcNull();
                nul.setUpper(ntry.ifc.ethtyp);
            }
            return false;
        }
        return true;
    }

    public void srvHelp(userHelping l) {
        l.add(null, "1 2  export-vrf                specify vrf to export");
        l.add(null, "2 3    <name:vrf>              vrf name");
        l.add(null, "3 .      <num>                 p4lang vrf number");
        l.add(null, "1 2  export-dynbr              specify dynamic bridge port range");
        l.add(null, "2 3    <num>                   first id");
        l.add(null, "3 .      <num>                 number of ids");
        l.add(null, "1 2  export-dynacc             specify dynamic access port range");
        l.add(null, "2 3    <num>                   first id");
        l.add(null, "3 .      <num>                 number of ids");
        l.add(null, "1 2  export-bridge             specify bridge to export");
        l.add(null, "2 .    <num>                   bridge number");
        l.add(null, "1 2  export-port               specify port to export");
        l.add(null, "2 3    <name:ifc>              interface name");
        l.add(null, "3 4,.    <num>                 port number");
        l.add(null, "4 5,.      <num>               speed in gbps");
        l.add(null, "5 6,.        <num>             fec, see hw vendor manual");
        l.add(null, "6 7,.          <num>           autoneg, see hw vendor manual");
        l.add(null, "7 .              <num>         flowctrl, see hw vendor manual");
        l.add(null, "1 2  export-srv6               specify srv6 to export");
        l.add(null, "2 .    <name:ifc>              interface name");
        l.add(null, "1 2  export-copp4              specify copp acl to export");
        l.add(null, "2 .    <name:acl>              acl name");
        l.add(null, "1 2  export-copp6              specify copp acl to export");
        l.add(null, "2 .    <name:acl>              acl name");
        l.add(null, "1 2  export-interval           specify export interval");
        l.add(null, "2 .    <num>                   time in ms");
        l.add(null, "1 2  downlink                  specify downlink for packetin");
        l.add(null, "2 3    <num>                   port number");
        l.add(null, "3 .      <name:ifc>            interface name");
        l.add(null, "1 2  interconnect              specify port to for packetin");
        l.add(null, "2 .    <name:ifc>              interface name");
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
            expIfc.get(i).doClear();
        }
        for (int i = 0; i < expVrf.size(); i++) {
            expVrf.get(i).doClear();
        }
        for (int i = 0; i < expBr.size(); i++) {
            expBr.get(i).doClear();
        }
        if (expDynBrIfc != null) {
            for (int i = 0; i < expDynBrIfc.length; i++) {
                expDynBrIfc[i] = null;
                expDynBrTun[i] = null;
            }
        }
        expDynBrNxt = 0;
        if (expDynAccIfc != null) {
            for (int i = 0; i < expDynAccIfc.length; i++) {
                expDynAccIfc[i] = null;
            }
        }
        expDynAccNxt = 0;
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
        if ((expDynAccIfc != null) && (id >= expDynAcc1st) && (id < (expDynAcc1st + expDynAccSiz))) {
            servP4langIfc ifc = expDynAccIfc[id - expDynAcc1st];
            if (ifc == null) {
                if (debugger.servP4langErr) {
                    logger.debug("got unneeded target: " + id);
                }
                cntr.drop(pck, counter.reasons.noIface);
                return;
            }
            ifc.ifc.ethtyp.gotFromDataplane(pck);
            return;
        }
        if ((expDynBrIfc != null) && (id >= expDynBr1st) && (id < (expDynBr1st + expDynBrSiz))) {
            if (pck.msbGetW(0) == ifcBridge.serialType) {
                pck.getSkip(2);
            } else {
                pck.getSkip(-addrMac.sizeX2);
            }
            ifcBridgeIfc ifc = expDynBrIfc[id - expDynBr1st];
            if (ifc == null) {
                if (debugger.servP4langErr) {
                    logger.debug("got unneeded target: " + id);
                }
                cntr.drop(pck, counter.reasons.noIface);
                return;
            }
            ifc.recvPack(pck);
            return;
        }
        servP4langDlnk dlnk = new servP4langDlnk(this, id);
        dlnk = downLinks.find(dlnk);
        if (dlnk != null) {
            ifcEther.createETHheader(pck, false);
            pck.getSkip(-2);
            ifcEther.parseETHheader(pck, false);
            dlnk.parent.sendPack(pck);
            return;
        }
        servP4langIfc ntry = new servP4langIfc(id);
        ntry = expIfc.find(ntry);
        if (ntry == null) {
            if (debugger.servP4langErr) {
                logger.debug("got unneeded target: " + id);
            }
            cntr.drop(pck, counter.reasons.noIface);
            return;
        }
        ntry.ifc.ethtyp.gotFromDataplane(pck);
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

class servP4langStr<T extends Comparator<T>> implements Comparator<servP4langStr<T>> {

    public final T data;

    public int store;

    public servP4langStr(T dat) {
        data = dat;
    }

    public int compare(servP4langStr<T> o1, servP4langStr<T> o2) {
        return o1.data.compare(o1.data, o2.data);
    }

    public boolean differs(tabGen<servP4langStr<T>> l) {
        servP4langStr<T> o = l.find(this);
        if (o == null) {
            return true;
        }
        return store != o.store;
    }

}

class servP4langDlnk implements Comparator<servP4langDlnk>, ifcUp {

    public final int id;

    public final servP4lang lower;

    public counter cntr = new counter();

    public ifcDn parent;

    public ifcEthTyp ifc;

    public servP4langDlnk(servP4lang prnt, int num) {
        id = num;
        lower = prnt;
    }

    public int compare(servP4langDlnk o1, servP4langDlnk o2) {
        if (o1.id < o2.id) {
            return -1;
        }
        if (o1.id > o2.id) {
            return +1;
        }
        return 0;
    }

    public void recvPack(packHolder pck) {
        cntr.rx(pck);
        ifcEther.createETHheader(pck, false);
        int id = pck.msbGetW(0);
        pck.getSkip(2);
        ifcEther.parseETHheader(pck, false);
        lower.sendPack(id, pck);
    }

    public void setParent(ifcDn prnt) {
        parent = prnt;
    }

    public void setState(state.states stat) {
    }

    public void closeUp() {
    }

    public counter getCounter() {
        return cntr;
    }

}

class servP4langNei implements Comparator<servP4langNei> {

    public final addrIP adr;

    public addrMac mac = null;

    public final servP4langIfc iface;

    public servP4langVrf vrf;

    public String sentIpsec = "";

    public servP4langNei viaH;

    public servP4langIfc viaI;

    public int sentIgNhop = -1;

    public int sentIfc;

    public int sentTun;

    public int id;

    public int need;

    public servP4langNei(servP4langIfc ifc, addrIP per) {
        adr = per;
        iface = ifc;
    }

    public int compare(servP4langNei o1, servP4langNei o2) {
        int i = o1.iface.compare(o1.iface, o2.iface);
        if (i != 0) {
            return i;
        }
        if (o1.iface.cloned != null) {
            return 0;
        }
        if ((o1.iface.ifc.type == cfgIfc.ifaceType.dialer) || (o1.iface.ifc.type == cfgIfc.ifaceType.tunnel) || (o1.iface.ifc.type == cfgIfc.ifaceType.virtppp) || (o1.iface.ifc.type == cfgIfc.ifaceType.template)) {
            return 0;
        }
        return o1.adr.compare(o1.adr, o2.adr);
    }

    public servP4langIfc getVia() {
        servP4langNei via = this;
        for (int i = 0; i < 8; i++) {
            if (via.viaH == null) {
                break;
            }
            via = via.viaH;
        }
        if (via.viaI == null) {
            return via.iface;
        } else {
            return via.viaI;
        }
    }

}

class servP4langBr implements Comparator<servP4langBr> {

    public final int id;

    public cfgBrdg br;

    public boolean routed;

    public tabGen<ifcBridgeAdr> macs = new tabGen<ifcBridgeAdr>();

    public tabGen<ifcBridgeIfc> ifcs = new tabGen<ifcBridgeIfc>();

    public servP4langBr(int i) {
        id = i;
    }

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

    public void doClear() {
        ifcs = new tabGen<ifcBridgeIfc>();
        macs = new tabGen<ifcBridgeAdr>();
    }
}

class servP4langVrf implements Comparator<servP4langVrf> {

    public final int id;

    public cfgVrf vrf;

    public boolean sentMcast;

    public tabGen<servP4langStr<tabRouteEntry<addrIP>>> routed4 = new tabGen<servP4langStr<tabRouteEntry<addrIP>>>();

    public tabGen<servP4langStr<tabRouteEntry<addrIP>>> routed6 = new tabGen<servP4langStr<tabRouteEntry<addrIP>>>();

    public tabRoute<addrIP> routes4 = new tabRoute<addrIP>("sent");

    public tabRoute<addrIP> routes6 = new tabRoute<addrIP>("sent");

    public tabConnect<addrIP, prtGenServ> udp4 = new tabConnect<addrIP, prtGenServ>(new addrIP(), "sent");

    public tabConnect<addrIP, prtGenServ> udp6 = new tabConnect<addrIP, prtGenServ>(new addrIP(), "sent");

    public tabConnect<addrIP, prtGenServ> tcp4 = new tabConnect<addrIP, prtGenServ>(new addrIP(), "sent");

    public tabConnect<addrIP, prtGenServ> tcp6 = new tabConnect<addrIP, prtGenServ>(new addrIP(), "sent");

    public tabGen<ipFwdMcast> mroutes4 = new tabGen<ipFwdMcast>();

    public tabGen<ipFwdMcast> mroutes6 = new tabGen<ipFwdMcast>();

    public tabListing<tabAceslstN<addrIP>, addrIP> natCfg4;

    public tabListing<tabAceslstN<addrIP>, addrIP> natCfg6;

    public tabListing<tabAceslstN<addrIP>, addrIP> natCfg4f;

    public tabListing<tabAceslstN<addrIP>, addrIP> natCfg6f;

    public tabListing<tabPbrN, addrIP> pbrCfg4;

    public tabListing<tabPbrN, addrIP> pbrCfg6;

    public tabListing<tabAceslstN<addrIP>, addrIP> flwSpc4;

    public tabListing<tabAceslstN<addrIP>, addrIP> flwSpc6;

    public tabGen<tabNatTraN> natTrns4 = new tabGen<tabNatTraN>();

    public tabGen<tabNatTraN> natTrns6 = new tabGen<tabNatTraN>();

    public tabGen<servP4langStr<tabIndex<addrIP>>> indexed4 = new tabGen<servP4langStr<tabIndex<addrIP>>>();

    public tabGen<servP4langStr<tabIndex<addrIP>>> indexed6 = new tabGen<servP4langStr<tabIndex<addrIP>>>();

    public tabGen<tabIndex<addrIP>> indexes4 = new tabGen<tabIndex<addrIP>>();

    public tabGen<tabIndex<addrIP>> indexes6 = new tabGen<tabIndex<addrIP>>();

    public servP4langVrf(int i) {
        id = i;
    }

    public int compare(servP4langVrf o1, servP4langVrf o2) {
        if (o1.id < o2.id) {
            return -1;
        }
        if (o1.id > o2.id) {
            return +1;
        }
        return 0;
    }

    public void doClear() {
        routed4 = new tabGen<servP4langStr<tabRouteEntry<addrIP>>>();
        routed6 = new tabGen<servP4langStr<tabRouteEntry<addrIP>>>();
        routes4 = new tabRoute<addrIP>("sent");
        routes6 = new tabRoute<addrIP>("sent");
        mroutes4 = new tabGen<ipFwdMcast>();
        mroutes6 = new tabGen<ipFwdMcast>();
        sentMcast = false;
        natCfg4 = null;
        natCfg4f = new tabListing<tabAceslstN<addrIP>, addrIP>();
        natCfg6 = null;
        natCfg6f = new tabListing<tabAceslstN<addrIP>, addrIP>();
        natTrns4 = new tabGen<tabNatTraN>();
        natTrns6 = new tabGen<tabNatTraN>();
        pbrCfg4 = new tabListing<tabPbrN, addrIP>();
        pbrCfg6 = new tabListing<tabPbrN, addrIP>();
        flwSpc4 = new tabListing<tabAceslstN<addrIP>, addrIP>();
        flwSpc6 = new tabListing<tabAceslstN<addrIP>, addrIP>();
        udp4 = new tabConnect<addrIP, prtGenServ>(new addrIP(), "sent");
        udp6 = new tabConnect<addrIP, prtGenServ>(new addrIP(), "sent");
        tcp4 = new tabConnect<addrIP, prtGenServ>(new addrIP(), "sent");
        tcp6 = new tabConnect<addrIP, prtGenServ>(new addrIP(), "sent");
        indexed4 = new tabGen<servP4langStr<tabIndex<addrIP>>>();
        indexed6 = new tabGen<servP4langStr<tabIndex<addrIP>>>();
        indexes4 = new tabGen<tabIndex<addrIP>>();
        indexes6 = new tabGen<tabIndex<addrIP>>();
    }

}

class servP4langIfc implements ifcDn, Comparator<servP4langIfc> {

    public servP4lang lower;

    public final int id;

    public int speed;

    public int errCorr;

    public int autoNeg;

    public int flowCtrl;

    public int sentVrf;

    public int sentMon;

    public int sentVlan;

    public int sentBundle;

    public int sentHairpin;

    public int sentMtu;

    public int sentPppoe;

    public int sentLabel;

    public int sentPolka;

    public String sentMacsec;

    public state.states sentState = state.states.close;

    public tabListing<tabAceslstN<addrIP>, addrIP> sentAcl4in1;

    public tabListing<tabAceslstN<addrIP>, addrIP> sentAcl4out1;

    public tabListing<tabAceslstN<addrIP>, addrIP> sentAcl6in1;

    public tabListing<tabAceslstN<addrIP>, addrIP> sentAcl6out1;

    public tabListing<tabAceslstN<addrIP>, addrIP> sentAcl4in2;

    public tabListing<tabAceslstN<addrIP>, addrIP> sentAcl4out2;

    public tabListing<tabAceslstN<addrIP>, addrIP> sentAcl6in2;

    public tabListing<tabAceslstN<addrIP>, addrIP> sentAcl6out2;

    public tabListing<tabAceslstN<addrIP>, addrIP> sentAcl4inF;

    public tabListing<tabAceslstN<addrIP>, addrIP> sentAcl4outF;

    public tabListing<tabAceslstN<addrIP>, addrIP> sentAcl6inF;

    public tabListing<tabAceslstN<addrIP>, addrIP> sentAcl6outF;

    public tabListing<tabAceslstN<addrIP>, addrIP> sentQos4in;

    public tabListing<tabAceslstN<addrIP>, addrIP> sentQos4out;

    public tabListing<tabAceslstN<addrIP>, addrIP> sentQos6in;

    public tabListing<tabAceslstN<addrIP>, addrIP> sentQos6out;

    public tabListing<tabAceslstN<addrIP>, addrIP> sentQos4inF;

    public tabListing<tabAceslstN<addrIP>, addrIP> sentQos4outF;

    public tabListing<tabAceslstN<addrIP>, addrIP> sentQos6inF;

    public tabListing<tabAceslstN<addrIP>, addrIP> sentQos6outF;

    public servP4langIfc master;

    public List<servP4langIfc> members;

    public servP4langIfc pppoe;

    public servP4langIfc cloned;

    public cfgIfc ifc;

    public ifcUp upper = new ifcNull();

    public counter cntr = new counter();

    public state.states lastState = state.states.up;

    public servP4langIfc(int i) {
        id = i;
    }

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

    public servP4langIfc getUcast() {
        if (master == null) {
            return this;
        }
        return master;
    }

    public servP4langIfc getMcast(int gid, servP4langNei hop) {
        servP4langIfc ifc;
        if (master == null) {
            ifc = this;
        } else {
            ifc = master;
        }
        if (ifc.members == null) {
            return ifc;
        }
        if (ifc.members.size() < 1) {
            return ifc;
        }
        if (hop != null) {
            gid ^= hop.id;
        }
        return ifc.members.get(gid % ifc.members.size());
    }

    public addrMac getMac() {
        addrType adr = ifc.ethtyp.getHwAddr();
        if (adr.getSize() != addrMac.size) {
            return new addrMac();
        }
        return (addrMac) adr;
    }

    public String getStateEnding() {
        return speed + " " + errCorr + " " + autoNeg + " " + flowCtrl;
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

    public void doClear() {
        lastState = state.states.up;
        upper.setState(lastState);
        sentVlan = 0;
        sentBundle = 0;
        sentHairpin = 0;
        sentPppoe = -1;
        sentMacsec = null;
        sentVrf = 0;
        sentMon = -1;
        sentState = state.states.close;
        sentMtu = 0;
        sentLabel = -1;
        sentPolka = -1;
        sentAcl4in1 = null;
        sentAcl4in2 = null;
        sentAcl4inF = new tabListing<tabAceslstN<addrIP>, addrIP>();
        sentAcl4out1 = null;
        sentAcl4out2 = null;
        sentAcl4outF = new tabListing<tabAceslstN<addrIP>, addrIP>();
        sentAcl6in1 = null;
        sentAcl6in2 = null;
        sentAcl6inF = new tabListing<tabAceslstN<addrIP>, addrIP>();
        sentAcl6out1 = null;
        sentAcl6out2 = null;
        sentAcl6outF = new tabListing<tabAceslstN<addrIP>, addrIP>();
        sentQos4in = null;
        sentQos4inF = new tabListing<tabAceslstN<addrIP>, addrIP>();
        sentQos4out = null;
        sentQos4outF = new tabListing<tabAceslstN<addrIP>, addrIP>();
        sentQos6in = null;
        sentQos6inF = new tabListing<tabAceslstN<addrIP>, addrIP>();
        sentQos6out = null;
        sentQos6outF = new tabListing<tabAceslstN<addrIP>, addrIP>();
    }

}

class servP4langConn implements Runnable {

    public pipeSide pipe;

    public servP4lang lower;

    public int keepalive;

    public tabGen<servP4langNei> neighs = new tabGen<servP4langNei>();

    public tabGen<tabLabelEntry> labels = new tabGen<tabLabelEntry>();

    public tabGen<tabNshEntry> nshs = new tabGen<tabNshEntry>();

    public tabListing<tabAceslstN<addrIP>, addrIP> copp4;

    public tabListing<tabAceslstN<addrIP>, addrIP> copp6;

    public tabListing<tabAceslstN<addrIP>, addrIP> copp4f = new tabListing<tabAceslstN<addrIP>, addrIP>();

    public tabListing<tabAceslstN<addrIP>, addrIP> copp6f = new tabListing<tabAceslstN<addrIP>, addrIP>();

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
            if (debugger.servP4langErr) {
                logger.debug("got unneeded report: " + cmd.getOriginal());
            }
            return;
        }
        counter old = ntry.hwCntr;
        ntry.hwCntr = new counter();
        ntry.hwCntr.packRx = bits.str2long(cmd.word());
        ntry.hwCntr.byteRx = bits.str2long(cmd.word());
        if (old == null) {
            old = new counter();
        }
        if (old.compare(old, ntry.hwCntr) == 0) {
            return;
        }
        ntry.lastUsed = bits.getTime();
        ntry.reverse.lastUsed = ntry.lastUsed;
    }

    private void updateMroute(cmds cmd, ipFwd fwd) {
        addrIP src = new addrIP();
        src.fromString(cmd.word());
        addrIP grp = new addrIP();
        grp.fromString(cmd.word());
        ipFwdMcast ntry = new ipFwdMcast(grp, src);
        ntry = fwd.groups.find(ntry);
        if (ntry == null) {
            if (debugger.servP4langErr) {
                logger.debug("got unneeded report: " + cmd.getOriginal());
            }
            return;
        }
        ntry.hwCntr = new counter();
        ntry.hwCntr.packTx = bits.str2long(cmd.word());
        ntry.hwCntr.byteTx = bits.str2long(cmd.word());
    }

    private void updateRoute(cmds cmd, ipFwd fwd, addrPrefix<addrIP> prf) {
        tabRouteEntry<addrIP> ntry = fwd.actualU.find(prf);
        if (ntry == null) {
            if (debugger.servP4langErr) {
                logger.debug("got unneeded report: " + cmd.getOriginal());
            }
            return;
        }
        ntry.hwCntr = new counter();
        ntry.hwCntr.packTx = bits.str2long(cmd.word());
        ntry.hwCntr.byteTx = bits.str2long(cmd.word());
    }

    private void updatePbr(cmds cmd, tabListing<tabPbrN, addrIP> pbr) {
        int seq = bits.str2num(cmd.word());
        tabPbrN rul = null;
        for (int i = 0; i < pbr.size(); i++) {
            rul = pbr.get(i);
            if (seq < rul.matcher.size()) {
                break;
            }
            seq -= rul.matcher.size();
        }
        if (rul == null) {
            if (debugger.servP4langErr) {
                logger.debug("got unneeded report: " + cmd.getOriginal());
            }
            return;
        }
        tabAceslstN<addrIP> ntry = rul.matcher.get(seq);
        if (ntry == null) {
            if (debugger.servP4langErr) {
                logger.debug("got unneeded report: " + cmd.getOriginal());
            }
            return;
        }
        if (ntry.rolledFrom != null) {
            ntry = ntry.rolledFrom;
        }
        counter old = ntry.hwCntr;
        ntry.hwCntr = new counter();
        ntry.hwCntr.packRx = bits.str2long(cmd.word());
        ntry.hwCntr.byteRx = bits.str2long(cmd.word());
        if (old == null) {
            old = new counter();
        }
        if (old.compare(old, ntry.hwCntr) == 0) {
            return;
        }
        ntry.lastMatch = bits.getTime();
    }

    private void updateAcl(cmds cmd, tabListing<tabAceslstN<addrIP>, addrIP> acl) {
        tabAceslstN<addrIP> ntry = acl.get(bits.str2num(cmd.word()));
        if (ntry == null) {
            if (debugger.servP4langErr) {
                logger.debug("got unneeded report: " + cmd.getOriginal());
            }
            return;
        }
        if (ntry.rolledFrom != null) {
            ntry = ntry.rolledFrom;
        }
        counter old = ntry.hwCntr;
        ntry.hwCntr = new counter();
        ntry.hwCntr.packRx = bits.str2long(cmd.word());
        ntry.hwCntr.byteRx = bits.str2long(cmd.word());
        if (old == null) {
            old = new counter();
        }
        if (old.compare(old, ntry.hwCntr) == 0) {
            return;
        }
        ntry.lastMatch = bits.getTime();
    }

    private void updateTunn(cmds cmd, ipFwd fwd, prtGen prt) {
        addrIP sa = new addrIP();
        sa.fromString(cmd.word());
        addrIP da = new addrIP();
        da.fromString(cmd.word());
        int sp = bits.str2num(cmd.word());
        int dp = bits.str2num(cmd.word());
        tabRouteEntry<addrIP> ntry = fwd.actualU.route(da);
        if (ntry == null) {
            if (debugger.servP4langErr) {
                logger.debug("got unneeded report: " + cmd.getOriginal());
            }
            return;
        }
        if (ntry.best.iface == null) {
            if (debugger.servP4langErr) {
                logger.debug("got unneeded report: " + cmd.getOriginal());
            }
            return;
        }
        counter cntr = new counter();
        cntr.packTx = bits.str2long(cmd.word());
        cntr.byteTx = bits.str2long(cmd.word());
        prt.counterUpdate((ipFwdIface) ntry.best.iface, sa, sp, dp, cntr);
    }

    private boolean doRound() {
        if (pipe.isClosed() != 0) {
            return true;
        }
        if (pipe.ready2rx() > 0) {
            String s = pipe.lineGet(0x11).trim();
            if (s.length() < 1) {
                return false;
            }
            if (debugger.servP4langRx) {
                logger.debug("rx: " + s);
            }
            cmds cmd = new cmds("p4lang", s);
            s = cmd.word();
            if (s.equals("dataplane-say")) {
                logger.info("dataplane said: " + cmd.getRemaining());
                return false;
            }
            if (s.equals("state")) {
                servP4langIfc ntry = findIfc(bits.str2num(cmd.word()));
                if (ntry == null) {
                    if (debugger.servP4langErr) {
                        logger.debug("got unneeded report: " + cmd.getOriginal());
                    }
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
            if (s.equals("counter")) {
                servP4langIfc ntry = findIfc(bits.str2num(cmd.word()));
                if (ntry == null) {
                    if (debugger.servP4langErr) {
                        logger.debug("got unneeded report: " + cmd.getOriginal());
                    }
                    return false;
                }
                ntry.ifc.ethtyp.hwCntr.packRx = bits.str2long(cmd.word());
                ntry.ifc.ethtyp.hwCntr.byteRx = bits.str2long(cmd.word());
                ntry.ifc.ethtyp.hwCntr.packTx = bits.str2long(cmd.word());
                ntry.ifc.ethtyp.hwCntr.byteTx = bits.str2long(cmd.word());
                ntry.ifc.ethtyp.hwCntr.packDr = bits.str2long(cmd.word());
                ntry.ifc.ethtyp.hwCntr.byteDr = bits.str2long(cmd.word());
                ntry.ifc.ethtyp.hwHstry.update(ntry.ifc.ethtyp.hwCntr);
                if (ntry.ifc.ethtyp.hwSub == null) {
                    return false;
                }
                ntry.ifc.ethtyp.hwCntr = ntry.ifc.ethtyp.hwCntr.minus(ntry.ifc.ethtyp.hwSub);
                return false;
            }
            if (s.equals("nattrns4_cnt")) {
                servP4langVrf vrf = new servP4langVrf(bits.str2num(cmd.word()));
                vrf = lower.expVrf.find(vrf);
                if (vrf == null) {
                    if (debugger.servP4langErr) {
                        logger.debug("got unneeded report: " + cmd.getOriginal());
                    }
                    return false;
                }
                updateTrans(cmd, vrf.vrf.fwd4);
                return false;
            }
            if (s.equals("nattrns6_cnt")) {
                servP4langVrf vrf = new servP4langVrf(bits.str2num(cmd.word()));
                vrf = lower.expVrf.find(vrf);
                if (vrf == null) {
                    if (debugger.servP4langErr) {
                        logger.debug("got unneeded report: " + cmd.getOriginal());
                    }
                    return false;
                }
                updateTrans(cmd, vrf.vrf.fwd6);
                return false;
            }
            if (s.equals("macsec_cnt")) {
                servP4langIfc ntry = findIfc(bits.str2num(cmd.word()));
                if (ntry == null) {
                    if (debugger.servP4langErr) {
                        logger.debug("got unneeded report: " + cmd.getOriginal());
                    }
                    return false;
                }
                if (ntry.ifc.ethtyp.macSec == null) {
                    if (debugger.servP4langErr) {
                        logger.debug("got unneeded report: " + cmd.getOriginal());
                    }
                    return false;
                }
                ntry.ifc.ethtyp.macSec.hwCntr = new counter();
                ntry.ifc.ethtyp.macSec.hwCntr.packRx = bits.str2long(cmd.word());
                ntry.ifc.ethtyp.macSec.hwCntr.byteRx = bits.str2long(cmd.word());
                ntry.ifc.ethtyp.macSec.hwCntr.packTx = bits.str2long(cmd.word());
                ntry.ifc.ethtyp.macSec.hwCntr.byteTx = bits.str2long(cmd.word());
                return false;
            }
            if (s.equals("bridge_cnt")) {
                servP4langBr br = new servP4langBr(bits.str2num(cmd.word()));
                br = lower.expBr.find(br);
                if (br == null) {
                    if (debugger.servP4langErr) {
                        logger.debug("got unneeded report: " + cmd.getOriginal());
                    }
                    return false;
                }
                addrMac mac = new addrMac();
                mac.fromString(cmd.word());
                ifcBridgeAdr ntry = br.br.bridgeHed.findMacAddr(mac);
                if (ntry == null) {
                    if (debugger.servP4langErr) {
                        logger.debug("got unneeded report: " + cmd.getOriginal());
                    }
                    return false;
                }
                counter old = ntry.hwCntr;
                ntry.hwCntr = new counter();
                ntry.hwCntr.packRx = bits.str2long(cmd.word());
                ntry.hwCntr.byteRx = bits.str2long(cmd.word());
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
            if (s.equals("inacl4_cnt")) {
                servP4langIfc ntry = findIfc(bits.str2num(cmd.word()));
                if (ntry == null) {
                    if (debugger.servP4langErr) {
                        logger.debug("got unneeded report: " + cmd.getOriginal());
                    }
                    return false;
                }
                updateAcl(cmd, ntry.sentAcl4inF);
                return false;
            }
            if (s.equals("inacl6_cnt")) {
                servP4langIfc ntry = findIfc(bits.str2num(cmd.word()));
                if (ntry == null) {
                    if (debugger.servP4langErr) {
                        logger.debug("got unneeded report: " + cmd.getOriginal());
                    }
                    return false;
                }
                updateAcl(cmd, ntry.sentAcl6inF);
                return false;
            }
            if (s.equals("outacl4_cnt")) {
                servP4langIfc ntry = findIfc(bits.str2num(cmd.word()));
                if (ntry == null) {
                    if (debugger.servP4langErr) {
                        logger.debug("got unneeded report: " + cmd.getOriginal());
                    }
                    return false;
                }
                updateAcl(cmd, ntry.sentAcl4outF);
                return false;
            }
            if (s.equals("outacl6_cnt")) {
                servP4langIfc ntry = findIfc(bits.str2num(cmd.word()));
                if (ntry == null) {
                    if (debugger.servP4langErr) {
                        logger.debug("got unneeded report: " + cmd.getOriginal());
                    }
                    return false;
                }
                updateAcl(cmd, ntry.sentAcl6outF);
                return false;
            }
            if (s.equals("natacl4_cnt")) {
                servP4langVrf vrf = new servP4langVrf(bits.str2num(cmd.word()));
                vrf = lower.expVrf.find(vrf);
                if (vrf == null) {
                    if (debugger.servP4langErr) {
                        logger.debug("got unneeded report: " + cmd.getOriginal());
                    }
                    return false;
                }
                updateAcl(cmd, vrf.natCfg4f);
                return false;
            }
            if (s.equals("natacl6_cnt")) {
                servP4langVrf vrf = new servP4langVrf(bits.str2num(cmd.word()));
                vrf = lower.expVrf.find(vrf);
                if (vrf == null) {
                    if (debugger.servP4langErr) {
                        logger.debug("got unneeded report: " + cmd.getOriginal());
                    }
                    return false;
                }
                updateAcl(cmd, vrf.natCfg6f);
                return false;
            }
            if (s.equals("pbracl4_cnt")) {
                servP4langVrf vrf = new servP4langVrf(bits.str2num(cmd.word()));
                vrf = lower.expVrf.find(vrf);
                if (vrf == null) {
                    if (debugger.servP4langErr) {
                        logger.debug("got unneeded report: " + cmd.getOriginal());
                    }
                    return false;
                }
                updatePbr(cmd, vrf.pbrCfg4);
                return false;
            }
            if (s.equals("pbracl6_cnt")) {
                servP4langVrf vrf = new servP4langVrf(bits.str2num(cmd.word()));
                vrf = lower.expVrf.find(vrf);
                if (vrf == null) {
                    if (debugger.servP4langErr) {
                        logger.debug("got unneeded report: " + cmd.getOriginal());
                    }
                    return false;
                }
                updatePbr(cmd, vrf.pbrCfg6);
                return false;
            }
            if (s.equals("coppacl4_cnt")) {
                updateAcl(cmd, copp4f);
                return false;
            }
            if (s.equals("coppacl6_cnt")) {
                updateAcl(cmd, copp6f);
                return false;
            }
            if (s.equals("inqos4_cnt")) {
                servP4langIfc ntry = findIfc(bits.str2num(cmd.word()));
                if (ntry == null) {
                    if (debugger.servP4langErr) {
                        logger.debug("got unneeded report: " + cmd.getOriginal());
                    }
                    return false;
                }
                updateAcl(cmd, ntry.sentQos4inF);
                return false;
            }
            if (s.equals("inqos6_cnt")) {
                servP4langIfc ntry = findIfc(bits.str2num(cmd.word()));
                if (ntry == null) {
                    if (debugger.servP4langErr) {
                        logger.debug("got unneeded report: " + cmd.getOriginal());
                    }
                    return false;
                }
                updateAcl(cmd, ntry.sentQos6inF);
                return false;
            }
            if (s.equals("outqos4_cnt")) {
                servP4langIfc ntry = findIfc(bits.str2num(cmd.word()));
                if (ntry == null) {
                    if (debugger.servP4langErr) {
                        logger.debug("got unneeded report: " + cmd.getOriginal());
                    }
                    return false;
                }
                updateAcl(cmd, ntry.sentQos4outF);
                return false;
            }
            if (s.equals("outqos6_cnt")) {
                servP4langIfc ntry = findIfc(bits.str2num(cmd.word()));
                if (ntry == null) {
                    if (debugger.servP4langErr) {
                        logger.debug("got unneeded report: " + cmd.getOriginal());
                    }
                    return false;
                }
                updateAcl(cmd, ntry.sentQos6outF);
                return false;
            }
            if (s.equals("flowspec4_cnt")) {
                servP4langVrf vrf = new servP4langVrf(bits.str2num(cmd.word()));
                vrf = lower.expVrf.find(vrf);
                if (vrf == null) {
                    if (debugger.servP4langErr) {
                        logger.debug("got unneeded report: " + cmd.getOriginal());
                    }
                    return false;
                }
                updateAcl(cmd, vrf.flwSpc4);
                return false;
            }
            if (s.equals("flowspec6_cnt")) {
                servP4langVrf vrf = new servP4langVrf(bits.str2num(cmd.word()));
                vrf = lower.expVrf.find(vrf);
                if (vrf == null) {
                    if (debugger.servP4langErr) {
                        logger.debug("got unneeded report: " + cmd.getOriginal());
                    }
                    return false;
                }
                updateAcl(cmd, vrf.flwSpc6);
                return false;
            }
            if (s.equals("mroute4_cnt")) {
                servP4langVrf vrf = new servP4langVrf(bits.str2num(cmd.word()));
                vrf = lower.expVrf.find(vrf);
                if (vrf == null) {
                    if (debugger.servP4langErr) {
                        logger.debug("got unneeded report: " + cmd.getOriginal());
                    }
                    return false;
                }
                updateMroute(cmd, vrf.vrf.fwd4);
                return false;
            }
            if (s.equals("mroute6_cnt")) {
                servP4langVrf vrf = new servP4langVrf(bits.str2num(cmd.word()));
                vrf = lower.expVrf.find(vrf);
                if (vrf == null) {
                    if (debugger.servP4langErr) {
                        logger.debug("got unneeded report: " + cmd.getOriginal());
                    }
                    return false;
                }
                updateMroute(cmd, vrf.vrf.fwd6);
                return false;
            }
            if (s.equals("route4_cnt")) {
                servP4langVrf vrf = new servP4langVrf(bits.str2num(cmd.word()));
                vrf = lower.expVrf.find(vrf);
                if (vrf == null) {
                    if (debugger.servP4langErr) {
                        logger.debug("got unneeded report: " + cmd.getOriginal());
                    }
                    return false;
                }
                addrIPv4 adr = new addrIPv4();
                adr.fromString(cmd.word());
                addrPrefix<addrIPv4> prf = new addrPrefix<addrIPv4>(adr, bits.str2num(cmd.word()));
                updateRoute(cmd, vrf.vrf.fwd4, addrPrefix.ip4toIP(prf));
                return false;
            }
            if (s.equals("route6_cnt")) {
                servP4langVrf vrf = new servP4langVrf(bits.str2num(cmd.word()));
                vrf = lower.expVrf.find(vrf);
                if (vrf == null) {
                    if (debugger.servP4langErr) {
                        logger.debug("got unneeded report: " + cmd.getOriginal());
                    }
                    return false;
                }
                addrIPv6 adr = new addrIPv6();
                adr.fromString(cmd.word());
                addrPrefix<addrIPv6> prf = new addrPrefix<addrIPv6>(adr, bits.str2num(cmd.word()));
                updateRoute(cmd, vrf.vrf.fwd6, addrPrefix.ip6toIP(prf));
                return false;
            }
            if (s.equals("mpls_cnt")) {
                tabLabelEntry ntry = new tabLabelEntry(bits.str2num(cmd.word()));
                ntry = tabLabel.labels.find(ntry);
                if (ntry == null) {
                    if (debugger.servP4langErr) {
                        logger.debug("got unneeded report: " + cmd.getOriginal());
                    }
                    return false;
                }
                ntry.hwCntr = new counter();
                ntry.hwCntr.packRx = bits.str2long(cmd.word());
                ntry.hwCntr.byteRx = bits.str2long(cmd.word());
                return false;
            }
            if (s.equals("polka_cnt")) {
                servP4langVrf vrf = new servP4langVrf(bits.str2num(cmd.word()));
                vrf = lower.expVrf.find(vrf);
                if (vrf == null) {
                    if (debugger.servP4langErr) {
                        logger.debug("got unneeded report: " + cmd.getOriginal());
                    }
                    return false;
                }
                tabIndex<addrIP> ntry = new tabIndex<addrIP>(bits.str2num(cmd.word()), null);
                tabIndex<addrIP> res = vrf.vrf.fwd4.actualI.find(ntry);
                if (res == null) {
                    res = vrf.vrf.fwd6.actualI.find(ntry);
                }
                if (res == null) {
                    if (debugger.servP4langErr) {
                        logger.debug("got unneeded report: " + cmd.getOriginal());
                    }
                    return false;
                }
                res.hwCntr = new counter();
                res.hwCntr.packRx = bits.str2long(cmd.word());
                res.hwCntr.byteRx = bits.str2long(cmd.word());
                return false;
            }
            if (s.equals("nsh_cnt")) {
                int i = bits.str2num(cmd.word());
                tabNshEntry ntry = new tabNshEntry(i, bits.str2num(cmd.word()));
                ntry = tabNshEntry.services.find(ntry);
                if (ntry == null) {
                    if (debugger.servP4langErr) {
                        logger.debug("got unneeded report: " + cmd.getOriginal());
                    }
                    return false;
                }
                ntry.hwCntr = new counter();
                ntry.hwCntr.packRx = bits.str2long(cmd.word());
                ntry.hwCntr.byteRx = bits.str2long(cmd.word());
                return false;
            }
            if (s.equals("tun4_cnt")) {
                servP4langVrf vrf = new servP4langVrf(bits.str2num(cmd.word()));
                vrf = lower.expVrf.find(vrf);
                if (vrf == null) {
                    if (debugger.servP4langErr) {
                        logger.debug("got unneeded report: " + cmd.getOriginal());
                    }
                    return false;
                }
                switch (bits.str2num(cmd.word())) {
                    case prtUdp.protoNum:
                        updateTunn(cmd, vrf.vrf.fwd4, vrf.vrf.udp4);
                        return false;
                    case prtTcp.protoNum:
                        updateTunn(cmd, vrf.vrf.fwd4, vrf.vrf.tcp4);
                        return false;
                    case prtLudp.protoNum:
                        updateTunn(cmd, vrf.vrf.fwd4, vrf.vrf.ludp4);
                        return false;
                    case prtDccp.protoNum:
                        updateTunn(cmd, vrf.vrf.fwd4, vrf.vrf.dccp4);
                        return false;
                    case prtSctp.protoNum:
                        updateTunn(cmd, vrf.vrf.fwd4, vrf.vrf.sctp4);
                        return false;
                }
                return false;
            }
            if (s.equals("tun6_cnt")) {
                servP4langVrf vrf = new servP4langVrf(bits.str2num(cmd.word()));
                vrf = lower.expVrf.find(vrf);
                if (vrf == null) {
                    if (debugger.servP4langErr) {
                        logger.debug("got unneeded report: " + cmd.getOriginal());
                    }
                    return false;
                }
                switch (bits.str2num(cmd.word())) {
                    case prtUdp.protoNum:
                        updateTunn(cmd, vrf.vrf.fwd6, vrf.vrf.udp6);
                        return false;
                    case prtTcp.protoNum:
                        updateTunn(cmd, vrf.vrf.fwd6, vrf.vrf.tcp6);
                        return false;
                    case prtLudp.protoNum:
                        updateTunn(cmd, vrf.vrf.fwd6, vrf.vrf.ludp6);
                        return false;
                    case prtDccp.protoNum:
                        updateTunn(cmd, vrf.vrf.fwd6, vrf.vrf.dccp6);
                        return false;
                    case prtSctp.protoNum:
                        updateTunn(cmd, vrf.vrf.fwd6, vrf.vrf.sctp6);
                        return false;
                }
                return false;
            }
            if (debugger.servP4langErr) {
                logger.debug("got unneeded report: " + cmd.getOriginal());
            }
            return false;
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
            sendAcl(0, "copp4_del ", "", "", "", true, false, copp4f, null, null);
            copp4 = lower.expCopp4;
            sendAcl(0, "copp4_add ", "", "", "", true, false, copp4, null, copp4f);
        }
        if (copp6 != lower.expCopp6) {
            sendAcl(0, "copp6_del ", "", "", "", false, false, copp6f, null, null);
            copp6 = lower.expCopp6;
            sendAcl(0, "copp6_add ", "", "", "", false, false, copp6, null, copp6f);
        }
        for (int i = 0; i < lower.expBr.size(); i++) {
            doBrdg(lower.expBr.get(i));
        }
        doDynAcc();
        for (int i = 0; i < lower.expIfc.size(); i++) {
            servP4langIfc ifc = lower.expIfc.get(i);
            doIface(ifc);
            doNeighs(true, ifc, ifc.ifc.ipIf4);
            doNeighs(false, ifc, ifc.ifc.ipIf6);
        }
        for (int i = 0; i < lower.expVrf.size(); i++) {
            servP4langVrf vrf = lower.expVrf.get(i);
            doVrf(vrf);
            doSockets(false, vrf.id, vrf.vrf.udp4.getProtoNum(), vrf.vrf.udp4.srvrs, vrf.udp4);
            doSockets(true, vrf.id, vrf.vrf.udp6.getProtoNum(), vrf.vrf.udp6.srvrs, vrf.udp6);
            doSockets(false, vrf.id, vrf.vrf.tcp4.getProtoNum(), vrf.vrf.tcp4.srvrs, vrf.tcp4);
            doSockets(true, vrf.id, vrf.vrf.tcp6.getProtoNum(), vrf.vrf.tcp6.srvrs, vrf.tcp6);
            doRoutes(true, vrf.id, vrf.vrf.fwd4.actualU, vrf.routes4, vrf.routed4);
            doRoutes(false, vrf.id, vrf.vrf.fwd6.actualU, vrf.routes6, vrf.routed6);
            doIndexes(true, vrf.id, vrf.vrf.fwd4.actualI, vrf.indexes4, vrf.vrf.fwd4.actualU, vrf.indexed4);
            doIndexes(false, vrf.id, vrf.vrf.fwd6.actualI, vrf.indexes6, vrf.vrf.fwd6.actualU, vrf.indexed6);
            doMroutes(true, vrf.id, vrf.vrf.fwd4.groups, vrf.mroutes4);
            doMroutes(false, vrf.id, vrf.vrf.fwd6.groups, vrf.mroutes6);
            vrf.natCfg4 = doNatCfg(true, vrf.id, vrf.vrf.fwd4.natCfg, vrf.natCfg4, vrf.natCfg4f);
            vrf.natCfg6 = doNatCfg(false, vrf.id, vrf.vrf.fwd6.natCfg, vrf.natCfg6, vrf.natCfg6f);
            doNatTrns(true, vrf.id, vrf.vrf.fwd4.natTrns, vrf.natTrns4);
            doNatTrns(false, vrf.id, vrf.vrf.fwd6.natTrns, vrf.natTrns6);
            doPbrCfg(true, vrf, vrf.vrf.fwd4.pbrCfg, vrf.pbrCfg4);
            doPbrCfg(false, vrf, vrf.vrf.fwd6.pbrCfg, vrf.pbrCfg6);
            vrf.flwSpc4 = doFlwSpc(true, vrf, vrf.vrf.fwd4, vrf.flwSpc4);
            vrf.flwSpc6 = doFlwSpc(false, vrf, vrf.vrf.fwd6, vrf.flwSpc6);
        }
        for (int i = 0; i < tabLabel.labels.size(); i++) {
            doLab1(tabLabel.labels.get(i));
        }
        for (int i = labels.size() - 1; i >= 0; i--) {
            doLab2(labels.get(i));
        }
        for (int i = 0; i < tabNshEntry.services.size(); i++) {
            doNsh1(tabNshEntry.services.get(i));
        }
        for (int i = nshs.size() - 1; i >= 0; i--) {
            doNsh2(nshs.get(i));
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

    private servP4langIfc findIfc(int id) {
        if ((lower.expDynAccIfc != null) && (id >= lower.expDynAcc1st) && (id < (lower.expDynAcc1st + lower.expDynAccSiz))) {
            return lower.expDynAccIfc[id - lower.expDynAcc1st];
        }
        servP4langIfc ntry = new servP4langIfc(id);
        return lower.expIfc.find(ntry);
    }

    private servP4langIfc findIfc(ifcEthTyp ifc) {
        for (int i = 0; i < lower.expIfc.size(); i++) {
            servP4langIfc ntry = lower.expIfc.get(i);
            if (ntry.ifc.ethtyp == ifc) {
                return ntry;
            }
        }
        if (lower.expDynAccIfc == null) {
            return null;
        }
        for (int i = 0; i < lower.expDynAccIfc.length; i++) {
            if (lower.expDynAccIfc[i] == null) {
                continue;
            }
            if (lower.expDynAccIfc[i].ifc.ethtyp == ifc) {
                return lower.expDynAccIfc[i];
            }
        }
        return null;
    }

    private servP4langIfc findIfc(cfgIfc ifc) {
        if (ifc == null) {
            return null;
        }
        if (ifc.cloned == null) {
            for (int i = 0; i < lower.expIfc.size(); i++) {
                servP4langIfc ntry = lower.expIfc.get(i);
                if (ntry.ifc == ifc) {
                    return ntry;
                }
            }
        }
        if (lower.expDynAccIfc == null) {
            return null;
        }
        if (ifc.cloned == null) {
            return null;
        }
        for (int i = 0; i < lower.expDynAccIfc.length; i++) {
            if (lower.expDynAccIfc[i] == null) {
                continue;
            }
            if (lower.expDynAccIfc[i].ifc == ifc) {
                return lower.expDynAccIfc[i];
            }
        }
        return null;
    }

    private servP4langIfc findIfc(tabRouteIface ifc) {
        if (ifc == null) {
            return null;
        }
        for (int i = 0; i < lower.expIfc.size(); i++) {
            servP4langIfc ntry = lower.expIfc.get(i);
            if (ifc == ntry.ifc.fwdIf4) {
                return ntry;
            }
            if (ifc == ntry.ifc.fwdIf6) {
                return ntry;
            }
        }
        if (lower.expDynAccIfc == null) {
            return null;
        }
        for (int i = 0; i < lower.expDynAccIfc.length; i++) {
            servP4langIfc ntry = lower.expDynAccIfc[i];
            if (ntry == null) {
                continue;
            }
            if (ifc == ntry.ifc.fwdIf4) {
                return ntry;
            }
            if (ifc == ntry.ifc.fwdIf6) {
                return ntry;
            }
        }
        return null;
    }

    private servP4langNei findNei(tabRouteIface ifc, addrIP hop) {
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

    private int findDynBr(ifcBridgeIfc ifc) {
        if (lower.expDynBrIfc == null) {
            return -1;
        }
        for (int i = 0; i < lower.expDynBrIfc.length; i++) {
            if (lower.expDynBrIfc[i] == ifc) {
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
        if (ntry.best.labelRem == null) {
            return getNullLabel(ntry);
        }
        if (ntry.best.labelRem.size() < 1) {
            return getNullLabel(ntry);
        }
        int i = ntry.best.labelRem.get(0);
        if (i == ipMpls.labelImp) {
            return getNullLabel(ntry);
        }
        return i;
    }

    private static String doLab5(tabLabelBierN ntry, byte[] full, int sis) {
        byte[] res = ntry.getAndShr(full, sis);
        if (res == null) {
            return " 0 0 0 0 0 0 0 0";
        }
        if (res.length < 1) {
            return " 0 0 0 0 0 0 0 0";
        }
        String a = "";
        for (int i = 0; i < res.length; i += 4) {
            a += " " + bits.msbGetD(res, i);
        }
        return a;
    }

    private void doLab4(ipFwd fwd, tabLabelEntry need, tabLabelEntry done, boolean bef) {
        if (done.bier == null) {
            done.bier = new tabLabelBier(0, 0);
        }
        servP4langVrf vrf = findVrf(fwd);
        if (vrf == null) {
            return;
        }
        int gid = need.getHashW();
        int si = need.label - need.bier.base;
        int bits = tabLabelBier.bsl2num(need.bier.bsl);
        if (bits != 256) {
            return;
        }
        int sis = bits * si;
        byte[] ful = tabLabelBier.bsl2msk(need.bier.bsl);
        for (int i = 0; i < done.bier.peers.size(); i++) {
            tabLabelBierN ntry = done.bier.peers.get(i);
            if (need.bier.peers.find(ntry) != null) {
                continue;
            }
            servP4langNei hop = findNei(ntry.iface, ntry.hop);
            if (hop == null) {
                continue;
            }
            if (hop.mac == null) {
                continue;
            }
            String a = doLab5(ntry, ful, sis);
            servP4langIfc ifc = hop.getVia();
            lower.sendLine("bierlabel" + fwd.ipVersion + "_del " + vrf.id + " " + gid + " " + need.label + " " + ifc.getMcast(gid, hop).id + " " + ifc.id + " " + hop.id + " " + (ntry.label + si) + a);
        }
        String act;
        for (int i = 0; i < need.bier.peers.size(); i++) {
            tabLabelBierN ntry = need.bier.peers.get(i);
            if (done.bier.peers.find(ntry) != null) {
                act = "mod";
            } else {
                act = "add";
            }
            servP4langNei hop = findNei(ntry.iface, ntry.hop);
            if (hop == null) {
                continue;
            }
            if (hop.mac == null) {
                continue;
            }
            String a = doLab5(ntry, ful, sis);
            servP4langIfc ifc = hop.getVia();
            lower.sendLine("bierlabel" + fwd.ipVersion + "_" + act + " " + vrf.id + " " + gid + " " + need.label + " " + ifc.getMcast(gid, hop).id + " " + ifc.id + " " + hop.id + " " + (ntry.label + si) + a);
        }
        if (bef) {
            act = "mod";
        } else {
            act = "add";
        }
        String a = doLab5(need.bier.getIdxMask(), ful, sis);
        lower.sendLine("bierlabloc" + fwd.ipVersion + "_" + act + " " + vrf.id + " " + gid + " " + need.label + a);
    }

    private void doLab3(ipFwd fwd, tabLabelEntry need, tabLabelEntry done, boolean bef) {
        if (done.duplicate == null) {
            done.duplicate = new tabGen<tabLabelDup>();
        }
        servP4langVrf vrf = findVrf(fwd);
        if (vrf == null) {
            return;
        }
        int gid = need.getHashW();
        int now = 0;
        if (need.needLocal) {
            now++;
        }
        for (int i = 0; i < done.duplicate.size(); i++) {
            tabLabelDup ntry = done.duplicate.get(i);
            if (need.duplicate.find(ntry) != null) {
                continue;
            }
            servP4langNei hop = findNei(ntry.iface, ntry.hop);
            if (hop == null) {
                continue;
            }
            if (hop.mac == null) {
                continue;
            }
            servP4langIfc ifc = hop.getVia();
            lower.sendLine("duplabel" + fwd.ipVersion + "_del " + vrf.id + " " + gid + " " + need.label + " " + ifc.getMcast(gid, hop).id + " " + ifc.id + " " + hop.id + " " + getLabel(ntry.label));
        }
        String act;
        for (int i = 0; i < need.duplicate.size(); i++) {
            tabLabelDup ntry = need.duplicate.get(i);
            if (done.duplicate.find(ntry) != null) {
                act = "mod";
            } else {
                act = "add";
            }
            servP4langNei hop = findNei(ntry.iface, ntry.hop);
            if (hop == null) {
                continue;
            }
            if (hop.mac == null) {
                continue;
            }
            servP4langIfc ifc = hop.getVia();
            lower.sendLine("duplabel" + fwd.ipVersion + "_" + act + " " + vrf.id + " " + gid + " " + need.label + " " + ifc.getMcast(gid, hop).id + " " + ifc.id + " " + hop.id + " " + getLabel(ntry.label));
            now++;
        }
        if (bef) {
            act = "mod";
            if (now < 1) {
                act = "del";
            }
        } else {
            act = "add";
        }
        lower.sendLine("duplabloc" + fwd.ipVersion + "_" + (need.needLocal ? "add " : "del ") + vrf.id + " " + gid + " " + need.label + " " + act);
    }

    private void doLab2(tabLabelEntry ntry) {
        if (tabLabel.labels.find(ntry) != null) {
            return;
        }
        labels.del(ntry);
        if (ntry.bier != null) {
            tabLabelEntry empty = new tabLabelEntry(ntry.label);
            empty.bier = new tabLabelBier(0, 0);
            doLab4(ntry.forwarder, empty, ntry, true);
            return;
        }
        if (ntry.duplicate != null) {
            tabLabelEntry empty = new tabLabelEntry(ntry.label);
            empty.duplicate = new tabGen<tabLabelDup>();
            doLab3(ntry.forwarder, empty, ntry, true);
            return;
        }
        if (ntry.nextHop == null) {
            servP4langVrf vrf = findVrf(ntry.forwarder);
            if (vrf == null) {
                return;
            }
            lower.sendLine("mylabel" + ntry.forwarder.ipVersion + "_del" + " " + ntry.label + " " + vrf.id);
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
            bits.msbPutD(adr.getBytes(), 12, ntry.label);
            lower.sendLine("mysrv" + ntry.forwarder.ipVersion + "_del " + vr.id + " " + adr + " " + vrf.id);
            return;
        }
        String afi;
        if (ntry.nextHop.isIPv4()) {
            afi = "4";
        } else {
            afi = "6";
        }
        servP4langNei hop = findNei(ntry.iface, ntry.nextHop);
        if (hop == null) {
            return;
        }
        int lab = getLabel(ntry.remoteLab);
        if (lab < 0) {
            lower.sendLine("unlabel" + afi + "_del " + ntry.label + " " + hop.id + " " + ntry.nextHop);
        } else {
            lower.sendLine("label" + afi + "_del " + ntry.label + " " + hop.id + " " + ntry.nextHop + " " + lab);
        }
    }

    private void doLab1(tabLabelEntry ntry) {
        if (ntry == null) {
            return;
        }
        ntry = ntry.copyBytes();
        if (ntry.pweIfc != null) {
            return;
        }
        if (ntry.bier != null) {
            tabLabelEntry old = labels.find(ntry);
            boolean bef;
            if (old != null) {
                if (!old.differs(ntry)) {
                    return;
                }
                bef = true;
            } else {
                old = new tabLabelEntry(ntry.label);
                bef = false;
            }
            labels.put(ntry);
            doLab4(ntry.forwarder, ntry, old, bef);
            return;
        }
        if (ntry.duplicate != null) {
            tabLabelEntry old = labels.find(ntry);
            boolean bef;
            if (old != null) {
                if (!old.differs(ntry)) {
                    return;
                }
                bef = true;
            } else {
                old = new tabLabelEntry(ntry.label);
                bef = false;
            }
            labels.put(ntry);
            doLab3(ntry.forwarder, ntry, old, bef);
            return;
        }
        if (ntry.nextHop == null) {
            servP4langVrf vrf = findVrf(ntry.forwarder);
            if (vrf == null) {
                return;
            }
            tabLabelEntry old = labels.find(ntry);
            String act = "add";
            if (old != null) {
                if (!old.differs(ntry)) {
                    return;
                }
                act = "mod";
            }
            labels.put(ntry);
            lower.sendLine("mylabel" + ntry.forwarder.ipVersion + "_" + act + " " + ntry.label + " " + vrf.id);
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
            bits.msbPutD(adr.getBytes(), 12, ntry.label);
            lower.sendLine("mysrv" + ntry.forwarder.ipVersion + "_" + act + " " + vr.id + " " + adr + " " + vrf.id);
            return;
        }
        servP4langNei hop = findNei(ntry.iface, ntry.nextHop);
        if (hop == null) {
            return;
        }
        tabLabelEntry old = labels.find(ntry);
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
            lower.sendLine("unlabel" + afi + "_" + act + " " + ntry.label + " " + hop.id + " " + ntry.nextHop);
        } else {
            lower.sendLine("label" + afi + "_" + act + " " + ntry.label + " " + hop.id + " " + ntry.nextHop + " " + lab);
        }
    }

    private String doNsh3(tabNshEntry ntry, String act) {
        if (ntry.iface != null) {
            servP4langIfc ifc = findIfc(ntry.iface);
            if (ifc == null) {
                return null;
            }
            return "nshfwd_" + act + " " + ntry.sp + " " + ntry.si + " " + ifc.id + " " + ifc.getMac().toEmuStr() + " " + ntry.target.toEmuStr() + " " + ntry.trgSp + " " + ntry.trgSi;
        }
        if (ntry.route4 != null) {
            servP4langVrf vrf = findVrf(ntry.route4);
            if (vrf == null) {
                return null;
            }
            return "nshloc_" + act + " " + ntry.sp + " " + ntry.si + " " + vrf.id + "";
        }
        return null;
    }

    private void doNsh2(tabNshEntry ntry) {
        if (tabNshEntry.services.find(ntry) != null) {
            return;
        }
        nshs.del(ntry);
        String act = doNsh3(ntry, "del");
        if (act == null) {
            return;
        }
        lower.sendLine(act);
    }

    private void doNsh1(tabNshEntry ntry) {
        if (ntry == null) {
            return;
        }
        ntry = ntry.copyBytes();
        tabNshEntry old = nshs.find(ntry);
        String act = "add";
        if (old != null) {
            if (!old.differs(ntry)) {
                return;
            }
            act = "mod";
        }
        act = doNsh3(ntry, act);
        if (act == null) {
            return;
        }
        nshs.put(ntry);
        lower.sendLine(act);
    }

    private void addDynBr(servP4langBr br, ifcBridgeIfc ntry, ifcDn ifc) {
        if (lower.expDynBrIfc == null) {
            return;
        }
        lower.expDynBrNxt = (lower.expDynBrNxt + 1) % lower.expDynBrSiz;
        if (lower.expDynBrIfc[lower.expDynBrNxt] != null) {
            br.ifcs.del(lower.expDynBrIfc[lower.expDynBrNxt]);
        }
        lower.expDynBrIfc[lower.expDynBrNxt] = ntry;
        br.ifcs.put(ntry);
        lower.sendLine("portbridge_add " + (lower.expDynBr1st + lower.expDynBrNxt) + " " + br.br.num);
    }

    private tabRouteEntry<addrIP> convRou(tabRouteEntry<addrIP> rou, boolean nhchk) {
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
            default:
                return rou;
        }
    }

    private servP4langNei findHop(ipFwd fwd, addrIP adr) {
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

    private void doBrdg(servP4langBr br) {
        br.routed = findIfc(br.br) != null;
        if (br.routed) {
            return;
        }
        tabGen<ifcBridgeIfc> seenI = new tabGen<ifcBridgeIfc>();
        for (int i = 0;; i++) {
            ifcBridgeIfc ntry = br.br.bridgeHed.getIface(i);
            if (ntry == null) {
                break;
            }
            seenI.put(ntry);
            if (br.ifcs.find(ntry) != null) {
                continue;
            }
            try {
                clntVxlan ifc = (clntVxlan) ntry.lowerIf;
                addDynBr(br, ntry, ifc);
                continue;
            } catch (Exception e) {
            }
            try {
                servVxlanConn ifc = (servVxlanConn) ntry.lowerIf;
                addDynBr(br, ntry, ifc);
                continue;
            } catch (Exception e) {
            }
            try {
                clntPckOudp ifc = (clntPckOudp) ntry.lowerIf;
                addDynBr(br, ntry, ifc);
                continue;
            } catch (Exception e) {
            }
            try {
                servPckOudpConn ifc = (servPckOudpConn) ntry.lowerIf;
                addDynBr(br, ntry, ifc);
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
        for (int i = br.ifcs.size() - 1; i >= 0; i--) {
            ifcBridgeIfc ntry = br.ifcs.get(i);
            if (seenI.find(ntry) != null) {
                continue;
            }
            br.ifcs.del(ntry);
            int brif = findDynBr(ntry);
            if (brif < 0) {
                continue;
            }
            lower.sendLine("portbridge_del " + (brif + lower.expDynBr1st) + " " + br.br.num);
            String s = lower.expDynBrTun[brif];
            lower.expDynBrIfc[brif] = null;
            lower.expDynBrTun[brif] = null;
            if (s == null) {
                continue;
            }
            cmds cmd = new cmds("lin", s);
            s = cmd.word() + " ";
            s = s.replaceAll("_add ", "_del ");
            s = s.replaceAll("_mod ", "_del ");
            s += cmd.word();
            cmd.word();
            lower.sendLine(s + " " + addrMac.getRandom().toEmuStr() + " " + cmd.getRemaining());
        }
        tabGen<ifcBridgeAdr> seenM = new tabGen<ifcBridgeAdr>();
        for (int i = 0;; i++) {
            ifcBridgeAdr ntry = br.br.bridgeHed.getMacAddr(i);
            if (ntry == null) {
                break;
            }
            ntry = ntry.copyBytes();
            seenM.put(ntry);
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
                servP4langNei nei = findNei(ifc.ifc.fwdIf4, new addrIP());
                if (nei == null) {
                    nei = findNei(ifc.ifc.fwdIf6, new addrIP());
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
                int brif = findDynBr(ntry.ifc);
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
                ipFwd ofwd = iface.vrf.getFwd(adr);
                servP4langVrf ovrf = findVrf(ofwd);
                if (ovrf == null) {
                    continue;
                }
                servP4langNei hop = findHop(ofwd, adr);
                if (hop == null) {
                    continue;
                }
                a = "bridgevxlan" + (adr.isIPv4() ? "4" : "6") + "_" + a + " " + br.br.num + " " + ntry.adr.toEmuStr() + " " + src + " " + adr + " " + hop.id + " " + iface.inst + " " + ovrf.id + " " + (brif + lower.expDynBr1st);
                lower.expDynBrTun[brif] = a;
                lower.sendLine(a);
                continue;
            } catch (Exception e) {
            }
            try {
                servVxlanConn iface = (servVxlanConn) ntry.ifc.lowerIf;
                int brif = findDynBr(ntry.ifc);
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
                ipFwd ofwd = iface.getFwder();
                servP4langVrf ovrf = findVrf(ofwd);
                if (ovrf == null) {
                    continue;
                }
                servP4langNei hop = findHop(ofwd, adr);
                if (hop == null) {
                    continue;
                }
                a = "bridgevxlan" + (adr.isIPv4() ? "4" : "6") + "_" + a + " " + br.br.num + " " + ntry.adr.toEmuStr() + " " + src + " " + adr + " " + hop.id + " " + iface.getInst() + " " + ovrf.id + " " + (brif + lower.expDynBr1st);
                lower.sendLine(a);
                lower.expDynBrTun[brif] = a;
                continue;
            } catch (Exception e) {
            }
            try {
                clntPckOudp iface = (clntPckOudp) ntry.ifc.lowerIf;
                int brif = findDynBr(ntry.ifc);
                if (brif < 0) {
                    continue;
                }
                adr = iface.getRemAddr();
                if (adr == null) {
                    continue;
                }
                addrIP src = iface.getLocAddr();
                if (src == null) {
                    continue;
                }
                ipFwd ofwd = iface.vrf.getFwd(adr);
                servP4langVrf ovrf = findVrf(ofwd);
                if (ovrf == null) {
                    continue;
                }
                servP4langNei hop = findHop(ofwd, adr);
                if (hop == null) {
                    continue;
                }
                a = "bridgepckoudp" + (adr.isIPv4() ? "4" : "6") + "_" + a + " " + br.br.num + " " + ntry.adr.toEmuStr() + " " + src + " " + adr + " " + iface.getLocPort() + " " + iface.getRemPort() + " " + hop.id + " " + ovrf.id + " " + (brif + lower.expDynBr1st);
                lower.sendLine(a);
                lower.expDynBrTun[brif] = a;
                continue;
            } catch (Exception e) {
            }
            try {
                servPckOudpConn iface = (servPckOudpConn) ntry.ifc.lowerIf;
                int brif = findDynBr(ntry.ifc);
                if (brif < 0) {
                    continue;
                }
                adr = iface.getRemAddr();
                if (adr == null) {
                    continue;
                }
                addrIP src = iface.getLocAddr();
                if (src == null) {
                    continue;
                }
                ipFwd ofwd = iface.getFwder();
                servP4langVrf ovrf = findVrf(ofwd);
                if (ovrf == null) {
                    continue;
                }
                servP4langNei hop = findHop(ofwd, adr);
                if (hop == null) {
                    continue;
                }
                a = "bridgepckoudp" + (adr.isIPv4() ? "4" : "6") + "_" + a + " " + br.br.num + " " + ntry.adr.toEmuStr() + " " + src + " " + adr + " " + iface.getLocPort() + " " + iface.getRemPort() + " " + hop.id + " " + ovrf.id + " " + (brif + lower.expDynBr1st);
                lower.sendLine(a);
                lower.expDynBrTun[brif] = a;
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
            rou = convRou(rou, false);
            if (l < 1) {
                continue;
            }
            if (rou == null) {
                continue;
            }
            servP4langNei hop = findNei(rou.best.iface, rou.best.nextHop);
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
            if (seenM.find(ntry) != null) {
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
        lower.sendLine("polkaown_add 0 " + vrf.id);
        lower.sendLine("myaddr4_add 224.0.0.0/4 -1 " + vrf.id);
        lower.sendLine("myaddr4_add 255.255.255.255/32 -1 " + vrf.id);
        lower.sendLine("myaddr6_add ff00::/8 -1 " + vrf.id);
    }

    private void doDynAcc() {
        if (lower.expDynAccIfc == null) {
            return;
        }
        for (int i = 0; i < cfgAll.ifaces.size(); i++) {
            cfgIfc ifc = cfgAll.ifaces.get(i);
            if (ifc == null) {
                continue;
            }
            if (ifc.cloned == null) {
                continue;
            }
            servP4langIfc ntry = findIfc(ifc);
            if (ntry != null) {
                doIface(ntry);
                doNeighs(true, ntry, ntry.ifc.ipIf4);
                doNeighs(false, ntry, ntry.ifc.ipIf6);
                continue;
            }
            servP4langIfc prnt = findIfc(ifc.cloned);
            if (prnt == null) {
                continue;
            }
            lower.expDynAccNxt = (lower.expDynAccNxt + 1) % lower.expDynAccSiz;
            ntry = new servP4langIfc(lower.expDynAccNxt + lower.expDynAcc1st);
            ntry.doClear();
            ntry.ifc = ifc;
            ntry.lower = lower;
            ntry.cloned = prnt;
            ntry.ifc.ethtyp.hwHstry = new history();
            ntry.ifc.ethtyp.hwCntr = new counter();
            lower.expDynAccIfc[lower.expDynAccNxt] = ntry;
        }
    }

    private void doIface(servP4langIfc ifc) {
        int i = -1;
        if (ifc.ifc.ethtyp.monSes != null) {
            servP4langIfc res = findIfc(ifc.ifc.ethtyp.monSes);
            if (res != null) {
                i = res.id;
            }
        }
        if (i != ifc.sentMon) {
            int o = i;
            String a;
            if (ifc.sentMon < 0) {
                a = "add";
            } else {
                a = "mod";
            }
            if (i < 0) {
                o = ifc.sentMon;
                a = "del";
            }
            int smp = ifc.ifc.ethtyp.monSmpN;
            if (smp < 1) {
                smp = 1;
            }
            int trn = ifc.ifc.ethtyp.monTrnc;
            if (trn < 1) {
                trn = ifc.ifc.ethtyp.getMTUsize();
            }
            lower.sendLine("monitor_" + a + " " + ifc.id + " " + o + " " + ifc.ifc.ethtyp.monDir + " " + smp + " " + trn);
            ifc.sentMon = i;
        }
        tabListing<tabAceslstN<addrIP>, addrIP> acl = null;
        String a = null;
        if (ifc.ifc.ethtyp.qosIn != null) {
            tabQosN qos = ifc.ifc.ethtyp.qosIn.getClass(0);
            acl = qos.entry.aclMatch;
            a = qos.getBytePerInt() + " " + qos.getInterval();
        }
        if (ifc.sentQos4in != acl) {
            if (a != null) {
                lower.sendLine("inqos_add " + ifc.id + " " + a);
            }
            sendAcl(0, "inqos4_del " + ifc.id + " " + ifc.id + " ", "", "", "", true, true, ifc.sentQos4inF, null, null);
            ifc.sentQos4in = acl;
            sendAcl(0, "inqos4_add " + ifc.id + " " + ifc.id + " ", "", "", "", true, true, ifc.sentQos4in, null, ifc.sentQos4inF);
        }
        if (ifc.sentQos6in != acl) {
            sendAcl(0, "inqos6_del " + ifc.id + " " + ifc.id + " ", "", "", "", false, true, ifc.sentQos6inF, null, null);
            ifc.sentQos6in = acl;
            sendAcl(0, "inqos6_add " + ifc.id + " " + ifc.id + " ", "", "", "", false, true, ifc.sentQos6in, null, ifc.sentQos6inF);
        }
        acl = null;
        a = null;
        if (ifc.ifc.ethtyp.qosOut != null) {
            tabQosN qos = ifc.ifc.ethtyp.qosOut.getClass(0);
            acl = qos.entry.aclMatch;
            a = qos.getBytePerInt() + " " + qos.getInterval();
        }
        if (ifc.sentQos4out != acl) {
            if (a != null) {
                lower.sendLine("outqos_add " + ifc.id + " " + a);
            }
            sendAcl(0, "outqos4_del " + ifc.id + " " + ifc.id + " ", "", "", "", true, false, ifc.sentQos4outF, null, null);
            ifc.sentQos4out = acl;
            sendAcl(0, "outqos4_add " + ifc.id + " " + ifc.id + " ", "", "", "", true, false, ifc.sentQos4out, null, ifc.sentQos4outF);
        }
        if (ifc.sentQos6out != acl) {
            sendAcl(0, "outqos6_del " + ifc.id + " " + ifc.id + " ", "", "", "", false, false, ifc.sentQos6outF, null, null);
            ifc.sentQos6out = acl;
            sendAcl(0, "outqos6_add " + ifc.id + " " + ifc.id + " ", "", "", "", false, false, ifc.sentQos6out, null, ifc.sentQos6outF);
        }
        if (ifc.ifc.ethtyp.macSec == null) {
            if (ifc.sentMacsec != null) {
                lower.sendLine("macsec_del " + ifc.id + " " + ifc.sentMacsec);
            }
            ifc.sentMacsec = null;
        } else {
            String s = null;
            if (ifc.ifc.ethtyp.macSec.keyHash != null) {
                s = ifc.ifc.ethtyp.macSec.myTyp + " " + ifc.ifc.ethtyp.macSec.cphrSiz + " " + ifc.ifc.ethtyp.macSec.hashSiz + " " + (ifc.ifc.ethtyp.macSec.needLayer2 ? "1" : "0") + " " + ifc.ifc.ethtyp.macSec.profil.trans.encr2str() + " " + ifc.ifc.ethtyp.macSec.profil.trans.hash2str() + " " + bits.toHex(ifc.ifc.ethtyp.macSec.keyEncr) + " " + bits.toHex(ifc.ifc.ethtyp.macSec.keyHash);
            }
            if (s != null) {
                if (ifc.sentMacsec != null) {
                    if (!s.equals(ifc.sentMacsec)) {
                        lower.sendLine("macsec_mod " + ifc.id + " " + s);
                    }
                } else {
                    lower.sendLine("macsec_add " + ifc.id + " " + s);
                }
            } else {
                if (ifc.sentMacsec != null) {
                    lower.sendLine("macsec_del " + ifc.id + " " + ifc.sentMacsec);
                }
            }
            ifc.sentMacsec = s;
        }
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
        i = ifc.ifc.ethtyp.getMTUsize();
        if ((ifc.master != null) || (ifc.ifc.type == cfgIfc.ifaceType.bundle) || (ifc.ifc.type == cfgIfc.ifaceType.bridge) || (ifc.ifc.type == cfgIfc.ifaceType.dialer) || (ifc.ifc.type == cfgIfc.ifaceType.hairpin) || (ifc.ifc.type == cfgIfc.ifaceType.tunnel) || (ifc.ifc.type == cfgIfc.ifaceType.virtppp)) {
            ifc.sentState = sta;
            ifc.sentMtu = i;
        }
        if (ifc.sentState != sta) {
            if (sta == state.states.up) {
                a = "1";
            } else {
                a = "0";
            }
            lower.sendLine("state " + ifc.id + " " + a + " " + ifc.getStateEnding());
            ifc.sentState = sta;
        }
        if (ifc.sentMtu != i) {
            lower.sendLine("mtu " + ifc.id + " " + i);
            ifc.sentMtu = i;
        }
        i = -1;
        int o = -1;
        if (ifc.ifc.polkaPack != null) {
            i = ifc.ifc.polkaPack.localId;
            o = ifc.ifc.polkaPack.coeffs[i].intCoeff();
        }
        if (i != ifc.sentPolka) {
            if (ifc.sentPolka >= 0) {
                a = "mod";
            } else {
                a = "add";
            }
            if (i < 0) {
                a = "del";
            }
            lower.sendLine("polkapoly_" + a + " " + ifc.id + " " + o);
            ifc.sentPolka = i;
        }
        if ((ifc.master != null) && (ifc.sentVlan == 0)) {
            lower.sendLine("portvlan_add " + ifc.id + " " + ifc.master.id + " " + ifc.ifc.vlanNum);
            ifc.sentVlan = ifc.ifc.vlanNum;
        }
        if (ifc.ifc.hairpinHed != null) {
            o = 0;
            for (i = 0; i < lower.expIfc.size(); i++) {
                servP4langIfc ntry = lower.expIfc.get(i);
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
            ifc.members = prt;
            List<servP4langIfc> vln = new ArrayList<servP4langIfc>();
            for (i = 0; i < lower.expIfc.size(); i++) {
                servP4langIfc ntry = lower.expIfc.get(i);
                if (ntry == ifc) {
                    continue;
                }
                if (ntry.master != ifc) {
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
                    prt.add(new servP4langIfc(0));
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
                for (o = 0; o < prt.size(); o++) {
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
            servP4langBr br = new servP4langBr(ifc.ifc.bridgeHed.num);
            br = lower.expBr.find(br);
            if (br == null) {
                br = new servP4langBr(0);
            }
            if (!br.routed) {
                if (ifc.sentAcl4in1 != ifc.ifc.bridgeIfc.filter4in) {
                    sendAcl(0, "inacl4_del " + ifc.id + " ", "", "", "", true, false, ifc.sentAcl4inF, null, null);
                    ifc.sentAcl4in1 = ifc.ifc.bridgeIfc.filter4in;
                    ifc.sentAcl4in2 = null;
                    sendAcl(0, "inacl4_add " + ifc.id + " ", "", "", "", true, false, ifc.sentAcl4in1, null, ifc.sentAcl4inF);
                }
                if (ifc.sentAcl4out1 != ifc.ifc.bridgeIfc.filter4out) {
                    sendAcl(0, "outacl4_del " + ifc.id + " ", "", "", "", true, false, ifc.sentAcl4outF, null, null);
                    ifc.sentAcl4out1 = ifc.ifc.bridgeIfc.filter4out;
                    ifc.sentAcl4out2 = null;
                    sendAcl(0, "outacl4_add " + ifc.id + " ", "", "", "", true, false, ifc.sentAcl4out1, null, ifc.sentAcl4outF);
                }
                if (ifc.sentAcl6in1 != ifc.ifc.bridgeIfc.filter6in) {
                    sendAcl(0, "inacl6_del " + ifc.id + " ", "", "", "", false, false, ifc.sentAcl6inF, null, null);
                    ifc.sentAcl6in1 = ifc.ifc.bridgeIfc.filter6in;
                    ifc.sentAcl6in2 = null;
                    sendAcl(0, "inacl6_add " + ifc.id + " ", "", "", "", false, false, ifc.sentAcl6in1, null, ifc.sentAcl6inF);
                }
                if (ifc.sentAcl6out1 != ifc.ifc.bridgeIfc.filter6out) {
                    sendAcl(0, "outacl6_del " + ifc.id + " ", "", "", "", false, false, ifc.sentAcl6outF, null, null);
                    ifc.sentAcl6out1 = ifc.ifc.bridgeIfc.filter6out;
                    ifc.sentAcl6out2 = null;
                    sendAcl(0, "outacl6_add " + ifc.id + " ", "", "", "", false, false, ifc.sentAcl6out1, null, ifc.sentAcl6outF);
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
            ntry = convRou(ntry, false);
            if (ntry == null) {
                return;
            }
            if (ntry.best.iface == null) {
                return;
            }
            servP4langNei hop = findNei(ntry.best.iface, ntry.best.nextHop);
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
            if ((ifc.sentAcl4in1 != mstr.ifc.fwdIf4.filterIn) || (ifc.sentAcl4in2 != mstr.ifc.fwdIf4.cfilterIn)) {
                sendAcl(0, "inacl4_del " + ifc.id + " ", "", "", "", true, false, ifc.sentAcl4inF, null, null);
                ifc.sentAcl4in1 = mstr.ifc.fwdIf4.filterIn;
                ifc.sentAcl4in2 = mstr.ifc.fwdIf4.cfilterIn;
                sendAcl(0, "inacl4_add " + ifc.id + " ", "", "", "", true, false, ifc.sentAcl4in1, ifc.sentAcl4in2, ifc.sentAcl4inF);
            }
            if ((ifc.sentAcl4out1 != mstr.ifc.fwdIf4.filterOut) || (ifc.sentAcl4out2 != mstr.ifc.fwdIf4.cfilterOut)) {
                sendAcl(0, "outacl4_del " + ifc.id + " ", "", "", "", true, false, ifc.sentAcl4outF, null, null);
                ifc.sentAcl4out1 = mstr.ifc.fwdIf4.filterOut;
                ifc.sentAcl4out2 = mstr.ifc.fwdIf4.cfilterOut;
                sendAcl(0, "outacl4_add " + ifc.id + " ", "", "", "", true, false, ifc.sentAcl4out1, ifc.sentAcl4out2, ifc.sentAcl4outF);
            }
        }
        if (mstr.ifc.fwdIf6 != null) {
            if ((ifc.sentAcl6in1 != mstr.ifc.fwdIf6.filterIn) || (ifc.sentAcl6in2 != mstr.ifc.fwdIf6.cfilterIn)) {
                sendAcl(0, "inacl6_del " + ifc.id + " ", "", "", "", false, false, ifc.sentAcl6inF, null, null);
                ifc.sentAcl6in1 = mstr.ifc.fwdIf6.filterIn;
                ifc.sentAcl6in2 = mstr.ifc.fwdIf6.cfilterIn;
                sendAcl(0, "inacl6_add " + ifc.id + " ", "", "", "", false, false, ifc.sentAcl6in1, ifc.sentAcl6in2, ifc.sentAcl6inF);
            }
            if ((ifc.sentAcl6out1 != mstr.ifc.fwdIf6.filterOut) || (ifc.sentAcl6out2 != mstr.ifc.fwdIf6.cfilterOut)) {
                sendAcl(0, "outacl6_del " + ifc.id + " ", "", "", "", false, false, ifc.sentAcl6outF, null, null);
                ifc.sentAcl6out1 = mstr.ifc.fwdIf6.filterOut;
                ifc.sentAcl6out2 = mstr.ifc.fwdIf6.cfilterOut;
                sendAcl(0, "outacl6_add " + ifc.id + " ", "", "", "", false, false, ifc.sentAcl6out1, ifc.sentAcl6out2, ifc.sentAcl6outF);
            }
        }
        if (vrf.id == ifc.sentVrf) {
            return;
        }
        lower.sendLine("portvrf_" + a + " " + ifc.id + " " + vrf.id);
        if (ifc.ifc.fwdIf4 != null) {
            lower.sendLine("tcpmss4in_" + a + " " + ifc.id + " " + ifc.ifc.fwdIf4.tcpMssIn);
            lower.sendLine("tcpmss4out_" + a + " " + ifc.id + " " + ifc.ifc.fwdIf4.tcpMssOut);
        }
        if (ifc.ifc.fwdIf6 != null) {
            lower.sendLine("tcpmss6in_" + a + " " + ifc.id + " " + ifc.ifc.fwdIf6.tcpMssIn);
            lower.sendLine("tcpmss6out_" + a + " " + ifc.id + " " + ifc.ifc.fwdIf6.tcpMssOut);
        }
        ifc.sentVrf = vrf.id;
    }

    private void doNeighs(servP4langNei ntry) {
        if (ntry.need < 1) {
            neighs.del(ntry);
            if (ntry.sentIgNhop >= 0) {
                lower.sendLine("nhop2port_del " + ntry.id + " " + ntry.iface.id + " " + ntry.sentIgNhop);
            }
            if (ntry.mac == null) {
                return;
            }
            lower.sendLine("neigh" + (ntry.adr.isIPv4() ? "4" : "6") + "_del " + ntry.id + " " + ntry.adr + " " + ntry.mac.toEmuStr() + " " + ntry.vrf.id + " " + ntry.iface.getMac().toEmuStr() + " " + ntry.sentIfc);
            return;
        }
        int val = ntry.getVia().getUcast().id;
        if (val == ntry.sentIgNhop) {
            return;
        }
        String act;
        if (ntry.sentIgNhop < 0) {
            act = "add";
        } else {
            act = "mod";
        }
        lower.sendLine("nhop2port_" + act + " " + ntry.id + " " + ntry.iface.id + " " + val);
        ntry.sentIgNhop = val;
    }

    private String getIpsecParam(packEsp esp) {
        return " " + esp.spi + " " + bits.toHex(esp.keyEncr) + " " + bits.toHex(esp.keyHash);
    }

    private String getIpsecParam(packEsp rx, packEsp tx, secTransform ts) {
        if (rx == null) {
            return "";
        }
        if (tx == null) {
            return "";
        }
        if (ts == null) {
            return "";
        }
        if (rx.keyHash == null) {
            return "";
        }
        if (tx.keyHash == null) {
            return "";
        }
        return " " + rx.encrSize + " " + rx.hashSize + " " + ts.encr2str() + " " + ts.hash2str() + getIpsecParam(rx) + getIpsecParam(tx);
    }

    private void doNeighs(boolean ipv4, servP4langIfc ifc, ipIfc ipi) {
        if (ipi == null) {
            return;
        }
        servP4langVrf vrf = findVrf(ifc);
        if (vrf == null) {
            return;
        }
        if (ifc.cloned != null) {
            servP4langNei nei = findNei(ifc.ifc.fwdIf4, new addrIP());
            if (nei == null) {
                nei = findNei(ifc.ifc.fwdIf6, new addrIP());
            }
            if (nei == null) {
                return;
            }
            nei.need++;
            nei.vrf = vrf;
            try {
                ifcP2pOEservSess ntry = (ifcP2pOEservSess) ifc.ifc.lower;
                addrMac macL = new addrMac();
                servP4langIfc prnt = findIfc(ntry.getLower(macL));
                if (prnt == null) {
                    return;
                }
                addrMac macR = new addrMac();
                int ses = ntry.getSession(macR);
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
                nei.viaI = prnt;
                lower.sendLine("pppoe_" + act + " " + ifc.id + " " + prnt.id + " " + nei.id + " " + vrf.id + " " + sess + " " + macR.toEmuStr() + " " + macL.toEmuStr());
                ifc.sentPppoe = ses;
                return;
            } catch (Exception e) {
            }
            try {
                servL2tp2sess ntry = (servL2tp2sess) ifc.ifc.lower;
                addrIP src = ntry.getAddrLoc();
                if (src == null) {
                    return;
                }
                addrIP trg = ntry.getAddrRem();
                if (trg == null) {
                    return;
                }
                ipFwd ofwd = ntry.getFwder();
                servP4langVrf ovrf = findVrf(ofwd);
                if (ovrf == null) {
                    return;
                }
                servP4langNei hop = findHop(ofwd, trg);
                if (hop == null) {
                    return;
                }
                if (hop.mac == null) {
                    return;
                }
                int tun = ntry.getTunnRem();
                if (tun < 1) {
                    return;
                }
                int ses = ntry.getSessRem();
                if (ses < 1) {
                    return;
                }
                tun = (tun << 16) | ses;
                int lp = ntry.getPortLoc();
                if (lp < 1) {
                    return;
                }
                int rp = ntry.getPortRem();
                if (rp < 1) {
                    return;
                }
                String act;
                if (nei.mac == null) {
                    act = "add";
                } else {
                    act = "mod";
                    if ((hop.mac.compare(hop.mac, nei.mac) == 0) && (nei.viaH == hop) && (nei.sentIfc == hop.sentIfc) && (nei.sentTun == lp)) {
                        return;
                    }
                }
                nei.viaH = hop;
                nei.mac = hop.mac.copyBytes();
                nei.sentIfc = hop.sentIfc;
                nei.sentTun = lp;
                String afi;
                if (trg.isIPv4()) {
                    afi = "4";
                } else {
                    afi = "6";
                }
                lower.sendLine("l2tp" + afi + "_" + act + " " + nei.id + " " + ifc.id + " " + hop.sentIfc + " " + src + " " + trg + " " + hop.mac.toEmuStr() + " " + ovrf.id + " " + hop.iface.getMac().toEmuStr() + " " + lp + " " + rp + " " + tun);
            } catch (Exception e) {
            }
            try {
                servAmtConn ntry = (servAmtConn) ifc.ifc.lower;
                addrIP src = ntry.getAddrLoc();
                if (src == null) {
                    return;
                }
                addrIP trg = ntry.getAddrRem();
                if (trg == null) {
                    return;
                }
                ipFwd ofwd = ntry.getFwder();
                servP4langVrf ovrf = findVrf(ofwd);
                if (ovrf == null) {
                    return;
                }
                servP4langNei hop = findHop(ofwd, trg);
                if (hop == null) {
                    return;
                }
                if (hop.mac == null) {
                    return;
                }
                int lp = ntry.getPortLoc();
                if (lp < 1) {
                    return;
                }
                int rp = ntry.getPortRem();
                if (rp < 1) {
                    return;
                }
                String act;
                if (nei.mac == null) {
                    act = "add";
                } else {
                    act = "mod";
                    if ((hop.mac.compare(hop.mac, nei.mac) == 0) && (nei.viaH == hop) && (nei.sentIfc == hop.sentIfc) && (nei.sentTun == lp)) {
                        return;
                    }
                }
                nei.viaH = hop;
                nei.mac = hop.mac.copyBytes();
                nei.sentIfc = hop.sentIfc;
                nei.sentTun = lp;
                String afi;
                if (trg.isIPv4()) {
                    afi = "4";
                } else {
                    afi = "6";
                }
                lower.sendLine("amt" + afi + "_" + act + " " + nei.id + " " + ifc.id + " " + hop.sentIfc + " " + src + " " + trg + " " + hop.mac.toEmuStr() + " " + ovrf.id + " " + hop.iface.getMac().toEmuStr() + " " + lp + " " + rp);
            } catch (Exception e) {
            }
            return;
        }
        if (ifc.ifc.type == cfgIfc.ifaceType.virtppp) {
            servP4langNei nei = findNei(ifc.ifc.fwdIf4, new addrIP());
            if (nei == null) {
                nei = findNei(ifc.ifc.fwdIf6, new addrIP());
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
            servP4langNei hop = findHop(ofwd, ifc.ifc.pwhe.adr);
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
                if ((hop.mac.compare(hop.mac, nei.mac) == 0) && (nei.viaH == hop) && (nei.sentIfc == hop.sentIfc) && (nei.sentTun == lp)) {
                    return;
                }
            }
            nei.viaH = hop;
            nei.mac = hop.mac.copyBytes();
            nei.sentIfc = hop.sentIfc;
            nei.sentTun = lp;
            String afi;
            if (ifc.ifc.pwhe.adr.isIPv4()) {
                afi = "4";
            } else {
                afi = "6";
            }
            lower.sendLine("l2tp" + afi + "_" + act + " " + nei.id + " " + ifc.id + " " + hop.sentIfc + " " + src + " " + ifc.ifc.pwhe.adr + " " + hop.mac.toEmuStr() + " " + ovrf.id + " " + hop.iface.getMac().toEmuStr() + " " + lp + " " + rp + " " + tun);
            return;
        }
        if (ifc.ifc.type == cfgIfc.ifaceType.tunnel) {
            servP4langNei nei = findNei(ifc.ifc.fwdIf4, new addrIP());
            if (nei == null) {
                nei = findNei(ifc.ifc.fwdIf6, new addrIP());
            }
            if (nei == null) {
                return;
            }
            nei.need++;
            nei.vrf = vrf;
            String prt;
            String par = "";
            if (ifc.ifc.tunMode == null) {
                return;
            }
            switch (ifc.ifc.tunMode) {
                case gre:
                    prt = "gre";
                    break;
                case ipip:
                    prt = "ipip";
                    break;
                case ipsec:
                    prt = "ipsec";
                    if (ifc.ifc.tunIPsec1 != null) {
                        par = getIpsecParam(ifc.ifc.tunIPsec1.espRx, ifc.ifc.tunIPsec1.espTx, ifc.ifc.tunIPsec1.transform);
                    }
                    if (ifc.ifc.tunIPsec2 != null) {
                        par = getIpsecParam(ifc.ifc.tunIPsec2.espRx, ifc.ifc.tunIPsec2.espTx, ifc.ifc.tunIPsec2.transform);
                    }
                    if (par.length() < 1) {
                        return;
                    }
                    break;
                case openvpn:
                    if (ifc.ifc.tunOpenvpn.keyEncr == null) {
                        return;
                    }
                    int lp = ifc.ifc.tunOpenvpn.getLocPort();
                    if (lp < 1) {
                        return;
                    }
                    prt = "openvpn";
                    par = " " + lp + " " + ifc.ifc.tunOpenvpn.getRemPort() + " " + ifc.ifc.tunOpenvpn.timTx + " " + ifc.ifc.tunOpenvpn.cphrSiz + " " + ifc.ifc.tunOpenvpn.hashSiz + " " + ifc.ifc.tunOpenvpn.transform.encr2str() + " " + ifc.ifc.tunOpenvpn.transform.hash2str() + " " + bits.toHex(ifc.ifc.tunOpenvpn.keyEncr) + " " + bits.toHex(ifc.ifc.tunOpenvpn.keyHash);
                    break;
                case wireguard:
                    if (ifc.ifc.tunWireguard.keyTx == null) {
                        return;
                    }
                    lp = ifc.ifc.tunWireguard.getLocPort();
                    if (lp < 1) {
                        return;
                    }
                    prt = "wireguard";
                    par = " " + lp + " " + ifc.ifc.tunWireguard.getRemPort() + " " + ifc.ifc.tunWireguard.idxTx + " " + bits.toHex(ifc.ifc.tunWireguard.keyTx) + " " + bits.toHex(ifc.ifc.tunWireguard.keyRx);
                    break;
                case amt:
                    lp = ifc.ifc.tunAmt.getLocPort();
                    if (lp < 1) {
                        return;
                    }
                    prt = "amt";
                    par = " " + lp + " " + ifc.ifc.tunAmt.getRemPort();
                    break;
                default:
                    return;
            }
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
            servP4langNei hop = findHop(ofwd, ifc.ifc.tunTrg);
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
                if ((hop.mac.compare(hop.mac, nei.mac) == 0) && (nei.sentIfc == hop.sentIfc) && (nei.viaH == hop) && (par.equals(nei.sentIpsec))) {
                    return;
                }
            }
            nei.viaH = hop;
            nei.mac = hop.mac.copyBytes();
            nei.sentIfc = hop.sentIfc;
            nei.sentIpsec = par;
            String afi;
            if (ifc.ifc.tunTrg.isIPv4()) {
                afi = "4";
            } else {
                afi = "6";
            }
            lower.sendLine(prt + afi + "_" + act + " " + nei.id + " " + ifc.id + " " + hop.sentIfc + " " + src + " " + ifc.ifc.tunTrg + " " + hop.mac.toEmuStr() + " " + ovrf.id + " " + hop.iface.getMac().toEmuStr() + par);
            return;
        }
        if (ifc.ifc.type == cfgIfc.ifaceType.dialer) {
            if (ifc.pppoe == null) {
                return;
            }
            servP4langNei nei = findNei(ifc.ifc.fwdIf4, new addrIP());
            if (nei == null) {
                nei = findNei(ifc.ifc.fwdIf6, new addrIP());
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
            nei.viaI = ifc.pppoe;
            lower.sendLine("pppoe_" + act + " " + ifc.id + " " + ifc.pppoe.id + " " + nei.id + " " + vrf.id + " " + sess + " " + mac.toEmuStr() + " " + ifc.pppoe.getMac().toEmuStr());
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
            servP4langNei ntry = new servP4langNei(ifc, new addrIP());
            ntry.mac = new addrMac();
            if (ipi.getL2info(i, ntry.adr, ntry.mac)) {
                break;
            }
            if (!ipi.checkConnected(ntry.adr)) {
                continue;
            }
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
                old.viaI = oif;
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
            lower.sendLine("neigh" + afi + "_" + act + " " + old.id + " " + old.adr + " " + old.mac.toEmuStr() + " " + vrf.id + " " + ifc.getMac().toEmuStr() + " " + old.sentIfc);
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

    private tabListing<tabAceslstN<addrIP>, addrIP> doNatCfg(boolean ipv4, int vrf, tabListing<tabNatCfgN, addrIP> curr, tabListing<tabAceslstN<addrIP>, addrIP> old, tabListing<tabAceslstN<addrIP>, addrIP> res) {
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
        sendAcl(0, "natcfg" + afi + "_del " + vrf + " ", "", "", "", ipv4, false, res, null, null);
        sendAcl(0, "natcfg" + afi + "_add " + vrf + " ", "", "", "", ipv4, false, need, null, res);
        return need;
    }

    private tabListing<tabAceslstN<addrIP>, addrIP> doFlwSpc(boolean ipv4, servP4langVrf vrf, ipFwd fwd, tabListing<tabAceslstN<addrIP>, addrIP> sent) {
        List<tabAceslstN<addrIP>> acl = new ArrayList<tabAceslstN<addrIP>>();
        List<tabQosN> qos = new ArrayList<tabQosN>();
        String afi;
        if (ipv4) {
            afi = "4";
        } else {
            afi = "6";
        }
        for (int i = 0;; i++) {
            if (fwd.flowspec == null) {
                break;
            }
            if (fwd.flowspec.getClass(i + 1) == null) {
                break;
            }
            tabQosN curQ = fwd.flowspec.getClass(i);
            if (curQ == null) {
                break;
            }
            tabListing<tabAceslstN<addrIP>, addrIP> curA = curQ.entry.aclMatch;
            tabAceslstN<addrIP> curE = null;
            if (curA != null) {
                curE = curA.get(0);
            }
            if (curE == null) {
                curE = new tabAceslstN<addrIP>(new addrIP());
                curE.action = tabListingEntry.actionType.actPermit;
            }
            acl.add(curE);
            qos.add(curQ);
        }
        for (int i = acl.size(); i < sent.size(); i++) {
            tabAceslstN<addrIP> old = sent.get(i);
            lower.sendLine("flowspec" + afi + "_del " + vrf.id + " " + i + " 0 0 " + ace2str(i, ipv4, old, false, false));
        }
        tabListing<tabAceslstN<addrIP>, addrIP> res = new tabListing<tabAceslstN<addrIP>, addrIP>();
        for (int i = 0; i < acl.size(); i++) {
            tabQosN curQ = qos.get(i);
            tabAceslstN<addrIP> curE = acl.get(i);
            tabAceslstN<addrIP> old = sent.get(i);
            curE.sequence = i + 1;
            res.add(curE);
            if (old != null) {
                curQ.entry.hwCntr = old.hwCntr;
            }
            if (curE == old) {
                continue;
            }
            String a;
            if (old == null) {
                a = "_add";
            } else {
                a = "_mod";
            }
            lower.sendLine("flowspec" + afi + a + " " + vrf.id + " " + i + " " + curQ.getBytePerInt() + " " + curQ.getInterval() + " " + ace2str(i, ipv4, curE, false, curQ.entry.action == tabListingEntry.actionType.actPermit));
        }
        return res;
    }

    private void doPbrCfg(boolean ipv4, servP4langVrf vrf, tabListing<tabPbrN, addrIP> ned, tabListing<tabPbrN, addrIP> don) {
        int seq = 0;
        for (int i = 0; i < ned.size(); i++) {
            tabPbrN ntry = ned.get(i);
            tabPbrN old = don.get(i);
            if (old == null) {
                old = new tabPbrN();
                old.matcher = new tabListing<tabAceslstN<addrIP>, addrIP>();
            }
            tabPbrN res = new tabPbrN();
            res.matcher = doPbrCfg(seq, ipv4, vrf, ntry, old.matcher, ntry.matcher);
            res.sequence = ntry.sequence;
            don.add(res);
            seq += ntry.matcher.size();
        }
        for (int i = don.size() - 1; i >= 0; i--) {
            tabPbrN ntry = don.get(i);
            if (ned.find(ntry) != null) {
                continue;
            }
            doPbrCfg(seq, ipv4, vrf, null, ntry.matcher, null);
            don.del(ntry);
            seq += ntry.matcher.size();
        }
    }

    private tabListing<tabAceslstN<addrIP>, addrIP> doPbrCfg(int seq, boolean ipv4, servP4langVrf vrf, tabPbrN ntry, tabListing<tabAceslstN<addrIP>, addrIP> old, tabListing<tabAceslstN<addrIP>, addrIP> need) {
        String cmd = "norm";
        String par = "0 0 ";
        if (ntry != null) {
            servP4langVrf tvrf = findVrf(ntry.setVrf);
            if (tvrf == null) {
                return old;
            }
            if (ntry.setHop == null) {
                cmd = "vrf";
                par = tvrf.id + " 0 ";
            } else {
                servP4langNei hop;
                tabRouteEntry<addrIP> rou = null;
                if (ntry.setIfc != null) {
                    hop = findNei(ntry.setIfc, ntry.setHop);
                } else {
                    if (ipv4) {
                        rou = tvrf.vrf.fwd4.actualU.route(ntry.setHop);
                    } else {
                        rou = tvrf.vrf.fwd6.actualU.route(ntry.setHop);
                    }
                    rou = convRou(rou, false);
                    if (rou == null) {
                        return old;
                    }
                    if (rou.best.iface == null) {
                        return old;
                    }
                    addrIP nh = rou.best.nextHop;
                    if (nh == null) {
                        nh = ntry.setHop;
                    }
                    hop = findNei(rou.best.iface, nh);
                }
                if (hop == null) {
                    return old;
                }
                cmd = "hop";
                par = tvrf.id + " " + hop.id + " ";
                int i = -1;
                if (rou != null) {
                    i = getLabel(rou.best.labelRem);
                }
                if (i > 0) {
                    cmd = "lab";
                    par += i + " ";
                }
            }
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
        sendAcl(seq, "pbr" + afi, "norm", "norm", "_del " + vrf.id + " " + par, ipv4, false, old, null, null);
        sendAcl(seq, "pbr" + afi, cmd, "norm", "_add " + vrf.id + " " + par, ipv4, false, need, null, null);
        return need;
    }

    private boolean doMroutes(String afi, int vrf, ipFwdMcast need, ipFwdMcast done, boolean bef) {
        int gid = need.group.getHashW() ^ need.source.getHashW() ^ vrf;
        servP4langIfc ingr = findIfc(need.iface);
        if (ingr == null) {
            return true;
        }
        tabGen<ipFwdIface> nflood = need.flood;
        tabGen<ipFwdIface> dflood = done.flood;
        ipFwdMpmp nlabel = need.label;
        ipFwdMpmp dlabel = done.label;
        ipFwdBier nbier = need.bier;
        ipFwdBier dbier = done.bier;
        if (nlabel == null) {
            nlabel = new ipFwdMpmp(false, new addrIP(), new byte[0]);
        }
        if (dlabel == null) {
            dlabel = new ipFwdMpmp(false, new addrIP(), new byte[0]);
        }
        if (nbier == null) {
            nbier = new ipFwdBier(null, 0);
        }
        if (dbier == null) {
            dbier = new ipFwdBier(null, 0);
        }
        int now = 0;
        if (need.local) {
            nflood = new tabGen<ipFwdIface>();
            nlabel = new ipFwdMpmp(false, new addrIP(), new byte[0]);
            nbier = new ipFwdBier(null, 0);
            now++;
        }
        if (done.local) {
            dflood = new tabGen<ipFwdIface>();
            dlabel = new ipFwdMpmp(false, new addrIP(), new byte[0]);
            dbier = new ipFwdBier(null, 0);
        }
        addrMac mac = need.group.conv2multiMac();
        String act;
        for (int i = 0; i < dbier.fwds.size(); i++) {
            tabLabelBierN ntry = dbier.fwds.get(i);
            if (nbier.fwds.find(ntry) != null) {
                continue;
            }
            servP4langNei hop = findNei(ntry.iface, ntry.hop);
            if (hop == null) {
                continue;
            }
            if (tabLabelBier.bsl2num(ntry.bsl) != 256) {
                continue;
            }
            String a = doLab5(ntry, tabLabelBier.bsl2msk(ntry.bsl), 0);
            servP4langIfc ifc = hop.getVia();
            lower.sendLine("mbierroute" + afi + "_del " + vrf + " " + gid + " " + need.group + " " + need.source + " " + ingr.id + " " + ifc.getMcast(gid, hop).id + " " + hop.id + " " + ntry.label + " " + ifc.id + " " + dbier.srcId + " 0" + a);
        }
        for (int i = 0; i < nbier.fwds.size(); i++) {
            tabLabelBierN ntry = nbier.fwds.get(i);
            if (tabLabelBier.bsl2num(ntry.bsl) != 256) {
                continue;
            }
            if (dbier.fwds.find(ntry) != null) {
                act = "mod";
            } else {
                act = "add";
            }
            servP4langNei hop = findNei(ntry.iface, ntry.hop);
            if (hop == null) {
                continue;
            }
            String a = doLab5(ntry, tabLabelBier.bsl2msk(ntry.bsl), 0);
            servP4langIfc ifc = hop.getVia();
            lower.sendLine("mbierroute" + afi + "_" + act + " " + vrf + " " + gid + " " + need.group + " " + need.source + " " + ingr.id + " " + ifc.getMcast(gid, hop).id + " " + hop.id + " " + ntry.label + " " + ifc.id + " " + nbier.srcId + " 0" + a);
            now++;
        }
        for (int i = 0; i < dlabel.neighs.size(); i++) {
            ipFwdMpNe ntry = dlabel.neighs.get(i);
            if (ntry.labelR < 0) {
                continue;
            }
            if (nlabel.neighs.find(ntry) != null) {
                continue;
            }
            servP4langNei hop = findNei(ntry.iface, ntry.addr);
            if (hop == null) {
                continue;
            }
            servP4langIfc ifc = hop.getVia();
            lower.sendLine("mlabroute" + afi + "_del " + vrf + " " + gid + " " + need.group + " " + need.source + " " + ingr.id + " " + ifc.getMcast(gid, hop).id + " " + hop.id + " " + ntry.labelR + " " + ifc.id);
        }
        for (int i = 0; i < nlabel.neighs.size(); i++) {
            ipFwdMpNe ntry = nlabel.neighs.get(i);
            if (ntry.labelR < 0) {
                continue;
            }
            if (dlabel.neighs.find(ntry) != null) {
                act = "mod";
            } else {
                act = "add";
            }
            servP4langNei hop = findNei(ntry.iface, ntry.addr);
            if (hop == null) {
                continue;
            }
            servP4langIfc ifc = hop.getVia();
            lower.sendLine("mlabroute" + afi + "_" + act + " " + vrf + " " + gid + " " + need.group + " " + need.source + " " + ingr.id + " " + ifc.getMcast(gid, hop).id + " " + hop.id + " " + ntry.labelR + " " + ifc.id);
            now++;
        }
        for (int i = 0; i < dflood.size(); i++) {
            ipFwdIface ntry = dflood.get(i);
            if (nflood.find(ntry) != null) {
                continue;
            }
            servP4langIfc ifc = findIfc(ntry);
            if (ifc == null) {
                continue;
            }
            lower.sendLine("mroute" + afi + "_del " + vrf + " " + gid + " " + need.group + " " + need.source + " " + ingr.id + " " + ifc.getMcast(gid, null).id + " " + ifc.id + " " + ifc.getMac().toEmuStr() + " " + mac.toEmuStr());
        }
        for (int i = 0; i < nflood.size(); i++) {
            ipFwdIface ntry = nflood.get(i);
            servP4langIfc ifc = findIfc(ntry);
            if (ifc == null) {
                continue;
            }
            if (dflood.find(ntry) != null) {
                act = "mod";
            } else {
                act = "add";
            }
            lower.sendLine("mroute" + afi + "_" + act + " " + vrf + " " + gid + " " + need.group + " " + need.source + " " + ingr.id + " " + ifc.getMcast(gid, null).id + " " + ifc.id + " " + ifc.getMac().toEmuStr() + " " + mac.toEmuStr());
            now++;
        }
        if (bef) {
            act = "mod";
            if (now < 1) {
                act = "del";
            }
        } else {
            act = "add";
        }
        lower.sendLine("mlocal" + afi + "_" + (need.local ? "add " : "del ") + vrf + " " + gid + " " + need.group + " " + need.source + " " + ingr.id + " " + act);
        return false;
    }

    private void doMroutes(boolean ipv4, int vrf, tabGen<ipFwdMcast> need, tabGen<ipFwdMcast> done) {
        String afi;
        if (ipv4) {
            afi = "4";
        } else {
            afi = "6";
        }
        for (int i = 0; i < need.size(); i++) {
            ipFwdMcast ntry = need.get(i);
            ntry = ntry.copyBytes();
            ipFwdMcast old = done.find(ntry);
            boolean bef;
            if (old != null) {
                if (!ntry.differs(old)) {
                    continue;
                }
                bef = true;
            } else {
                old = new ipFwdMcast(ntry.group, ntry.source);
                bef = false;
            }
            if (doMroutes(afi, vrf, ntry, old, bef)) {
                continue;
            }
            done.put(ntry);
        }
        for (int i = done.size() - 1; i >= 0; i--) {
            ipFwdMcast ntry = done.get(i);
            if (need.find(ntry) != null) {
                continue;
            }
            ipFwdMcast empty = new ipFwdMcast(ntry.group, ntry.source);
            empty.iface = ntry.iface;
            if (doMroutes(afi, vrf, empty, ntry, true)) {
                continue;
            }
            done.del(ntry);
        }
    }

    private void doSockets(boolean ipv4, int vrf, int prt, tabConnect<addrIP, prtGenServ> need, tabConnect<addrIP, prtGenServ> done) {
        String afi;
        if (ipv4) {
            afi = "4";
        } else {
            afi = "6";
        }
        for (int i = 0; i < done.size(); i++) {
            tabConnectEntry<addrIP, prtGenServ> ntry = done.read(i);
            prtGenServ old = need.get(ntry.iface, null, ntry.local, ntry.remote);
            if (old != null) {
                continue;
            }
            done.del(ntry.iface, null, ntry.local, ntry.remote);
            servP4langIfc fif = findIfc(ntry.iface);
            String sif = "-1";
            if (fif != null) {
                sif = "" + fif.id;
            }
            lower.sendLine("socket" + afi + "_del " + vrf + " " + prt + " " + sif + " " + ntry.local + " " + ntry.remote);
        }
        for (int i = 0; i < need.size(); i++) {
            tabConnectEntry<addrIP, prtGenServ> ntry = need.read(i);
            prtGenServ old = done.get(ntry.iface, null, ntry.local, ntry.remote);
            if (old != null) {
                continue;
            }
            done.add(ntry.iface, null, ntry.local, ntry.remote, new prtGenServ(), "save");
            servP4langIfc fif = findIfc(ntry.iface);
            String sif = "-1";
            if (fif != null) {
                sif = "" + fif.id;
            }
            lower.sendLine("socket" + afi + "_add " + vrf + " " + prt + " " + sif + " " + ntry.local + " " + ntry.remote);
        }
    }

    private void doIndexes(boolean ipv4, int vrf, tabGen<tabIndex<addrIP>> need, tabGen<tabIndex<addrIP>> done, tabRoute<addrIP> routes, tabGen<servP4langStr<tabIndex<addrIP>>> store) {
        for (int i = 0; i < need.size(); i++) {
            tabIndex<addrIP> ntry = need.get(i);
            ntry = ntry.copyBytes();
            tabRouteEntry<addrIP> rou = routes.find(ntry.prefix);
            if (rou == null) {
                continue;
            }
            if (rou.best.nextHop == null) {
                continue;
            }
            servP4langNei hop = findNei(rou.best.iface, rou.best.nextHop);
            if (hop == null) {
                continue;
            }
            servP4langStr<tabIndex<addrIP>> str = new servP4langStr<tabIndex<addrIP>>(ntry);
            str.store = hop.id;
            tabIndex<addrIP> old = done.find(ntry);
            String act = "add";
            if (old != null) {
                if (!ntry.differs(old) && !str.differs(store)) {
                    continue;
                }
                act = "mod";
            }
            done.put(ntry);
            store.put(str);
            lower.sendLine("polkaidx_" + act + " " + ntry.index + " " + vrf + " " + hop.id);
        }
        for (int i = done.size() - 1; i >= 0; i--) {
            tabIndex<addrIP> ntry = done.get(i);
            if (need.find(ntry) != null) {
                continue;
            }
            done.del(ntry);
            store.del(new servP4langStr<tabIndex<addrIP>>(ntry));
            lower.sendLine("polkaidx_del " + ntry.index + " " + vrf + " 0");
        }
    }

    private void doRoutes(boolean ipv4, int vrf, tabRoute<addrIP> need, tabRoute<addrIP> done, tabGen<servP4langStr<tabRouteEntry<addrIP>>> store) {
        String afi;
        if (ipv4) {
            afi = "4";
        } else {
            afi = "6";
        }
        for (int i = 0; i < need.size(); i++) {
            tabRouteEntry<addrIP> ntry = need.get(i);
            ntry = ntry.copyBytes(tabRoute.addType.notyet);
            tabRouteEntry<addrIP> old = done.find(ntry);
            if ((ntry.best.iface == null) && (ntry.best.rouTab != null)) {
                tabRouteEntry<addrIP> recur;
                if (ntry.best.segrouPrf == null) {
                    recur = ntry.best.rouTab.actualU.route(ntry.best.nextHop);
                } else {
                    recur = ntry.best.rouTab.actualU.route(ntry.best.segrouPrf);
                }
                recur = convRou(recur, true);
                if (recur == null) {
                    continue;
                }
                servP4langNei hop = findNei(recur.best.iface, recur.best.nextHop);
                if (hop == null) {
                    continue;
                }
                servP4langStr<tabRouteEntry<addrIP>> str = new servP4langStr<tabRouteEntry<addrIP>>(ntry);
                str.store = hop.id;
                String act = "add";
                if (old != null) {
                    if (!ntry.differs(tabRoute.addType.notyet, old) && !str.differs(store)) {
                        continue;
                    }
                    act = "mod";
                }
                done.add(tabRoute.addType.always, ntry, true, true);
                store.put(str);
                String a;
                if (ipv4) {
                    a = "" + addrPrefix.ip2ip4(ntry.prefix);
                } else {
                    a = "" + addrPrefix.ip2ip6(ntry.prefix);
                }
                if (ntry.best.segrouPrf == null) {
                    lower.sendLine("vpnroute" + afi + "_" + act + " " + a + " " + hop.id + " " + ntry.best.nextHop + " " + vrf + " " + getLabel(recur) + " " + getLabel(ntry));
                } else {
                    lower.sendLine("srvroute" + afi + "_" + act + " " + a + " " + hop.id + " " + ntry.best.nextHop + " " + vrf + " " + ntry.best.segrouPrf);
                }
                continue;
            }
            String act = "add";
            if (old != null) {
                if (ntry.best.nextHop != null) {
                    findNei(ntry.best.iface, ntry.best.nextHop);
                }
                if (!ntry.differs(tabRoute.addType.notyet, old)) {
                    continue;
                }
                act = "mod";
            }
            done.add(tabRoute.addType.always, ntry, true, true);
            old = convRou(ntry, true);
            if (old == null) {
                done.del(ntry);
                continue;
            }
            ntry = old;
            String a;
            if (ipv4) {
                a = "" + addrPrefix.ip2ip4(ntry.prefix);
            } else {
                a = "" + addrPrefix.ip2ip6(ntry.prefix);
            }
            if (ntry.best.nextHop == null) {
                servP4langIfc fif = findIfc(ntry.best.iface);
                String sif = "-1";
                if (fif != null) {
                    sif = "" + fif.id;
                }
                lower.sendLine("myaddr" + afi + "_" + act + " " + a + " " + sif + " " + vrf);
                continue;
            }
            servP4langNei hop = findNei(ntry.best.iface, ntry.best.nextHop);
            if (hop == null) {
                done.del(ntry);
                continue;
            }
            if (ntry.best.attribAs == ifcPolka.type) {
                lower.sendLine("polroute" + afi + "_" + act + " " + a + " " + hop.id + " " + ntry.best.nextHop + " " + vrf + " " + bits.toHex(ntry.best.attribVal));
                continue;
            }
            if (ntry.best.labelRem != null) {
                lower.sendLine("labroute" + afi + "_" + act + " " + a + " " + hop.id + " " + ntry.best.nextHop + " " + vrf + " " + getLabel(ntry));
                continue;
            }
            lower.sendLine("route" + afi + "_" + act + " " + a + " " + hop.id + " " + ntry.best.nextHop + " " + vrf);
        }
        for (int i = done.size() - 1; i >= 0; i--) {
            tabRouteEntry<addrIP> ntry = done.get(i);
            if (need.find(ntry) != null) {
                continue;
            }
            if ((ntry.best.iface == null) && (ntry.best.rouTab != null)) {
                tabRouteEntry<addrIP> old;
                if (ntry.best.segrouPrf == null) {
                    old = ntry.best.rouTab.actualU.route(ntry.best.nextHop);
                } else {
                    old = ntry.best.rouTab.actualU.route(ntry.best.segrouPrf);
                }
                old = convRou(old, true);
                if (old == null) {
                    continue;
                }
                servP4langNei hop = findNei(old.best.iface, old.best.nextHop);
                if (hop == null) {
                    continue;
                }
                done.del(ntry);
                store.del(new servP4langStr<tabRouteEntry<addrIP>>(ntry));
                String a;
                if (ipv4) {
                    a = "" + addrPrefix.ip2ip4(ntry.prefix);
                } else {
                    a = "" + addrPrefix.ip2ip6(ntry.prefix);
                }
                if (ntry.best.segrouPrf == null) {
                    lower.sendLine("vpnroute" + afi + "_del " + a + " " + hop.id + " " + ntry.best.nextHop + " " + vrf + " " + getLabel(old) + " " + getLabel(ntry));
                } else {
                    lower.sendLine("srvroute" + afi + "_del " + a + " " + hop.id + " " + ntry.best.nextHop + " " + vrf + " " + ntry.best.segrouPrf);
                }
                continue;
            }
            done.del(ntry);
            ntry = convRou(ntry, true);
            if (ntry == null) {
                continue;
            }
            String a;
            if (ipv4) {
                a = "" + addrPrefix.ip2ip4(ntry.prefix);
            } else {
                a = "" + addrPrefix.ip2ip6(ntry.prefix);
            }
            if (ntry.best.nextHop == null) {
                servP4langIfc fif = findIfc(ntry.best.iface);
                String sif = "-1";
                if (fif != null) {
                    sif = "" + fif.id;
                }
                lower.sendLine("myaddr" + afi + "_del " + a + " " + sif + " " + vrf);
                continue;
            }
            servP4langNei hop = findNei(ntry.best.iface, ntry.best.nextHop);
            if (hop == null) {
                continue;
            }
            if (ntry.best.labelRem != null) {
                lower.sendLine("labroute" + afi + "_del " + a + " " + hop.id + " " + ntry.best.nextHop + " " + vrf + " " + getLabel(ntry));
                continue;
            }
            lower.sendLine("route" + afi + "_del " + a + " " + hop.id + " " + ntry.best.nextHop + " " + vrf);
        }
    }

    public String numat2str(tabIntMatcher mat, int max) {
        if (mat.action == tabIntMatcher.actionType.xact) {
            return mat.rangeMin + " " + max;
        }
        if (mat.action != tabIntMatcher.actionType.range) {
            return "0 0";
        }
        if (mat.rangeMin == mat.rangeMax) {
            return mat.rangeMin + " " + max;
        }
        return mat.rangeMin + " " + (max - mat.rangeMax + mat.rangeMin);
    }

    public String ip2str(boolean ipv4, addrIP adr) {
        if (ipv4) {
            return "" + adr.toIPv4();
        } else {
            return "" + adr.toIPv6();
        }
    }

    public String ace2str(int seq, boolean ipv4, tabAceslstN<addrIP> ace, boolean check, boolean negate) {
        if (check) {
            if (!ace.srcMask.isFilled(0)) {
                if (ace.srcMask.isIPv4() != ipv4) {
                    return null;
                }
            }
            if (!ace.trgMask.isFilled(0)) {
                if (ace.trgMask.isIPv4() != ipv4) {
                    return null;
                }
            }
        }
        String cmd;
        if (negate ^ (ace.action == tabListingEntry.actionType.actPermit)) {
            cmd = tabListingEntry.action2string(tabListingEntry.actionType.actPermit);
        } else {
            cmd = tabListingEntry.action2string(tabListingEntry.actionType.actDeny);
        }
        return seq + " " + cmd + " " + numat2str(ace.proto, 255) + " " + ip2str(ipv4, ace.srcAddr) + " " + ip2str(ipv4, ace.srcMask) + " " + ip2str(ipv4, ace.trgAddr) + " " + ip2str(ipv4, ace.trgMask) + " " + numat2str(ace.srcPort, 65535) + " " + numat2str(ace.trgPort, 65535) + " " + numat2str(ace.tos, 255) + " " + numat2str(ace.flow, ipv4 ? 65535 : 1048575);
    }

    public void sendAcl(int seq, String pre1, String perm, String deny, String pre2, boolean ipv4, boolean check, tabListing<tabAceslstN<addrIP>, addrIP> iface, tabListing<tabAceslstN<addrIP>, addrIP> infra, tabListing<tabAceslstN<addrIP>, addrIP> res) {
        if (res == null) {
            res = new tabListing<tabAceslstN<addrIP>, addrIP>();
        }
        res.clear();
        if (iface != null) {
            iface = tabAceslstN.unrollAcl(iface);
        }
        if (infra != null) {
            infra = tabAceslstN.unrollAcl(infra);
        }
        res.mergeTwo(infra, iface);
        for (int i = 0; i < res.size(); i++) {
            tabAceslstN<addrIP> ace = res.get(i);
            String a = ace2str(seq + i, ipv4, ace, check, false);
            if (a == null) {
                continue;
            }
            lower.sendLine(pre1 + (ace.action == tabListingEntry.actionType.actPermit ? perm : deny) + pre2 + a);
        }
    }

}
