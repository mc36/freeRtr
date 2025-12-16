package org.freertr.serv;

import java.util.List;
import org.freertr.cfg.cfgAll;
import org.freertr.cfg.cfgBrdg;
import org.freertr.cfg.cfgIfc;
import org.freertr.cfg.cfgVrf;
import org.freertr.ifc.ifcBridgeIfc;
import org.freertr.ifc.ifcEthTyp;
import org.freertr.ifc.ifcNull;
import org.freertr.pack.packHolder;
import org.freertr.pack.packOpenflow;
import org.freertr.pipe.pipeLine;
import org.freertr.pipe.pipeSide;
import org.freertr.prt.prtGenConn;
import org.freertr.prt.prtServS;
import org.freertr.tab.tabGen;
import org.freertr.user.userFilter;
import org.freertr.user.userHelp;
import org.freertr.util.bits;
import org.freertr.util.cmds;
import org.freertr.util.counter;
import org.freertr.util.debugger;
import org.freertr.util.history;
import org.freertr.util.logger;
import org.freertr.util.notifier;
import org.freertr.tab.tabRouteIface;

/**
 * openflow server
 *
 * @author matecsaba
 */
public class servOpenflow extends servGeneric implements prtServS, servGenFwdr {

    /**
     * create instance
     */
    public servOpenflow() {
        parent = new servStack();
        parid = new servStackFwd(parent);
    }

    private servOpenflowRx thrdRx;

    private servOpenflowTx thrdTx;

    /**
     * connection
     */
    protected pipeSide conn;

    /**
     * connection start
     */
    protected long started = 0;

    /**
     * connections accepted
     */
    protected int reconns = 0;

    /**
     * current stack
     */
    protected servStack parent;

    /**
     * current stack id
     */
    protected servStackFwd parid;

    /**
     * tx notifier
     */
    protected notifier notif = new notifier();

    /**
     * tx id
     */
    protected int xid = 1;

    /**
     * counter
     */
    protected counter cntr = new counter();

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
    public final static int tabGrp = -1;

    /**
     * port table
     */
    public final static int tabPort = 0;

    /**
     * mpls table
     */
    public final static int tabMpls = 1;

    /**
     * ipv4 table
     */
    public final static int tabIpv4 = 2;

    /**
     * ipv6 table
     */
    public final static int tabIpv6 = 3;

    /**
     * exported interfaces
     */
    public tabGen<servOpenflowIfc1> expIfc = new tabGen<servOpenflowIfc1>();

    /**
     * defaults text
     */
    public final static userFilter[] defaultF = {
        new userFilter("server openflow .*", cmds.tabulator + "port " + packOpenflow.port, null),
        new userFilter("server openflow .*", cmds.tabulator + "protocol " + proto2string(protoAllStrm), null),
        new userFilter("server openflow .*", cmds.tabulator + "version 4", null)
    };

    public userFilter[] srvDefFlt() {
        return defaultF;
    }

    public boolean srvAccept(pipeSide pipe, prtGenConn id) {
        started = bits.getTime();
        reconns++;
        if (conn != null) {
            thrdRx.working = false;
            thrdTx.working = false;
            conn.setClose();
            notif.wakeup();
        }
        pipe.setTime(120000);
        conn = pipe;
        thrdRx = new servOpenflowRx(conn, this);
        thrdTx = new servOpenflowTx(conn, this);
        logger.warn("neighbor " + id.peerAddr + " up");
        return false;
    }

    public void srvShRun(String beg, List<String> l, int filter) {
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
            cfgIfc ifc = cfgAll.ifcFind(cmd.word(), 0);
            if (ifc == null) {
                cmd.error("no such interface");
                return false;
            }
            if ((ifc.type != tabRouteIface.ifaceType.sdn) && (ifc.type != tabRouteIface.ifaceType.bridge)) {
                cmd.error("not sdn interface");
                return false;
            }
            servOpenflowIfc1 ntry = new servOpenflowIfc1();
            ntry.id = bits.str2num(cmd.word());
            ntry.ifc = ifc;
            ntry.lower = this;
            if (ifc.type == tabRouteIface.ifaceType.bridge) {
                ntry.id = tabGrp;
                ntry.grp = ifc.bridgeHed.number;
            }
            expIfc.put(ntry);
            if (conn != null) {
                ntry.sendState(0);
            }
            if (ntry.id != tabGrp) {
                ntry.setUpper(ifc.ethtyp);
            }
            ntry.ifc.ethtyp.hwHstry = new history();
            ntry.ifc.ethtyp.hwCntr = new counter();
            notif.wakeup();
            return false;
        }
        if (!s.equals(cmds.negated)) {
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
            cfgIfc ifc = cfgAll.ifcFind(cmd.word(), 0);
            if (ifc == null) {
                cmd.error("no such interface");
                return false;
            }
            servOpenflowIfc1 ntry = new servOpenflowIfc1();
            ntry.id = bits.str2num(cmd.word());
            ntry.ifc = ifc;
            if (ifc.type == tabRouteIface.ifaceType.bridge) {
                ntry.id = tabGrp;
                ntry.grp = ifc.bridgeHed.number;
            }
            ntry = expIfc.del(ntry);
            clearIface(ntry);
            notif.wakeup();
            return false;
        }
        return true;
    }

    public void srvHelp(userHelp l) {
        l.add(null, false, 1, new int[]{2}, "version", "openflow version");
        l.add(null, false, 2, new int[]{-1}, "<num>", "version number");
        l.add(null, false, 1, new int[]{2}, "export-vrf", "specify vrf to export");
        l.add(null, false, 2, new int[]{-1}, "<name:vrf>", "vrf name");
        l.add(null, false, 1, new int[]{2}, "export-port", "specify port to export");
        l.add(null, false, 2, new int[]{3}, "<name:ifc>", "interface name");
        l.add(null, false, 3, new int[]{-1}, "<num>", "openflow port number");
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
        if (debugger.servOpenflowTx) {
            logger.debug("tx " + pckO.dump(pckB));
        }
        cntr.tx(pckB);
        pckO.xid = xid++;
        pckO.version = version;
        pckO.sendPack(pckB);
    }

    /**
     * get hardware forwarder
     *
     * @return offload info
     */
    public String getShGenOneLiner() {
        String a = "n/a";
        if (expVrf != null) {
            a = expVrf.name;
        }
        a = "opnflw,vrf=" + a + ",prt=" + expIfc.size() + ",rec=" + reconns;
        if (conn == null) {
            return a + ",disc";
        }
        return a + ",cls=" + conn.isClosed() + ",rdy=" + conn.isReady();
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
        servOpenflowIfc1 ifcc = new servOpenflowIfc1();
        ifcc.id = ifcn;
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
        for (int i = 0; i < cntr; i++) {
            ifcc.sendPack(pck.copyBytes(true, true));
        }
        return false;
    }

    /**
     * find interface
     *
     * @param ifc interface
     * @return interface, null if error
     */
    protected servOpenflowIfc1 findIfc(ifcEthTyp ifc) {
        if (ifc == null) {
            return null;
        }
        for (int i = 0; i < expIfc.size(); i++) {
            servOpenflowIfc1 ntry = expIfc.get(i);
            if (ntry == null) {
                continue;
            }
            if (ntry.ifc.ethtyp == ifc) {
                return ntry;
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
    protected servOpenflowIfc1 findIfc(tabRouteIface ifc) {
        if (ifc == null) {
            return null;
        }
        for (int i = 0; i < expIfc.size(); i++) {
            servOpenflowIfc1 ntry = expIfc.get(i);
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

    /**
     * find interface
     *
     * @param ifc interface
     * @return interface, null if error
     */
    protected servOpenflowIfc1 findIfc(cfgBrdg ifc) {
        for (int i = 0; i < expIfc.size(); i++) {
            servOpenflowIfc1 old = expIfc.get(i);
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
    protected servOpenflowIfc1 findIfc(ifcBridgeIfc ifc) {
        if (ifc == null) {
            return null;
        }
        for (int i = 0; i < expIfc.size(); i++) {
            servOpenflowIfc1 ntry = expIfc.get(i);
            if (ntry.ifc == null) {
                continue;
            }
            if (ifc == ntry.ifc.bridgeIfc) {
                return ntry;
            }
        }
        return null;
    }

}
