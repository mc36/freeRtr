package net.freertr.clnt;

import java.util.Comparator;
import net.freertr.addr.addrEmpty;
import net.freertr.addr.addrIP;
import net.freertr.addr.addrType;
import net.freertr.cfg.cfgIfc;
import net.freertr.ifc.ifcDn;
import net.freertr.ifc.ifcNull;
import net.freertr.ifc.ifcUp;
import net.freertr.ip.ipFwd;
import net.freertr.ip.ipFwdIface;
import net.freertr.pack.packHolder;
import net.freertr.pack.packL2tp2;
import net.freertr.prt.prtGenConn;
import net.freertr.prt.prtServP;
import net.freertr.user.userFormat;
import net.freertr.util.bits;
import net.freertr.util.cmds;
import net.freertr.util.counter;
import net.freertr.util.debugger;
import net.freertr.util.logger;
import net.freertr.util.state;

/**
 * sdwan connection
 *
 * @author matecsaba
 */
public class clntSdwanConn implements ifcDn, prtServP, Comparator<clntSdwanConn> {

    /**
     * parent
     */
    public final clntSdwan lower;

    /**
     * ip version
     */
    public int ver;

    /**
     * remote address
     */
    public addrIP addr;

    /**
     * remote port
     */
    public int port;

    /**
     * neighbor id
     */
    public int num;

    /**
     * inner ipv4
     */
    public addrIP peer4;

    /**
     * inner ipv6
     */
    public addrIP peer6;

    /**
     * peer name
     */
    public String name;

    private cfgIfc ifc;

    private ifcUp upper = new ifcNull();

    private counter cntr = new counter();

    private prtGenConn conn;

    private boolean noMacsec;

    private boolean noSgt;

    /**
     * create instance
     *
     * @param parent lower layer
     */
    public clntSdwanConn(clntSdwan parent) {
        lower = parent;
    }

    public String toString() {
        return "sdwan to " + addr;
    }

    public int compare(clntSdwanConn o1, clntSdwanConn o2) {
        if (o1.port < o2.port) {
            return -1;
        }
        if (o1.port > o2.port) {
            return +1;
        }
        return o1.addr.compare(o1.addr, o2.addr);
    }

    /**
     * read string
     *
     * @param cmd string
     */
    public void fromString(cmds cmd) {
        ver = bits.str2num(cmd.word());
        addr = new addrIP();
        addr.fromString(cmd.word());
        port = bits.str2num(cmd.word());
        num = bits.str2num(cmd.word());
        peer4 = new addrIP();
        peer4.fromString(cmd.word());
        peer6 = new addrIP();
        peer6.fromString(cmd.word());
        name = cmd.word();
        for (;;) {
            String a = cmd.word();
            if (a.length() < 1) {
                break;
            }
            if (a.equals("nomacsec")) {
                noMacsec = true;
            }
            if (a.equals("nosgt")) {
                noSgt = true;
            }
        }
    }

    /**
     * stop connection
     */
    public void workStop() {
        if (debugger.clntSdwanTraf) {
            logger.debug("stopping peer " + addr + " " + port);
        }
        if (ifc != null) {
            ifc.cloneStop();
        }
        if (conn != null) {
            conn.setClosing();
        }
        ifc = null;
        conn = null;
    }

    /**
     * stop connection
     */
    public void workStart() {
        if (debugger.clntSdwanTraf) {
            logger.debug("starting peer " + addr + " " + port);
        }
        conn = lower.udpCor.packetConnect(this, lower.fwdIfc, lower.dataPort, addr, port, "sdwan", null, -1);
        if (conn == null) {
            logger.error("failed to connect " + addr + " " + port);
            return;
        }
        conn.timeout = 120000;
        conn.sendFLW = lower.sendingFLW;
        conn.sendTOS = lower.sendingTOS;
        conn.sendTTL = lower.sendingTTL;
        upper = new ifcNull();
        ifc = lower.clonIfc.cloneStart(this);
        ifc.addr4changed(lower.myAddr4, ifc.mask4, peer4.toIPv4());
        ifc.addr6changed(lower.myAddr6, ifc.mask6, peer6.toIPv6());
        if (noMacsec) {
            ifc.disableMacsec = true;
            ifc.ethtyp.macSec = null;
            ifc.ethtyp.timerUpdate();
        }
        if (noSgt) {
            ifc.disableSgt = true;
            ifc.ethtyp.sgtHnd = null;
        }
    }

    /**
     * get show
     *
     * @param l list to update
     */
    protected void getShow(userFormat l) {
        l.add(name + "|" + addr + "|" + port + "|" + num + "|" + ifc.name + "|" + peer4 + "|" + peer6);
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
        return 1400;
    }

    public long getBandwidth() {
        return 8000000;
    }

    public void sendPack(packHolder pck) {
        packL2tp2 tx = new packL2tp2();
        tx.ctrl = false;
        tx.sesID = num;
        tx.tunID = lower.myNum;
        tx.createHeader(pck);
        cntr.tx(pck);
        pck.putDefaults();
        conn.send2net(pck);
    }

    public boolean datagramRecv(prtGenConn id, packHolder pck) {
        packL2tp2 rx = new packL2tp2();
        if (rx.parseHeader(pck)) {
            cntr.drop(pck, counter.reasons.badHdr);
            return false;
        }
        if (rx.tunID != num) {
            cntr.drop(pck, counter.reasons.badID);
            return false;
        }
        if (rx.sesID != lower.myNum) {
            cntr.drop(pck, counter.reasons.badID);
            return false;
        }
        if (rx.ctrl) {
            cntr.drop(pck, counter.reasons.badHdr);
            return false;
        }
        cntr.rx(pck);
        upper.recvPack(pck);
        return false;
    }

    public void closedInterface(ipFwdIface ifc) {
    }

    public boolean datagramAccept(prtGenConn id) {
        return true;
    }

    public void datagramReady(prtGenConn id) {
    }

    public void datagramClosed(prtGenConn id) {
    }

    public void datagramWork(prtGenConn id) {
    }

    public boolean datagramError(prtGenConn id, packHolder pck, addrIP rtr, counter.reasons err, int lab) {
        return false;
    }

    public boolean datagramState(prtGenConn id, state.states stat) {
        return false;
    }

    /**
     * get forwarder
     *
     * @return forwarder used
     */
    public ipFwd getFwder() {
        return lower.fwdCor;
    }

    /**
     * get local address
     *
     * @return address
     */
    public addrIP getAddrLoc() {
        return lower.fwdIfc.addr;
    }

    /**
     * get remote address
     *
     * @return address
     */
    public addrIP getAddrRem() {
        return addr.copyBytes();
    }

    /**
     * get local port number
     *
     * @return session id, 0 if no session
     */
    public int getPortLoc() {
        return lower.dataPort;
    }

    /**
     * get remote port number
     *
     * @return session id, 0 if no session
     */
    public int getPortRem() {
        return port;
    }

    /**
     * get remote session id
     *
     * @return session id, 0 if no session
     */
    public int getSessRem() {
        return num;
    }

    /**
     * get remote tunn id
     *
     * @return session id, 0 if no tunnel
     */
    public int getTunnRem() {
        return lower.myNum;
    }

}
