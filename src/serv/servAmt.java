package serv;

import addr.addrIP;
import addr.addrIPv4;
import addr.addrIPv6;
import addr.addrMac;
import addr.addrType;
import cfg.cfgAll;
import cfg.cfgIfc;
import clnt.clntAmt;
import ifc.ifcDn;
import ifc.ifcEther;
import ifc.ifcNull;
import ifc.ifcUp;
import ip.ipIcmp6;
import ip.ipMhost4;
import java.util.Comparator;
import java.util.List;
import pack.packHolder;
import pipe.pipeSide;
import prt.prtGenConn;
import prt.prtServP;
import tab.tabGen;
import user.userFilter;
import user.userHelping;
import util.bits;
import util.cmds;
import util.counter;
import util.state;

/**
 * automatic multicast tunneling (rfc7450) protocol server
 *
 * @author matecsaba
 */
public class servAmt extends servGeneric implements prtServP {

    /**
     * interface to use
     */
    public cfgIfc tempIfc;

    /**
     * timeout
     */
    public int timeout = 60000;

    /**
     * list of connections
     */
    public tabGen<servAmtConn> conns = new tabGen<servAmtConn>();

    /**
     * defaults text
     */
    public final static String[] defaultL = {
        "server amt .*! port " + clntAmt.portNum,
        "server amt .*! protocol " + proto2string(protoAllDgrm),
        "server amt .*! timeout 60000"
    };

    /**
     * defaults filter
     */
    public static tabGen<userFilter> defaultF;

    public tabGen<userFilter> srvDefFlt() {
        return defaultF;
    }

    /**
     * find one connection
     *
     * @param id connection id
     * @param create set true to create if none found
     * @return connection entry
     */
    public servAmtConn connFind(prtGenConn id, boolean create) {
        servAmtConn ntry = new servAmtConn(id, this);
        if (!create) {
            return conns.find(ntry);
        }
        if (tempIfc == null) {
            return null;
        }
        servAmtConn old = conns.add(ntry);
        if (old != null) {
            return old;
        }
        ntry.doStartup();
        return ntry;
    }

    /**
     * delete one connection
     *
     * @param id connection id
     * @return connection entry
     */
    public servAmtConn connDel(prtGenConn id) {
        servAmtConn ntry = new servAmtConn(id, this);
        return conns.del(ntry);
    }

    public void srvShRun(String beg, List<String> l, int filter) {
        if (tempIfc == null) {
            l.add(beg + "no clone");
        } else {
            l.add(beg + "clone " + tempIfc.name);
        }
        l.add(beg + "timeout " + timeout);
    }

    public boolean srvCfgStr(cmds cmd) {
        String s = cmd.word();
        if (s.equals("timeout")) {
            timeout = bits.str2num(cmd.word());
            return false;
        }
        if (s.equals("clone")) {
            tempIfc = cfgAll.ifcFind(cmd.word(), false);
            if (tempIfc == null) {
                cmd.error("no such interface");
                return false;
            }
            if (tempIfc.type != cfgIfc.ifaceType.template) {
                cmd.error("not template interface");
                tempIfc = null;
                return false;
            }
            return false;
        }
        if (!s.equals("no")) {
            return true;
        }
        s = cmd.word();
        if (s.equals("clone")) {
            tempIfc = null;
            return false;
        }
        return true;
    }

    public void srvHelp(userHelping l) {
        l.add("1 2  clone                        set interface to clone");
        l.add("2 .    <name>                     name of interface");
        l.add("1 2  timeout                      timeout of client");
        l.add("2 .    <num>                      milliseconds");
    }

    public String srvName() {
        return "amt";
    }

    public int srvPort() {
        return clntAmt.portNum;
    }

    public int srvProto() {
        return protoAllDgrm;
    }

    public boolean srvInit() {
        return genDgrmStart(this, 0);
    }

    public boolean srvDeinit() {
        return genericStop(0);
    }

    public boolean srvAccept(pipeSide pipe, prtGenConn id) {
        id.timeout = timeout;
        connFind(id, true);
        return false;
    }

    /**
     * connection ready
     *
     * @param id connection
     */
    public void datagramReady(prtGenConn id) {
    }

    /**
     * stop connection
     *
     * @param id connection
     */
    public void datagramClosed(prtGenConn id) {
        servAmtConn ntry = connDel(id);
        if (ntry == null) {
            return;
        }
        ntry.closeDn();
    }

    /**
     * work connection
     *
     * @param id connection
     */
    public void datagramWork(prtGenConn id) {
        servAmtConn ntry = connFind(id, false);
        if (ntry == null) {
            id.setClosing();
            return;
        }
    }

    /**
     * received error
     *
     * @param id connection
     * @param pck packet
     * @param rtr reporting router
     * @param err error happened
     * @param lab error label
     * @return false on success, true on error
     */
    public boolean datagramError(prtGenConn id, packHolder pck, addrIP rtr, counter.reasons err, int lab) {
        return false;
    }

    /**
     * notified that state changed
     *
     * @param id id number to reference connection
     * @param stat state
     * @return return false if successful, true if error happened
     */
    public boolean datagramState(prtGenConn id, state.states stat) {
        return false;
    }

    /**
     * received packet
     *
     * @param id connection
     * @param pck packet
     * @return false on success, true on error
     */
    public boolean datagramRecv(prtGenConn id, packHolder pck) {
        servAmtConn ntry = connFind(id, false);
        if (ntry == null) {
            id.setClosing();
            return false;
        }
        ntry.doRecv(pck);
        return false;
    }

}

class servAmtConn implements ifcDn, Comparator<servAmtConn> {

    public prtGenConn conn;

    public servAmt lower;

    public counter cntr = new counter();

    public ifcUp upper = new ifcNull();

    public cfgIfc acesIfc;

    public int nonce;

    public int compare(servAmtConn o1, servAmtConn o2) {
        return o1.conn.compare(o1.conn, o2.conn);
    }

    public servAmtConn(prtGenConn id, servAmt parent) {
        conn = id;
        lower = parent;
    }

    public String toString() {
        return "amt with " + conn.peerAddr;
    }

    public void doRecv(packHolder pck) {
        cntr.rx(pck);
        int typ = pck.getByte(0);
        switch (typ) {
            case 1: // discovery
                typ = pck.msbGetD(4);
                pck.clear();
                pck.msbPutD(0, 0x02000000);
                pck.msbPutD(4, typ);
                pck.putSkip(8);
                if (conn.iface.addr.isIPv4()) {
                    pck.putAddr(0, conn.iface.addr.toIPv4());
                    pck.putSkip(addrIPv4.size);
                } else {
                    pck.putAddr(0, conn.iface.addr.toIPv6());
                    pck.putSkip(addrIPv6.size);
                }
                pck.merge2beg();
                pck.putDefaults();
                conn.send2net(pck);
                break;
            case 3: // request
                nonce = pck.msbGetD(4);
                break;
            case 5: // membership
                pck.getSkip(12);
                typ = ifcEther.guessEtherType(pck);
                if (typ < 0) {
                    cntr.drop(pck, counter.reasons.badVal);
                    return;
                }
                pck.msbPutW(0, typ);
                pck.putSkip(2);
                pck.merge2beg();
                upper.recvPack(pck);
                break;
        }
    }

    public void doStartup() {
        acesIfc = lower.tempIfc.cloneStart(this);
        setUpper(acesIfc.ethtyp);
        acesIfc.ethtyp.setState(state.states.up);
    }

    public void sendPack(packHolder pck) {
        pck.merge2beg();
        pck.getSkip(2); // ethertype
        if ((pck.IPprt == ipMhost4.protoNum) || (pck.IPprt == ipIcmp6.protoNum)) {
            pck.msbPutW(0, 0x0400);
            pck.msbPutW(2, bits.randomW());
            pck.msbPutD(4, bits.randomD());
            pck.msbPutD(8, nonce);
            pck.putSkip(12);
        } else {
            pck.msbPutW(0, 0x0600);
            pck.putSkip(2);
        }
        pck.merge2beg();
        pck.putDefaults();
        conn.send2net(pck);
    }

    public addrType getHwAddr() {
        return addrMac.getRandom();
    }

    public void setFilter(boolean promisc) {
    }

    public state.states getState() {
        return state.states.up;
    }

    public void closeDn() {
        acesIfc.cloneStop();
        lower.connDel(conn);
        conn.setClosing();
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

}
