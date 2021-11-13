package net.freertr.serv;

import java.util.Comparator;
import java.util.List;
import net.freertr.addr.addrIP;
import net.freertr.cfg.cfgAll;
import net.freertr.cfg.cfgIfc;
import net.freertr.ip.ipFwd;
import net.freertr.ip.ipFwdIface;
import net.freertr.ip.ipPrt;
import net.freertr.pack.packHolder;
import net.freertr.pipe.pipeSide;
import net.freertr.prt.prtGenConn;
import net.freertr.prt.prtMplsIp;
import net.freertr.tab.tabGen;
import net.freertr.user.userFilter;
import net.freertr.user.userHelping;
import net.freertr.util.bits;
import net.freertr.util.cmds;
import net.freertr.util.counter;
import net.freertr.util.state;

/**
 * mpls in ip (rfc4023) server
 *
 * @author matecsaba
 */
public class servMplsIp extends servGeneric implements ipPrt {

    /**
     * create instance
     */
    public servMplsIp() {
    }

    /**
     * interface to use
     */
    public cfgIfc tempIfc;

    /**
     * list of connections
     */
    public tabGen<servMplsIpConn> conns = new tabGen<servMplsIpConn>();

    /**
     * timeout
     */
    public int timeout = 60000;

    /**
     * counter
     */
    public counter cntr;

    /**
     * defaults text
     */
    public final static String[] defaultL = {
        "server mplsip .*! port " + prtMplsIp.prot,
        "server mplsip .*! protocol " + proto2string(protoAllDgrm),
        "server mplsip .*! timeout 60000"
    };

    /**
     * defaults filter
     */
    public static tabGen<userFilter> defaultF;

    public tabGen<userFilter> srvDefFlt() {
        return defaultF;
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
        l.add(null, "1 2  clone                        set interface to clone");
        l.add(null, "2 .    <name:ifc>                 name of interface");
        l.add(null, "1 2  timeout                      timeout of client");
        l.add(null, "2 .    <num>                      milliseconds");
    }

    public String srvName() {
        return "mplsip";
    }

    public int srvPort() {
        return prtMplsIp.prot;
    }

    public int srvProto() {
        return protoAllDgrm;
    }

    public boolean srvInit() {
        return genRawStart(this, 0);
    }

    public boolean srvDeinit() {
        return genRawStop(this, 0);
    }

    public boolean srvAccept(pipeSide pipe, prtGenConn id) {
        return true;
    }

    /**
     * get protocol number
     *
     * @return number
     */
    public int getProtoNum() {
        return prtMplsIp.prot;
    }

    public String toString() {
        return "mplsip";
    }

    /**
     * close interface
     *
     * @param iface interface
     */
    public void closeUp(ipFwdIface iface) {
    }

    /**
     * set state
     *
     * @param iface interface
     * @param stat state
     */
    public void setState(ipFwdIface iface, state.states stat) {
    }

    /**
     * received packet
     *
     * @param rxIfc interface
     * @param pck packet
     */
    public void recvPack(ipFwdIface rxIfc, packHolder pck) {
        servMplsIpConn ntry = new servMplsIpConn(rxIfc, pck.IPsrc, this);
        servMplsIpConn old = conns.add(ntry);
        if (old != null) {
            old.doRecv(pck);
            return;
        }
        if (tempIfc == null) {
            return;
        }
        if (srvCheckAccept(rxIfc, pck)) {
            return;
        }
        ntry.doStartup();
        ntry.doRecv(pck);
    }

    /**
     * alert packet
     *
     * @param rxIfc interface
     * @param pck packet
     * @return false on success, true on error
     */
    public boolean alertPack(ipFwdIface rxIfc, packHolder pck) {
        return true;
    }

    /**
     * error packet
     *
     * @param err error code
     * @param rtr address
     * @param rxIfc interface
     * @param pck packet
     */
    public void errorPack(counter.reasons err, addrIP rtr, ipFwdIface rxIfc, packHolder pck) {
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

class servMplsIpConn implements Runnable, Comparator<servMplsIpConn> {

    public servMplsIp lower;

    public ipFwd fwdCor;

    public ipFwdIface iface;

    public addrIP peer;

    public cfgIfc acesIfc;

    public prtMplsIp worker;

    public boolean seenPack;

    public servMplsIpConn(ipFwdIface ifc, addrIP adr, servMplsIp parent) {
        iface = ifc;
        peer = adr.copyBytes();
        lower = parent;
        fwdCor = lower.srvVrf.getFwd(peer);
        worker = new prtMplsIp(fwdCor);
    }

    public String toString() {
        return "mplsip with " + peer;
    }

    public int compare(servMplsIpConn o1, servMplsIpConn o2) {
        int i = o1.iface.compare(o1.iface, o2.iface);
        if (i != 0) {
            return i;
        }
        return o1.peer.compare(o1.peer, o2.peer);
    }

    public void doStartup() {
        worker.setEndpoints(iface, peer, false);
        acesIfc = lower.tempIfc.cloneStart(worker);
        worker.setUpper(acesIfc.ethtyp);
        acesIfc.ethtyp.setState(state.states.up);
        new Thread(this).start();
    }

    public void doRecv(packHolder pck) {
        seenPack = true;
        worker.recvPack(iface, pck);
    }

    public void run() {
        for (;;) {
            bits.sleep(lower.timeout);
            if (!seenPack) {
                break;
            }
            seenPack = false;
        }
        acesIfc.cloneStop();
        worker.closeDn();
        lower.conns.del(this);
    }

}
