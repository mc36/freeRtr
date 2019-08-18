package serv;

import addr.addrIP;
import cfg.cfgAll;
import cfg.cfgBrdg;
import clnt.clntEtherIp;
import ifc.ifcBridgeIfc;
import ip.ipFwd;
import ip.ipFwdIface;
import ip.ipPrt;
import java.util.Comparator;
import java.util.List;
import pack.packHolder;
import pipe.pipeSide;
import prt.prtGenConn;
import tab.tabGen;
import user.userFilter;
import user.userHelping;
import util.bits;
import util.cmds;
import util.counter;
import util.state;

/**
 * etherip (rfc3378) server
 *
 * @author matecsaba
 */
public class servEtherIp extends servGeneric implements ipPrt {

    /**
     * interface to use
     */
    public cfgBrdg brdgIfc;

    /**
     * list of connections
     */
    public tabGen<servEtherIpConn> conns = new tabGen<servEtherIpConn>();

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
    public final static String defaultL[] = {
        "server etherip .*! port " + clntEtherIp.prot,
        "server etherip .*! protocol " + proto2string(protoAllDgrm),
        "server etherip .*! timeout 60000"
    };

    /**
     * defaults filter
     */
    public static tabGen<userFilter> defaultF;

    public tabGen<userFilter> srvDefFlt() {
        return defaultF;
    }

    public void srvShRun(String beg, List<String> l) {
        if (brdgIfc == null) {
            l.add(beg + "no bridge");
        } else {
            l.add(beg + "bridge " + brdgIfc.name);
        }
        l.add(beg + "timeout " + timeout);
    }

    public boolean srvCfgStr(cmds cmd) {
        String s = cmd.word();
        if (s.equals("timeout")) {
            timeout = bits.str2num(cmd.word());
            return false;
        }
        if (s.equals("bridge")) {
            brdgIfc = cfgAll.brdgFind(cmd.word(), false);
            if (brdgIfc == null) {
                cmd.error("no such bridge group");
                return false;
            }
            return false;
        }
        if (!s.equals("no")) {
            return true;
        }
        s = cmd.word();
        if (s.equals("bridge")) {
            brdgIfc = null;
            return false;
        }
        return true;
    }

    public void srvHelp(userHelping l) {
        l.add("1 2  bridge                       set interface to clone");
        l.add("2 .    <name>                     name of interface");
        l.add("1 2  timeout                      timeout of client");
        l.add("2 .    <num>                      milliseconds");
    }

    public String srvName() {
        return "etherip";
    }

    public int srvPort() {
        return clntEtherIp.prot;
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
        return clntEtherIp.prot;
    }

    public String toString() {
        return "etherip";
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
        servEtherIpConn ntry = new servEtherIpConn(rxIfc, pck.IPsrc, this);
        servEtherIpConn old = conns.add(ntry);
        if (old != null) {
            old.doRecv(pck);
            return;
        }
        if (brdgIfc == null) {
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

class servEtherIpConn implements Runnable, Comparator<servEtherIpConn> {

    public servEtherIp lower;

    public ipFwd fwdCor;

    public ipFwdIface iface;

    public addrIP peer;

    public clntEtherIp worker;

    public ifcBridgeIfc brdgIfc;

    public boolean seenPack;

    public servEtherIpConn(ipFwdIface ifc, addrIP adr, servEtherIp parent) {
        iface = ifc;
        peer = adr.copyBytes();
        lower = parent;
        fwdCor = lower.srvVrf.getFwd(peer);
        worker = new clntEtherIp();
    }

    public int compare(servEtherIpConn o1, servEtherIpConn o2) {
        int i = o1.iface.compare(o1.iface, o2.iface);
        if (i != 0) {
            return i;
        }
        return o1.peer.compare(o1.peer, o2.peer);
    }

    public void doStartup() {
        worker.setEndpoints(fwdCor, iface, peer);
        brdgIfc = lower.brdgIfc.bridgeHed.newIface(false, true, false);
        worker.setUpper(brdgIfc);
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
        brdgIfc.closeUp();
        worker.closeDn();
        lower.conns.del(this);
    }

}
