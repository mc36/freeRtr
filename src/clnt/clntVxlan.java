package clnt;

import addr.addrIP;
import addr.addrMac;
import addr.addrType;
import cfg.cfgIfc;
import cfg.cfgVrf;
import ifc.ifcDn;
import ifc.ifcNull;
import ifc.ifcUp;
import ip.ipFwdIface;
import pack.packHolder;
import pack.packVxlan;
import prt.prtGenConn;
import prt.prtServP;
import prt.prtUdp;
import user.userTerminal;
import util.bits;
import util.counter;
import util.logger;
import util.state;

/**
 * vxlan (rfc7348) client
 *
 * @author matecsaba
 */
public class clntVxlan implements Runnable, prtServP, ifcDn {

    /**
     * upper layer
     */
    public ifcUp upper = new ifcNull();

    /**
     * target of tunnel
     */
    public String target = null;

    /**
     * instance id
     */
    public int inst;

    /**
     * vrf of target
     */
    public cfgVrf vrf = null;

    /**
     * source interface
     */
    public cfgIfc srcIfc = null;

    /**
     * wildcard port listener
     */
    public boolean wildcard = false;

    /**
     * sending ttl value, -1 means maps out
     */
    public int sendingTTL = 255;

    /**
     * sending tos value, -1 means maps out
     */
    public int sendingTOS = -1;

    /**
     * counter
     */
    public counter cntr = new counter();

    private prtGenConn conn;

    private boolean working = true;

    private addrIP trg;

    private prtUdp udp;

    private ipFwdIface fwdIfc;

    public String toString() {
        return "vxlan to " + target;
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
        clearState();
    }

    public void flapped() {
        clearState();
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
        return 4000000;
    }

    public void sendPack(packHolder pckBin) {
        pckBin.merge2beg();
        if (conn == null) {
            return;
        }
        cntr.tx(pckBin);
        packVxlan pckVxl = new packVxlan();
        pckVxl.instance = inst;
        pckVxl.createHeader(pckBin);
        pckBin.merge2beg();
        pckBin.putDefaults();
        conn.send2net(pckBin);
    }

    /**
     * start connection
     */
    public void workStart() {
        new Thread(this).start();
    }

    /**
     * stop connection
     */
    public void workStop() {
        working = false;
        clearState();
    }

    public void run() {
        for (;;) {
            if (!working) {
                break;
            }
            try {
                clearState();
                workDoer();
            } catch (Exception e) {
                logger.traceback(e);
            }
            clearState();
            bits.sleep(1000);
        }
    }

    private void workDoer() {
        trg = userTerminal.justResolv(target, 0);
        if (trg == null) {
            return;
        }
        udp = vrf.getUdp(trg);
        fwdIfc = null;
        if (srcIfc != null) {
            fwdIfc = srcIfc.getFwdIfc(trg);
        }
        if (wildcard) {
            udp.packetListen(this, fwdIfc, packVxlan.port, trg, 0, 0, "vxlan", null, -1);
        }
        conn = udp.packetConnect(this, fwdIfc, packVxlan.port, trg, packVxlan.port, "vxlan", null, -1);
        if (conn == null) {
            return;
        }
        conn.timeout = 120000;
        conn.sendTOS = sendingTOS;
        conn.sendTTL = sendingTTL;
        for (;;) {
            bits.sleep(1000);
            if (!working) {
                return;
            }
            if (conn.txBytesFree() < 0) {
                return;
            }
        }
    }

    private void clearState() {
        if (conn != null) {
            conn.setClosing();
            if (wildcard) {
                udp.listenStop(fwdIfc, packVxlan.port, trg, 0, 0);
            }
        }
    }

    public void closedInterface(ipFwdIface ifc) {
    }

    public boolean datagramAccept(prtGenConn id) {
        if (!wildcard) {
            return true;
        }
        id.timeout = 120000;
        return false;
    }

    public void datagramReady(prtGenConn id) {
    }

    public void datagramClosed(prtGenConn id) {
    }

    public void datagramWork(prtGenConn id) {
    }

    public boolean datagramRecv(prtGenConn id, packHolder pckBin) {
        if (!wildcard) {
            if (id.compare(id, conn) != 0) {
                id.setClosing();
                return false;
            }
        }
        cntr.rx(pckBin);
        packVxlan pckVxl = new packVxlan();
        if (pckVxl.parseHeader(pckBin)) {
            return false;
        }
        if (pckVxl.instance != inst) {
            return false;
        }
        upper.recvPack(pckBin);
        return false;
    }

}
