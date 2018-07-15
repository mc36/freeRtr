package clnt;

import addr.addrEmpty;
import addr.addrIP;
import addr.addrType;
import ifc.ifcDn;
import ifc.ifcNull;
import ifc.ifcUp;
import ip.ipFwdIface;
import java.util.Comparator;
import pack.packHolder;
import prt.prtGenConn;
import prt.prtMplsIp;
import prt.prtServP;
import prt.prtUdp;
import serv.servMplsUdp;
import util.bits;
import util.counter;
import util.logger;
import util.state;

/**
 * mpls in udp (rfc7510) client
 *
 * @author matecsaba
 */
public class clntMplsUdp implements Comparator<clntMplsUdp>, Runnable, prtServP, ifcDn {

    /**
     * upper layer
     */
    public ifcUp upper = new ifcNull();

    /**
     * target of tunnel
     */
    public addrIP target = null;

    /**
     * remote port number
     */
    public int prtR;

    /**
     * local port number
     */
    public int prtL;

    /**
     * udp to use
     */
    public prtUdp udp;

    /**
     * source interface
     */
    public ipFwdIface fwdIfc = null;

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

    public String toString() {
        return "mplsudp to " + target;
    }

    public int compare(clntMplsUdp o1, clntMplsUdp o2) {
        return o1.target.compare(o1.target, o2.target);
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

    public void sendPack(packHolder pck) {
        if (conn == null) {
            return;
        }
        if (prtMplsIp.ethtyp2mpls(pck)) {
            return;
        }
        cntr.tx(pck);
        pck.putDefaults();
        conn.send2net(pck);
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
        if (prtR == 0) {
            prtR = servMplsUdp.portNum;
        }
        if (prtL == 0) {
            prtL = servMplsUdp.portNum;
        }
        conn = udp.packetConnect(this, fwdIfc, prtL, target, prtR, "mplsudp", null, -1);
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
        }
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

    public boolean datagramRecv(prtGenConn id, packHolder pck) {
        if (prtMplsIp.mpls2ethtyp(pck)) {
            return false;
        }
        cntr.rx(pck);
        upper.recvPack(pck);
        return false;
    }

}
