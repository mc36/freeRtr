package net.freertr.clnt;

import net.freertr.addr.addrEmpty;
import net.freertr.addr.addrIP;
import net.freertr.addr.addrType;
import net.freertr.cfg.cfgIfc;
import net.freertr.cfg.cfgVrf;
import net.freertr.ifc.ifcDn;
import net.freertr.ifc.ifcNull;
import net.freertr.ifc.ifcUp;
import net.freertr.ip.ipFwdIface;
import net.freertr.pack.packHolder;
import net.freertr.pack.packRtp;
import net.freertr.pipe.pipeLine;
import net.freertr.pipe.pipeSide;
import net.freertr.pipe.pipeSync;
import net.freertr.prt.prtUdp;
import net.freertr.user.userTerminal;
import net.freertr.util.bits;
import net.freertr.util.counter;
import net.freertr.util.logger;
import net.freertr.util.state;

/**
 * time division multiplexing over ip (rfc5087) client
 *
 * @author matecsaba
 */
public class clntTdmOudp implements Runnable, ifcDn {

    /**
     * create instance
     */
    public clntTdmOudp() {
    }

    /**
     * default port
     */
    public final static int defPort = 2142;

    /**
     * upper layer
     */
    public ifcUp upper = new ifcNull();

    /**
     * preferred ip protocol version
     */
    public int prefer = 0;

    /**
     * target of tunnel
     */
    public String target = null;

    /**
     * vrf of target
     */
    public cfgVrf vrf = null;

    /**
     * source interface
     */
    public cfgIfc srcIfc = null;

    /**
     * remote port number
     */
    public int prtR;

    /**
     * local port number
     */
    public int prtL;

    /**
     * first channel to encode
     */
    public int chanMin;

    /**
     * last channel to encode
     */
    public int chanMax;

    /**
     * counter
     */
    public counter cntr = new counter();

    private packRtp conn;

    private pipeSide pipe;

    private boolean working = true;

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
        addrIP trg = userTerminal.justResolv(target, prefer);
        if (trg == null) {
            return;
        }
        prtUdp udp = vrf.getUdp(trg);
        ipFwdIface fwdIfc = null;
        if (srcIfc != null) {
            fwdIfc = srcIfc.getFwdIfc(trg);
        }
        if (prtR == 0) {
            prtR = defPort;
        }
        if (prtL == 0) {
            prtL = defPort;
        }
        conn = new packRtp();
        if (conn.startConnect(udp, new pipeLine(32768, true), fwdIfc, prtL, trg, prtR)) {
            return;
        }
        pipeLine pip = new pipeLine(65535, true);
        pipe = pip.getSide();
        new pipeSync(pip.getSide(), conn, chanMin, chanMax - chanMin + 1, 0);
        packHolder pck = new packHolder(true, true);
        for (;;) {
            if (!working) {
                return;
            }
            if (conn.isClosed() != 0) {
                return;
            }
            if (pipe.isClosed() != 0) {
                return;
            }
            pck.clear();
            if (pck.pipeRecv(pipe, 0, 8192, 143) < 1) {
                return;
            }
            cntr.rx(pck);
            upper.recvPack(pck);
        }
    }

    private void clearState() {
        if (conn != null) {
            conn.setClose();
        }
        if (pipe != null) {
            pipe.setClose();
        }
    }

    public String toString() {
        return "tdmOudp to " + target;
    }

    /**
     * get hw address
     *
     * @return hw address
     */
    public addrType getHwAddr() {
        return new addrEmpty();
    }

    /**
     * set filter
     *
     * @param promisc promiscous mode
     */
    public void setFilter(boolean promisc) {
    }

    /**
     * get state
     *
     * @return state
     */
    public state.states getState() {
        return state.states.up;
    }

    /**
     * close interface
     */
    public void closeDn() {
        clearState();
    }

    /**
     * flap interface
     */
    public void flapped() {
        clearState();
    }

    /**
     * set upper layer
     *
     * @param server upper layer
     */
    public void setUpper(ifcUp server) {
        upper = server;
        upper.setParent(this);
    }

    /**
     * get counter
     *
     * @return counter
     */
    public counter getCounter() {
        return cntr;
    }

    /**
     * get mtu size
     *
     * @return mtu size
     */
    public int getMTUsize() {
        return 1500;
    }

    /**
     * get bandwidth
     *
     * @return bandwidth
     */
    public long getBandwidth() {
        return 64000;
    }

    public void sendPack(packHolder pck) {
        if (pipe == null) {
            return;
        }
        cntr.tx(pck);
        pck.pipeSend(pipe, 0, pck.dataSize(), 1);
    }

}
