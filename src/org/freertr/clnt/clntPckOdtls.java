package org.freertr.clnt;

import org.freertr.addr.addrEmpty;
import org.freertr.addr.addrIP;
import org.freertr.addr.addrType;
import org.freertr.ifc.ifcDn;
import org.freertr.ifc.ifcNull;
import org.freertr.ifc.ifcUp;
import org.freertr.pack.packHolder;
import org.freertr.pipe.pipeSide;
import org.freertr.serv.servGeneric;
import org.freertr.util.bits;
import org.freertr.util.counter;
import org.freertr.util.logger;
import org.freertr.util.state;

/**
 * packet over dtls encapsulation client
 *
 * @author matecsaba
 */
public class clntPckOdtls implements Runnable, ifcDn {

    /**
     * create instance
     */
    public clntPckOdtls() {
    }

    /**
     * upper layer
     */
    public ifcUp upper = new ifcNull();

    /**
     * target of tunnel
     */
    public String target = null;

    /**
     * remote port number
     */
    public int prtR;

    /**
     * proxy profile
     */
    public clntProxy proxy;

    /**
     * counter
     */
    public counter cntr = new counter();

    private boolean working = true;

    private pipeSide pipe = null;

    public String toString() {
        return "pckOdtls to " + target;
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
        return 1400;
    }

    /**
     * get bandwidth
     *
     * @return bandwidth
     */
    public long getBandwidth() {
        return 4000000;
    }

    /**
     * start connection
     */
    public void workStart() {
        logger.startThread(this);
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

    private void clearState() {
        if (pipe != null) {
            pipe.setClose();
        }
        pipe = null;
    }

    private void workDoer() {
        if (proxy == null) {
            return;
        }
        addrIP trg = clntDns.justResolv(target, proxy.prefer);
        if (trg == null) {
            return;
        }
        pipe = proxy.doConnect(servGeneric.protoUdp, trg, prtR, "pckodtls");
        if (pipe == null) {
            return;
        }
        pipe.setTime(120000);
        packHolder pck = new packHolder(true, true);
        for (;;) {
            if (pipe == null) {
                break;
            }
            if (pipe.isClosed() != 0) {
                break;
            }
            pck.clear();
            if (pck.pipeRecv(pipe, 0, 8192, 143) < 1) {
                break;
            }
            upper.recvPack(pck);
        }
    }

    /**
     * send packet
     *
     * @param pck packet
     */
    public void sendPack(packHolder pck) {
        if (pipe == null) {
            return;
        }
        pck.putDefaults();
        pck.pipeSend(pipe, 0, pck.dataSize(), 2);
    }

}
