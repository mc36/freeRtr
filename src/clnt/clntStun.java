package clnt;

import ifc.ifcDn;
import ifc.ifcNull;
import ifc.ifcUp;
import pack.packHolder;
import pack.packStun;
import pipe.pipeSide;
import serv.servStun;
import user.userTerminal;
import util.bits;
import util.counter;
import util.logger;
import util.state;
import addr.addrEmpty;
import addr.addrIP;
import addr.addrType;
import serv.servGeneric;

/**
 * serial tunneling client
 *
 * @author matecsaba
 */
public class clntStun implements Runnable, ifcDn {

    /**
     * upper layer
     */
    public ifcUp upper = new ifcNull();

    /**
     * target of tunnel
     */
    public String target = null;

    /**
     * proxy profile
     */
    public clntProxy proxy;

    /**
     * group
     */
    public int group = 0;

    /**
     * counter
     */
    public counter cntr = new counter();

    private boolean working = true;

    private packStun lower;

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
        return 1504;
    }

    /**
     * get bandwidth
     *
     * @return bandwidth
     */
    public long getBandwidth() {
        return 8000000;
    }

    /**
     * send packet
     *
     * @param pck packet
     */
    public void sendPack(packHolder pck) {
        if (lower == null) {
            return;
        }
        cntr.tx(pck);
        pck.putDefaults();
        lower.sendPack(pck);
    }

    private void clearState() {
        if (lower != null) {
            lower.setClose();
        }
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

    private void workDoer() {
        addrIP trg = userTerminal.justResolv(target, proxy.prefer);
        if (trg == null) {
            return;
        }
        pipeSide conn = proxy.doConnect(servGeneric.protoTcp, trg, new servStun().srvPort(), "stun");
        if (conn == null) {
            return;
        }
        lower = new packStun(conn, group);
        for (;;) {
            packHolder pck = lower.recvPack();
            if (pck == null) {
                break;
            }
            upper.recvPack(pck);
        }
    }

}
