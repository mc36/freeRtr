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
        return 1504;
    }

    public long getBandwidth() {
        return 8000000;
    }

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
