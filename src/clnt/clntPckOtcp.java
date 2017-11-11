package clnt;

import addr.addrEmpty;
import addr.addrIP;
import addr.addrType;
import ifc.ifcDn;
import ifc.ifcNull;
import ifc.ifcUp;
import pack.packHolder;
import pack.packSize;
import pipe.pipeSide;
import serv.servGeneric;
import user.userTerminal;
import util.bits;
import util.counter;
import util.logger;
import util.state;

/**
 * packet over dtls encapsulation client
 *
 * @author matecsaba
 */
public class clntPckOtcp implements Runnable, ifcDn {

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
     * local port number
     */
    public int prtL;

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

    private packSize pips = null;

    public String toString() {
        return "pckOtcp to " + target;
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

    private void clearState() {
        if (pipe != null) {
            pipe.setClose();
        }
        pipe = null;
        pips = null;
    }

    private void workDoer() {
        if (proxy == null) {
            return;
        }
        addrIP trg = userTerminal.justResolv(target, proxy.prefer);
        if (trg == null) {
            return;
        }
        pipe = proxy.doConnect(servGeneric.protoTcp, trg, prtR, "pckotcp");
        if (pipe == null) {
            return;
        }
        pips = new packSize(pipe, 2, true, 1, 0);
        packHolder pck = new packHolder(true, true);
        for (;;) {
            if (pipe == null) {
                break;
            }
            if (pipe.isClosed() != 0) {
                break;
            }
            pck.clear();
            if (pips.recvPacket(pck)) {
                break;
            }
            upper.recvPack(pck);
        }
    }

    public void sendPack(packHolder pck) {
        if (pipe == null) {
            return;
        }
        pck.putDefaults();
        pips.sendPacket(pck);
    }

}
