package net.freertr.clnt;

import net.freertr.addr.addrEmpty;
import net.freertr.addr.addrIP;
import net.freertr.addr.addrType;
import net.freertr.ifc.ifcDn;
import net.freertr.ifc.ifcNull;
import net.freertr.ifc.ifcUp;
import net.freertr.line.lineBstun;
import net.freertr.line.lineHdlc;
import net.freertr.line.lineScript;
import net.freertr.pack.packHolder;
import net.freertr.pipe.pipeSide;
import net.freertr.serv.servBstun;
import net.freertr.serv.servGeneric;
import net.freertr.user.userTerminal;
import net.freertr.util.bits;
import net.freertr.util.counter;
import net.freertr.util.logger;
import net.freertr.util.state;

/**
 * block serial tunneling client
 *
 * @author matecsaba
 */
public class clntBstun implements Runnable, ifcDn {

    /**
     * create instance
     */
    public clntBstun() {
    }

    /**
     * upper layer
     */
    public ifcUp upper = new ifcNull();

    /**
     * chat script to use
     */
    public lineScript script;

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

    private lineBstun lower;

    private lineHdlc hdlc;

    public String toString() {
        return "bstun to " + target;
    }

    /**
     * get hw address
     *
     * @return address
     */
    public addrType getHwAddr() {
        return new addrEmpty();
    }

    /**
     * set filter
     *
     * @param promisc prmiscous mode
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
        if (hdlc == null) {
            return;
        }
        cntr.tx(pck);
        pck.putDefaults();
        hdlc.sendPack(pck);
    }

    private void clearState() {
        if (lower != null) {
            lower.setClose();
        }
        if (hdlc != null) {
            hdlc.closeDn();
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
        pipeSide conn = proxy.doConnect(servGeneric.protoTcp, trg, new servBstun().srvPort(), "bstun");
        if (conn == null) {
            return;
        }
        conn.setTime(120000);
        lower = new lineBstun(conn, group);
        if (script.doScript(lower.getPipe())) {
            conn.setClose();
            return;
        }
        hdlc = new lineHdlc(lower.getPipe());
        hdlc.setUpper(upper);
        for (;;) {
            bits.sleep(1000);
            if (conn.isClosed() != 0) {
                return;
            }
        }
    }

}
