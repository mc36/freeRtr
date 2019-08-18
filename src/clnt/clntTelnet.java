package clnt;

import addr.addrEmpty;
import addr.addrIP;
import addr.addrType;
import ifc.ifcDn;
import ifc.ifcNull;
import ifc.ifcUp;
import line.lineHdlc;
import line.lineScript;
import pack.packHolder;
import pipe.pipeSide;
import sec.secClient;
import serv.servGeneric;
import serv.servTelnet;
import user.userTerminal;
import util.bits;
import util.counter;
import util.logger;
import util.state;

/**
 * ppp over telnet client
 *
 * @author matecsaba
 */
public class clntTelnet implements Runnable, ifcDn {

    /**
     * upper layer
     */
    public ifcUp upper = new ifcNull();

    /**
     * enable security
     */
    public boolean tls;

    /**
     * port number
     */
    public int port;

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
     * counter
     */
    public counter cntr = new counter();

    private boolean working = true;

    private lineHdlc hdlc;

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
        if (hdlc == null) {
            return;
        }
        cntr.tx(pck);
        pck.putDefaults();
        hdlc.sendPack(pck);
    }

    private void clearState() {
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
        if (port == 0) {
            port = new servTelnet().srvPort();
        }
        addrIP trg = userTerminal.justResolv(target, proxy.prefer);
        if (trg == null) {
            return;
        }
        pipeSide conn = proxy.doConnect(servGeneric.protoTcp, trg, port, "telnet");
        if (tls) {
            conn = secClient.openSec(conn, servGeneric.protoTls, null, null);
        }
        if (conn == null) {
            return;
        }
        if (script.doScript(conn)) {
            conn.setClose();
            return;
        }
        hdlc = new lineHdlc(conn);
        hdlc.setUpper(upper);
        for (;;) {
            bits.sleep(1000);
            if (conn.isClosed() != 0) {
                return;
            }
        }
    }

}
