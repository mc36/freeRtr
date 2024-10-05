package org.freertr.prt;

import org.freertr.addr.addrIP;
import org.freertr.ip.ipFwdIface;
import org.freertr.pipe.pipeLine;
import org.freertr.pipe.pipeSide;
import org.freertr.util.notifier;

/**
 * accept one protocol connection
 *
 * @author matecsaba
 */
public class prtAccept implements prtServS {

    private final prtGen pr;

    private final ipFwdIface li;

    private final int lp;

    private final int rp;

    private final addrIP ra;

    private final notifier notif = new notifier();

    private pipeSide pip;

    public void closedInterface(ipFwdIface ifc) {
        notif.wakeup();
    }

    /**
     * accept connection
     *
     * @param pipe pipeline
     * @param id connection
     * @return false on success, true on error
     */
    public synchronized boolean streamAccept(pipeSide pipe, prtGenConn id) {
        if (ra != null) {
            if (ra.compareTo(id.peerAddr) != 0) {
                return true;
            }
        }
        if (pip != null) {
            return true;
        }
        pip = pipe;
        notif.wakeup();
        return false;
    }

    public boolean streamForceBlock() {
        return true;
    }

    /**
     * start listening on port
     *
     * @param prot protocol to use
     * @param pip sample pipeline
     * @param locI local interface, 0 means all
     * @param locP local port, 0 means all
     * @param remA remote address, null means all
     * @param remP remote port, 0 means all
     * @param name name of server
     * @param kid key id if applicable
     * @param pwd password
     * @param ttl time to live
     * @param tos type of service
     */
    public prtAccept(prtGen prot, pipeLine pip, ipFwdIface locI, int locP, addrIP remA, int remP, String name, int kid, String pwd, int ttl, int tos) {
        pr = prot;
        li = locI;
        lp = locP;
        if (remA == null) {
            ra = null;
        } else {
            ra = remA.copyBytes();
        }
        rp = remP;
        pr.streamListen(this, pip, li, lp, ra, rp, name, kid, pwd, ttl, tos);
    }

    /**
     * wait for connection
     *
     * @param tim time in ms, 0 to forever
     */
    public void wait4conn(int tim) {
        notif.misleep(tim);
    }

    /**
     * get connection
     *
     * @param shut shutdown befure
     * @return connection, null if nothing
     */
    public pipeSide getConn(boolean shut) {
        if (shut) {
            pr.listenStop(li, lp, ra, rp);
            notif.wakeup();
        }
        pipeSide r = pip;
        pip = null;
        return r;
    }

}
