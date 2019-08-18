package prt;

import ip.ipFwdIface;
import pipe.pipeLine;
import pipe.pipeSide;
import util.notifier;
import addr.addrIP;

/**
 * accept one protocol connection
 *
 * @author matecsaba
 */
public class prtAccept implements prtServS {

    private final prtGen pr;

    private final ipFwdIface li;

    private final int lp;

    private final int rm;

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
            if (ra.compare(ra, id.peerAddr) != 0) {
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
     * @param remM remote mask, 0 means host route
     * @param remP remote port, 0 means all
     * @param name name of server
     * @param pwd password
     * @param ttl time to live
     */
    public prtAccept(prtGen prot, pipeLine pip, ipFwdIface locI, int locP, addrIP remA, int remM, int remP, String name, String pwd, int ttl) {
        pr = prot;
        li = locI;
        lp = locP;
        if (remA == null) {
            ra = null;
        } else {
            ra = remA.copyBytes();
        }
        rm = remM;
        rp = remP;
        pr.streamListen(this, pip, li, lp, ra, rm, rp, name, pwd, ttl);
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
            pr.listenStop(li, lp, ra, rm, rp);
            notif.wakeup();
        }
        pipeSide r = pip;
        pip = null;
        return r;
    }

}
