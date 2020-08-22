package clnt;

import addr.addrIP;
import cfg.cfgIfc;
import cfg.cfgVrf;
import ip.ipFwdIface;
import pack.packHolder;
import prt.prtGenConn;
import prt.prtServP;
import prt.prtUdp;
import util.bits;
import util.counter;
import util.notifier;

/**
 * traceroute helper
 *
 * @author matecsaba
 */
public class clntTrace implements prtServP {

    /**
     * vrf to use
     */
    public cfgVrf vrf;

    /**
     * iface to use
     */
    public cfgIfc ifc;

    /**
     * target address
     */
    public addrIP trg;

    /**
     * target port
     */
    public int prt;

    /**
     * reporting router
     */
    public addrIP errRtr;

    /**
     * reported error
     */
    public counter.reasons errCod;

    /**
     * reported label
     */
    public int errLab;

    /**
     * reported time
     */
    public int errTim;

    private prtGenConn con;

    private notifier notif = new notifier();

    private int magic;

    private long started;

    /**
     * register to protocol
     *
     * @return false on success, true on error
     */
    public boolean register2ip() {
        if (trg == null) {
            return true;
        }
        if (vrf == null) {
            return true;
        }
        ipFwdIface ifc2 = null;
        if (ifc != null) {
            ifc2 = ifc.getFwdIfc(trg);
        }
        prtUdp udp = vrf.getUdp(trg);
        con = udp.packetConnect(this, ifc2, 0, trg, prt, "traceroute", null, -1);
        return con == null;
    }

    /**
     * unregister from protocol
     */
    public void unregister2ip() {
        if (con != null) {
            con.setClosing();
        }
    }

    /**
     * do one round
     *
     * @param ttl hop number
     * @param tos type of service
     * @param tim timeout
     * @param len size
     * @return true on error, false on success
     */
    public boolean doRound(int ttl, int tos, int tim, int len) {
        if (con == null) {
            return true;
        }
        errRtr = null;
        errLab = -1;
        errCod = null;
        errTim = tim;
        con.sendTOS = tos;
        con.sendTTL = ttl;
        packHolder pck = new packHolder(true, true);
        magic = bits.randomD();
        pck.msbPutD(0, magic);
        pck.putSkip(len);
        started = bits.getTime();
        con.send2net(pck);
        notif.misleep(tim);
        return errRtr == null;
    }

    /**
     * closed interface
     *
     * @param ifc interface
     */
    public void closedInterface(ipFwdIface ifc) {
    }

    /**
     * accept connection
     *
     * @param id connection
     * @return false on success, true on error
     */
    public boolean datagramAccept(prtGenConn id) {
        return true;
    }

    /**
     * connection ready
     *
     * @param id connection
     */
    public void datagramReady(prtGenConn id) {
    }

    /**
     * connection closed
     *
     * @param id connection
     */
    public void datagramClosed(prtGenConn id) {
    }

    /**
     * work connection
     *
     * @param id connection
     */
    public void datagramWork(prtGenConn id) {
    }

    /**
     * received error
     *
     * @param id connection
     * @param pck packet
     * @param rtr reporting router
     * @param err error happened
     * @param lab error label
     * @return false on success, true on error
     */
    public boolean datagramError(prtGenConn id, packHolder pck, addrIP rtr, counter.reasons err, int lab) {
        if (pck.msbGetD(0) != magic) {
            return true;
        }
        magic++;
        errTim = (int) (bits.getTime() - started);
        errRtr = rtr.copyBytes();
        errCod = err;
        errLab = lab;
        notif.wakeup();
        return false;
    }

    /**
     * received packet
     *
     * @param id connection
     * @param pck packet
     * @return false on success, true on error
     */
    public boolean datagramRecv(prtGenConn id, packHolder pck) {
        if (pck.msbGetD(0) != magic) {
            return true;
        }
        magic++;
        errTim = (int) (bits.getTime() - started);
        errRtr = pck.IPsrc.copyBytes();
        errLab = -1;
        errCod = null;
        notif.wakeup();
        return false;
    }

}
