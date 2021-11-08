package net.freertr.clnt;

import net.freertr.addr.addrIP;
import net.freertr.cfg.cfgIfc;
import net.freertr.cfg.cfgVrf;
import net.freertr.ip.ipFwd;
import net.freertr.ip.ipFwdEcho;
import net.freertr.ip.ipFwdIface;
import net.freertr.ip.ipFwdTab;
import net.freertr.ip.ipPrt;
import net.freertr.pack.packHolder;
import net.freertr.prt.prtGenConn;
import net.freertr.prt.prtServP;
import net.freertr.prt.prtUdp;
import net.freertr.util.bits;
import net.freertr.util.counter;
import net.freertr.util.notifier;
import net.freertr.util.state;

/**
 * traceroute helper
 *
 * @author matecsaba
 */
public class clntTrace implements prtServP, ipPrt {

    /**
     * create instance
     */
    public clntTrace() {
    }

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
    public int port;

    /**
     * target protocol
     */
    public int proto;

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

    private ipFwdIface ifc2;

    private ipFwd fwd;

    private prtUdp udp;

    private notifier notif = new notifier();

    private int magic;

    private long started;

    public String toString() {
        return "traceroute to " + trg;
    }

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
        fwd = vrf.getFwd(trg);
        udp = vrf.getUdp(trg);
        ifc2 = null;
        if (ifc != null) {
            ifc2 = ifc.getFwdIfc(trg);
        } else {
            ifc2 = ipFwdTab.findSendingIface(fwd, trg);
        }
        if (ifc2 == null) {
            return true;
        }
        if (proto > 0) {
            return fwd.protoAdd(this, ifc2, trg);
        } else {
            con = udp.packetConnect(this, ifc2, 0, trg, port, "traceroute", null, -1);
            return con == null;
        }
    }

    /**
     * unregister from protocol
     */
    public void unregister2ip() {
        if (proto > 0) {
            fwd.protoDel(this, ifc2, trg);
        } else {
            if (con != null) {
                con.setClosing();
            }
            con = null;
        }
    }

    /**
     * do one round
     *
     * @param ttl hop number
     * @param tos type of service
     * @param flw flow label
     * @param tim timeout
     * @param len size
     * @return true on error, false on success
     */
    public boolean doRound(int ttl, int tos, int flw, int tim, int len) {
        errRtr = null;
        errLab = -1;
        errCod = null;
        errTim = tim;
        if (con != null) {
            con.sendTOS = tos;
            con.sendTTL = ttl;
            con.sendFLW = flw;
        }
        packHolder pck = new packHolder(true, true);
        pck.putDefaults();
        magic = bits.randomD();
        pck.msbPutD(0, magic);
        pck.putSkip(len);
        pck.merge2beg();
        pck.IPttl = ttl;
        pck.IPtos = tos;
        pck.IPid = flw;
        started = bits.getTime();
        pck.IPprt = proto;
        pck.IPsrc.setAddr(ifc2.addr);
        pck.IPtrg.setAddr(trg);
        if (proto > 0) {
            fwd.protoPack(ifc2, null, pck);
        } else {
            con.send2net(pck);
        }
        if (tim > 0) {
            notif.misleep(tim);
        }
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

    private void gotPack(packHolder pck, addrIP rtr, counter.reasons err, int lab) {
        if (pck.msbGetD(0) != magic) {
            return;
        }
        magic++;
        errTim = (int) (bits.getTime() - started);
        errRtr = rtr.copyBytes();
        errCod = err;
        errLab = lab;
        notif.wakeup();
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
        gotPack(pck, rtr, err, lab);
        return false;
    }

    /**
     * notified that state changed
     *
     * @param id id number to reference connection
     * @param stat state
     * @return return false if successful, true if error happened
     */
    public boolean datagramState(prtGenConn id, state.states stat) {
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
        gotPack(pck, pck.IPsrc, null, -1);
        return false;
    }

    public int getProtoNum() {
        return proto;
    }

    public void closeUp(ipFwdIface iface) {
    }

    public void setState(ipFwdIface iface, state.states stat) {
    }

    public void recvPack(ipFwdIface rxIfc, packHolder pck) {
        gotPack(pck, pck.IPsrc, null, -1);
    }

    public boolean alertPack(ipFwdIface rxIfc, packHolder pck) {
        return false;
    }

    public void errorPack(counter.reasons err, addrIP rtr, ipFwdIface rxIfc, packHolder pck) {
        int lab = ipFwdEcho.getMplsExt(pck);
        gotPack(pck, rtr, err, lab);
    }

    public counter getCounter() {
        return new counter();
    }

}
