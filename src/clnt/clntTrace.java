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

    private prtGenConn con;

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
     * @return address of hop, null if nothing
     */
    public addrIP doRound(int ttl, int tos, int tim, int len) {
        if (con == null) {
            return null;
        }
        con.errRtr = null;
        con.errCod = null;
        con.sendTOS = tos;
        con.sendTTL = ttl;
        packHolder pck = new packHolder(true, true);
        pck.msbPutQ(0, bits.getTime());
        pck.putSkip(len);
        con.send2net(pck);
        bits.sleep(tim);
        if (con.errRtr == null) {
            return null;
        }
        return con.errRtr;
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
     * received packet
     *
     * @param id connection
     * @param pck packet
     * @return false on success, true on error
     */
    public boolean datagramRecv(prtGenConn id, packHolder pck) {
        con.errRtr = pck.IPsrc.copyBytes();
        return false;
    }

}
