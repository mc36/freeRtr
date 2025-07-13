package org.freertr.ifc;

import java.net.DatagramPacket;
import java.net.DatagramSocket;
import java.net.InetAddress;
import java.net.InetSocketAddress;
import org.freertr.addr.addrEmpty;
import org.freertr.addr.addrMac;
import org.freertr.util.bits;
import org.freertr.util.logger;

/**
 * external interface handler over udp
 *
 * @author matecsaba
 */
public class ifcUdpInt extends ifcThread {

    private DatagramSocket sck;

    private int lprt;

    private int rprt;

    private String rnam;

    private String lnam;

    private InetAddress radr;

    private InetAddress ladr;

    private InetSocketAddress rsad;

    private InetSocketAddress lsad;

    /**
     * set filter
     *
     * @param promisc promiscous mode
     */
    public void setFilter(boolean promisc) {
    }

    /**
     * flap interface
     */
    public void flapped() {
    }

    /**
     * get mtu size
     *
     * @return mtu size
     */
    public int getMTUsize() {
        return 1500;
    }

    /**
     * get bandwidth
     *
     * @return bandwidth
     */
    public long getBandwidth() {
        if (etherEnc) {
            return 100000000;
        }
        return 2000000;
    }

    /**
     * creates new interface
     *
     * @param lname name of local
     * @param lport local port to bind to
     * @param rname name of remote
     * @param rport remote port to connect to
     * @param hwar hardware address
     * @param notEther false if ethernet
     * @param stat process state packets
     */
    public ifcUdpInt(String lname, int lport, String rname, int rport, String hwar, boolean notEther, boolean stat) {
        lprt = lport;
        lnam = lname;
        rprt = rport;
        rnam = rname;
        haveState = stat;
        try {
            radr = InetAddress.getByName(rnam);
            ladr = InetAddress.getByName(lnam);
            rsad = new InetSocketAddress(radr, rprt);
            lsad = new InetSocketAddress(ladr, lprt);
            sck = new DatagramSocket(null);
        } catch (Exception e) {
            logger.exception(e);
        }
        try {
            sck.setReuseAddress(true);
        } catch (Exception e) {
        }
        try {
            sck.bind(lsad);
            sck.connect(rsad);
        } catch (Exception e) {
            logger.exception(e);
        }
        try {
            sck.setReceiveBufferSize(512 * 1024);
        } catch (Exception e) {
        }
        try {
            sck.setSendBufferSize(512 * 1024);
        } catch (Exception e) {
        }
        etherEnc = !notEther;
        if (!notEther) {
            hwaddr = new addrMac();
            if (hwaddr.fromString(hwar)) {
                hwaddr = addrMac.getRandom();
            }
        } else {
            hwaddr = new addrEmpty();
        }
        checkStalled(bits.getTime());
    }

    public void rxtxClose() throws Exception {
        sck.close();
        sck = null;
    }

    public void txOnePack(byte[] buf, int ofs, int len) throws Exception {
        DatagramPacket d = new DatagramPacket(buf, ofs, len);
        sck.send(d);
    }

    public int rxOnePack(byte[] buf, int ofs) throws Exception {
        DatagramPacket d = new DatagramPacket(buf, ofs, buf.length - ofs);
        sck.receive(d);
        return d.getLength();
    }

    public String toString() {
        return "ifc on " + ladr + " " + lprt + " " + radr + " " + rprt;
    }

}
