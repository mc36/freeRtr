package ifc;

import addr.addrEmpty;
import addr.addrMac;
import java.net.DatagramPacket;
import java.net.DatagramSocket;
import java.net.InetAddress;
import util.logger;

/**
 * external interface handler over udp
 *
 * @author matecsaba
 */
public class ifcUdpInt extends ifcThread {

    private DatagramSocket sck;

    private int lprt;

    private int rprt;

    private InetAddress radr;

    private InetAddress ladr;

    private String rnam;

    private String lnam;

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
            sck = new DatagramSocket(lprt);
        } catch (Exception e) {
            logger.exception(e);
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
        checkStalled();
    }

    public void rxtxClose() throws Exception {
        sck.close();
        sck = null;
    }

    public void txOnePack(byte[] buf, int ofs, int len) throws Exception {
        DatagramPacket d = new DatagramPacket(buf, ofs, len);
        d.setAddress(radr);
        d.setPort(rprt);
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
