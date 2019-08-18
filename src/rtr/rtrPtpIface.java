package rtr;

import addr.addrIP;
import cfg.cfgAll;
import ip.ipFwdIface;
import pack.packHolder;
import pack.packPtp;
import prt.prtGenConn;
import prt.prtServP;
import prt.prtUdp;
import util.bits;
import util.debugger;
import util.logger;

/**
 * precision time protocol (ieee1588) interface
 *
 * @author matecsaba
 */
public class rtrPtpIface implements Runnable, prtServP {

    /**
     * udp handler
     */
    protected prtUdp udp;

    /**
     * ip interface
     */
    protected ipFwdIface ifc;

    /**
     * receive time
     */
    public boolean receive;

    private boolean need2run = false;

    private prtGenConn connS;

    private prtGenConn connF;

    /**
     * create one instance
     *
     * @param dgrm udp handler
     * @param iface the ip interface to work on
     */
    public rtrPtpIface(prtUdp dgrm, ipFwdIface iface) {
        udp = dgrm;
        ifc = iface;
    }

    public String toString() {
        return "ptp on " + ifc;
    }

    /**
     * unregister from udp
     */
    public void unregister2udp() {
        if (debugger.rtrPtpEvnt) {
            logger.debug("stopping on " + ifc);
        }
        udp.listenStop(ifc, packPtp.portS, null, 0, 0);
        udp.listenStop(ifc, packPtp.portF, null, 0, 0);
        if (connS != null) {
            connS.setClosing();
        }
        if (connF != null) {
            connF.setClosing();
        }
        need2run = false;
    }

    /**
     * register to udp
     */
    public void register2udp() {
        if (debugger.rtrPtpEvnt) {
            logger.debug("starting on " + ifc);
        }
        udp.packetListen(this, ifc, packPtp.portS, null, 0, 0, "ptp", null, -1);
        udp.packetListen(this, ifc, packPtp.portF, null, 0, 0, "ptp", null, -1);
        addrIP peer = new addrIP();
        packPtp.setIP(ifc.addr.isIPv4(), peer);
        connS = udp.packetConnect(this, ifc, packPtp.portS, peer, packPtp.portS, "ptp", null, -1);
        connF = udp.packetConnect(this, ifc, packPtp.portF, peer, packPtp.portF, "ptp", null, -1);
        need2run = true;
        new Thread(this).start();
    }

    /**
     * close interface
     *
     * @param ifc interface
     */
    public void closedInterface(ipFwdIface ifc) {
    }

    /**
     * accept connection
     *
     * @param id connection
     * @return false if success, true if error
     */
    public boolean datagramAccept(prtGenConn id) {
        if (debugger.rtrPtpEvnt) {
            logger.debug("starting with " + id.peerAddr);
        }
        return false;
    }

    /**
     * connection ready
     *
     * @param id connection
     */
    public void datagramReady(prtGenConn id) {
    }

    /**
     * stop connection
     *
     * @param id connection
     */
    public void datagramClosed(prtGenConn id) {
        if (debugger.rtrPtpEvnt) {
            logger.debug("stopping with " + id.peerAddr);
        }
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
     * @return false if success, true if error
     */
    public boolean datagramRecv(prtGenConn id, packHolder pck) {
        if (debugger.rtrPtpTraf) {
            logger.debug("got from " + id.peerAddr);
        }
        if (!receive) {
            return false;
        }
        packPtp ptp = new packPtp();
        if (ptp.parsePacket(pck)) {
            return false;
        }
        if (debugger.rtrPtpTraf) {
            logger.debug("offsets: old=" + cfgAll.timeServerOffset + " new=" + ptp.offset + " diff=" + (cfgAll.timeServerOffset - ptp.offset));
        }
        long diff = cfgAll.timeServerOffset - ptp.offset;
        if (diff < 0) {
            diff = -diff;
        }
        if (diff > 1000) {
            logger.info("setting clock to " + bits.time2str(cfgAll.timeZoneName, bits.getTime() + ptp.offset, 3));
        }
        cfgAll.timeServerOffset = ptp.offset;
        return false;
    }

    public void run() {
        int clk = bits.randomD();
        int seq = 0;
        for (;;) {
            if (!need2run) {
                return;
            }
            bits.sleep(1000);
            seq++;
            if (debugger.rtrPtpTraf) {
                logger.debug("sending packet");
            }
            packHolder pck = new packHolder(true, true);
            packPtp ptp = new packPtp();
            ptp.domain = 0;
            ptp.port = 1;
            ptp.clock = clk;
            ptp.sequence = seq;
            ptp.offset = cfgAll.timeServerOffset;
            ptp.createSync(pck);
            connS.send2net(pck);
            pck.clear();
            ptp.createFollow(pck);
            connF.send2net(pck);
        }
    }

}
