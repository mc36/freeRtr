package org.freertr.clnt;

import org.freertr.addr.addrIP;
import org.freertr.ip.ipFwdIface;
import org.freertr.pipe.pipeLine;
import org.freertr.pipe.pipeSide;
import org.freertr.prt.prtUdp;
import org.freertr.serv.servEchoS;
import org.freertr.tab.tabAverage;
import org.freertr.util.bits;
import org.freertr.util.logger;
import org.freertr.util.notifier;

/**
 * echo (rfc862) client
 *
 * @author matecsaba
 */
public class clntEcho implements Runnable {

    /**
     * create instance
     */
    public clntEcho() {
    }

    /**
     * measurement
     */
    public tabAverage meas;

    /**
     * notifier
     */
    public notifier notif;

    /**
     * udp
     */
    public prtUdp udp;

    /**
     * source
     */
    public ipFwdIface src;

    /**
     * target
     */
    public addrIP trg;

    /**
     * timeout
     */
    public int timeout = 1000;

    /**
     * size
     */
    public int size = 64;

    /**
     * ttl
     */
    public int tim2liv = 255;

    /**
     * tos
     */
    public int typOsrv = 0;

    /**
     * data pattern
     */
    public int datPat = 0;

    /**
     * do work
     */
    public void doWork() {
        logger.startThread(this);
    }

    public void run() {
        try {
            pipeSide pip = udp.streamConnect(new pipeLine(65536, true), src, 0, trg, servEchoS.port, "echo", -1, null, tim2liv, typOsrv);
            if (pip == null) {
                return;
            }
            pip.wait4ready(timeout);
            pip.setTime(timeout);
            byte[] buf = new byte[size];
            bits.byteFill(buf, 0, buf.length, datPat);
            long beg = bits.getTime();
            pip.blockingPut(buf, 0, buf.length);
            int len = pip.blockingGet(buf, 0, buf.length);
            pip.setClose();
            beg = bits.getTime() - beg;
            if (len != buf.length) {
                return;
            }
            if (meas != null) {
                meas.addValue((int) beg);
            }
            if (notif != null) {
                notif.wakeup();
            }
        } catch (Exception e) {
            logger.traceback(e);
        }
    }

}
