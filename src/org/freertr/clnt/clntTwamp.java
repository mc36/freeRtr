package org.freertr.clnt;

import org.freertr.addr.addrIP;
import org.freertr.cfg.cfgAll;
import org.freertr.ip.ipFwdIface;
import org.freertr.pack.packHolder;
import org.freertr.pack.packNtp;
import org.freertr.pack.packTwamp;
import org.freertr.pipe.pipeLine;
import org.freertr.pipe.pipeSide;
import org.freertr.prt.prtUdp;
import org.freertr.tab.tabAverage;
import org.freertr.util.bits;
import org.freertr.util.logger;
import org.freertr.util.notifier;

/**
 * two way measurement protocol (rfc5357) client
 *
 * @author matecsaba
 */
public class clntTwamp implements Runnable {

    /**
     * create instance
     */
    public clntTwamp() {
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
            pipeSide pip = udp.streamConnect(new pipeLine(65536, true), src, 0, trg, packTwamp.port, "twamp", -1, null, tim2liv, typOsrv);
            if (pip == null) {
                return;
            }
            pip.wait4ready(timeout);
            pip.setTime(timeout);
            packHolder pck = new packHolder(true, true);
            pck.putFill(0, size, datPat);
            pck.putSkip(size);
            pck.merge2end();
            long beg = bits.getTime();
            packTwamp twm = new packTwamp();
            twm.sequence = 1;
            twm.timestmp = packNtp.encode(beg + cfgAll.timeServerOffset);
            twm.errEst = packTwamp.errMag;
            twm.createHeader(pck);
            pck.pipeSend(pip, 0, pck.dataSize(), 2);
            pck.clear();
            pck.pipeRecv(pip, 0, packHolder.maxData, 133);
            beg = bits.getTime() - beg;
            if (twm.parseHeader(pck)) {
                return;
            }
            if (pck.dataSize() != size) {
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
