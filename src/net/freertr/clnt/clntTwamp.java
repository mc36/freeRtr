package net.freertr.clnt;

import net.freertr.addr.addrIP;
import net.freertr.cfg.cfgAll;
import net.freertr.ip.ipFwdIface;
import net.freertr.pack.packHolder;
import net.freertr.pack.packNtp;
import net.freertr.pack.packTwamp;
import net.freertr.pipe.pipeLine;
import net.freertr.pipe.pipeSide;
import net.freertr.prt.prtUdp;
import net.freertr.tab.tabAverage;
import net.freertr.util.bits;
import net.freertr.util.logger;

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
     * data pattern
     */
    public int datPat = 0;

    /**
     * do work
     */
    public void doWork() {
        new Thread(this).start();
    }

    public void run() {
        try {
            pipeSide pip = udp.streamConnect(new pipeLine(65536, true), src, 0, trg, packTwamp.port, "twamp", null, tim2liv);
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
            meas.addValue((int) beg);
        } catch (Exception e) {
            logger.traceback(e);
        }
    }

}
