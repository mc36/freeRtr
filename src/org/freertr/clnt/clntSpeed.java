package org.freertr.clnt;

import org.freertr.addr.addrIP;
import org.freertr.cfg.cfgAll;
import org.freertr.pipe.pipeSide;
import org.freertr.serv.servCharGen;
import org.freertr.serv.servDiscard;
import org.freertr.serv.servGeneric;
import org.freertr.util.bits;
import org.freertr.util.cmds;
import org.freertr.util.logger;

/**
 * speed test client
 *
 * @author matecsaba
 */
public class clntSpeed {

    /**
     * create instance
     */
    public clntSpeed() {
    }

    /**
     * rx pipeline
     */
    protected pipeSide rxp;

    /**
     * tx pipeline
     */
    protected pipeSide txp;

    /**
     * rx counter
     */
    protected int rxc;

    /**
     * tx counter
     */
    protected int txc;

    /**
     * small servers client
     *
     * @param cmd command to use
     */
    public static void smllClnt(cmds cmd) {
        String a = cmd.word();
        addrIP trg = clntDns.justResolv(a, 0);
        if (trg == null) {
            return;
        }
        clntProxy prx = cfgAll.getClntPrx(null);
        if (prx == null) {
            return;
        }
        clntSpeed s = new clntSpeed();
        s.rxp = prx.doConnect(servGeneric.protoTcp, trg, new servCharGen().srvPort(), "speed");
        if (s.rxp == null) {
            return;
        }
        s.txp = prx.doConnect(servGeneric.protoTcp, trg, new servDiscard().srvPort(), "speed");
        if (s.txp == null) {
            s.rxp.setClose();
            return;
        }
        new clntSpeedRx(s).start();
        new clntSpeedTx(s).start();
        cmd.error("       rxbps       txbps");
        for (;;) {
            if (cmd.pipe.isClosed() != 0) {
                break;
            }
            if (cmd.pipe.ready2rx() != 0) {
                break;
            }
            int rxo = s.rxc;
            int txo = s.txc;
            bits.sleep(1000);
            cmd.error(bits.padBeg(bits.toUser((s.rxc - rxo) * 8), 12, " ") + bits.padBeg(bits.toUser((s.txc - txo) * 8), 12, " "));
        }
        s.rxp.setClose();
        s.txp.setClose();
    }

}

class clntSpeedRx implements Runnable {

    private clntSpeed parent;

    public clntSpeedRx(clntSpeed lower) {
        parent = lower;
    }

    public void start() {
        logger.startThread(this);
    }

    public void run() {
        try {
            for (;;) {
                byte[] buf = new byte[1024];
                int i = parent.rxp.blockingGet(buf, 0, buf.length);
                if (i < 0) {
                    break;
                }
                parent.rxc += i;
            }
        } catch (Exception e) {
            logger.traceback(e);
        }
    }

}

class clntSpeedTx implements Runnable {

    private clntSpeed parent;

    public clntSpeedTx(clntSpeed lower) {
        parent = lower;
    }

    public void start() {
        logger.startThread(this);
    }

    public void run() {
        try {
            for (;;) {
                byte[] buf = new byte[1024];
                int i = parent.txp.blockingPut(buf, 0, buf.length);
                if (i < 0) {
                    break;
                }
                parent.txc += i;
            }
        } catch (Exception e) {
            logger.traceback(e);
        }
    }

}
