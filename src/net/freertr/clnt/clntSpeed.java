package net.freertr.clnt;

import net.freertr.addr.addrIP;
import net.freertr.cfg.cfgAll;
import net.freertr.pipe.pipeProgress;
import net.freertr.pipe.pipeSide;
import net.freertr.serv.servCharGen;
import net.freertr.serv.servDiscard;
import net.freertr.serv.servGeneric;
import net.freertr.user.userTerminal;
import net.freertr.util.bits;
import net.freertr.util.cmds;

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
        userTerminal t = new userTerminal(new pipeProgress(cmd.pipe));
        String a = cmd.word();
        addrIP trg = userTerminal.justResolv(a, 0);
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
        new Thread(new clntSpeedRx(s)).start();
        new Thread(new clntSpeedTx(s)).start();
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

    public void run() {
        for (;;) {
            byte[] buf = new byte[1024];
            int i = parent.rxp.blockingGet(buf, 0, buf.length);
            if (i < 0) {
                break;
            }
            parent.rxc += i;
        }
    }

}

class clntSpeedTx implements Runnable {

    private clntSpeed parent;

    public clntSpeedTx(clntSpeed lower) {
        parent = lower;
    }

    public void run() {
        for (;;) {
            byte[] buf = new byte[1024];
            int i = parent.txp.blockingPut(buf, 0, buf.length);
            if (i < 0) {
                break;
            }
            parent.txc += i;
        }
    }

}
