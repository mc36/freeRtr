package clnt;

import addr.addrIP;
import cfg.cfgAll;
import pipe.pipeSide;
import serv.servGeneric;
import serv.servIrc;
import user.userTerminal;
import util.bits;
import util.logger;

/**
 * internet relay chat (rfc2812) client
 *
 * @author matecsaba
 */
public class clntIrc implements Runnable {

    private String hostName;

    private String chanName;

    private pipeSide pipe;

    private boolean running;

    /**
     * create logger
     *
     * @param hst host where to log
     * @param chan name of channel
     */
    public clntIrc(String hst, String chan) {
        hostName = hst;
        chanName = chan;
        running = hostName != null;;
    }

    public String toString() {
        if (hostName == null) {
            return "";
        }
        return hostName + " " + chanName;
    }

    /**
     * stop logging
     */
    public void logStop() {
        running = false;
        if (pipe != null) {
            pipe.setClose();
        }
        pipe = null;
    }

    /**
     * start logging
     */
    public void logStart() {
        if (!running) {
            return;
        }
        new Thread(this).start();
    }

    /**
     * log one message
     *
     * @param s message to log
     */
    public void logMsg(String s) {
        if (pipe == null) {
            return;
        }
        byte[] buf = str2lin("PRIVMSG " + chanName + " :" + s);
        pipe.nonBlockPut(buf, 0, buf.length);
    }

    public void run() {
        for (;;) {
            if (!running) {
                break;
            }
            try {
                workDoer();
            } catch (Exception e) {
                logger.traceback(e);
            }
            bits.sleep(1000);
        }
    }

    private static byte[] str2lin(String s) {
        byte[] buf = new byte[1];
        buf[0] = 10;
        return bits.byteConcat(s.getBytes(), buf);
    }

    private void workDoer() {
        if (pipe != null) {
            pipe.setClose();
        }
        addrIP trg = userTerminal.justResolv(hostName, 0);
        if (trg == null) {
            return;
        }
        pipe = cfgAll.clntConnect(servGeneric.protoTcp, trg, new servIrc().srvPort());
        if (pipe == null) {
            return;
        }
        pipe.timeout = 120000;
        pipe.lineRx = pipeSide.modTyp.modeCRtryLF;
        pipe.lineTx = pipeSide.modTyp.modeLF;
        pipe.linePut("CAP LS");
        pipe.linePut("NICK " + cfgAll.hostName);
        pipe.linePut("CAP END");
        logger.warn("neighbor " + hostName + " up");
        int cnt = 0;
        for (;;) {
            bits.sleep(1000);
            if (!running) {
                break;
            }
            if (pipe.isClosed() != 0) {
                break;
            }
            int i = pipe.ready2rx();
            if (i > 0) {
                pipe.nonBlockSkip(i);
            }
            cnt = (cnt + 1) % 30;
            if (cnt == 0) {
                pipe.linePut("PING :" + hostName);
            }
        }
        logger.warn("neighbor " + hostName + " down");
    }

}
