package clnt;

import addr.addrIP;
import pack.packNetflow;
import pipe.pipeSide;
import serv.servGeneric;
import tab.tabSession;
import util.bits;
import util.logger;

/**
 * netflow (rfc3954) client
 *
 * @author matecsaba
 */
public class clntNetflow implements Runnable {

    /**
     * sessions
     */
    public final tabSession session;

    /**
     * proxy
     */
    public clntProxy proxy;

    /**
     * target address
     */
    public addrIP trgAddr;

    /**
     * target port
     */
    public int trgPort;

    private boolean need2run;

    /**
     * create client
     *
     * @param ver ip version
     */
    public clntNetflow(int ver) {
        session = new tabSession();
        session.logBefore = false;
        session.logAfter = false;
        session.unidir = false;
        session.ipv4 = ver == 4;
        if (session.ipv4) {
            session.limit = packNetflow.flow4;
        } else {
            session.limit = packNetflow.flow6;
        }
        session.source = bits.randomD();
    }

    /**
     * start timer
     */
    public void startTimer() {
        need2run = true;
        new Thread(this).start();
    }

    /**
     * stop timer
     */
    public void stopTimer() {
        need2run = false;
    }

    public String toString() {
        if (proxy == null) {
            return "collect";
        } else {
            return "export " + proxy + " " + trgAddr + " " + trgPort;
        }
    }

    public void run() {
        pipeSide pipe = null;
        for (;;) {
            if (!need2run) {
                return;
            }
            bits.sleep(session.timeout / 4);
            try {
                boolean b = pipe == null;
                if (!b) {
                    b = pipe.isClosed() != 0;
                }
                b &= proxy != null;
                if (b) {
                    pipe = proxy.doConnect(servGeneric.protoUdp, trgAddr, trgPort, "netflow");
                    if (pipe != null) {
                        pipe.timeout = 120000;
                    }
                }
                session.doNetflow(pipe);
            } catch (Exception e) {
                logger.traceback(e);
            }
        }
    }

}
