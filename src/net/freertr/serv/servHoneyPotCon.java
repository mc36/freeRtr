package net.freertr.serv;

import net.freertr.addr.addrIP;
import net.freertr.pipe.pipeSide;
import net.freertr.util.logger;

/**
 * honeypot connection
 *
 * @author matecsaba
 */
public class servHoneyPotCon implements Runnable {
    
    private final servHoneyPot lower;
    
    private final pipeSide pipe;
    
    private final addrIP addr;
    
    private final int port;

    /**
     * create one connection
     *
     * @param parent lower
     * @param conn pipe
     * @param peer address
     * @param prt port
     */
    public servHoneyPotCon(servHoneyPot parent, pipeSide conn, addrIP peer, int prt) {
        lower = parent;
        pipe = conn;
        addr = peer;
        port = prt;
    }

    /**
     * do startup
     */
    public void doStart() {
        new Thread(this).start();
    }
    
    public void run() {
        try {
            pipe.setReady();
            logger.info("honeypot hit from " + addr + " " + port);
            servHoneyPotWrk w = new servHoneyPotWrk(lower.ipInfo, pipe, addr, port);
            w.doHttpRead();
            w.doWork();
            w.doHttpWrite();
            w.putResult(true);
            w.doHttpFinish();
            pipe.setClose();
        } catch (Exception e) {
            logger.traceback(e, addr + " " + port);
        }
    }
    
}
