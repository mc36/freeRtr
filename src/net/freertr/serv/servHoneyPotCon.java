package net.freertr.serv;

import java.util.List;
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

    public servHoneyPotCon(servHoneyPot parent, pipeSide conn, addrIP peer, int prt) {
        lower = parent;
        pipe = conn;
        addr = peer;
        port = prt;
        new Thread(this).start();
    }

    public void run() {
        pipe.setReady();
        logger.info("honeypot hit from " + addr + " " + port);
        servHoneyPotWrk w = new servHoneyPotWrk(lower.ipInfo, pipe, addr, port);
        w.doWork(true);
        w.putResult(true);
        pipe.setClose();
    }

}
