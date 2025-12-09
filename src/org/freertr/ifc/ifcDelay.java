package org.freertr.ifc;

import org.freertr.pack.packHolder;
import org.freertr.util.bits;
import org.freertr.util.logger;

/**
 * delayed sender
 *
 * @author matecsaba
 */
public class ifcDelay {

    private ifcDelay() {
    }

    /**
     * delayed packet sender
     *
     * @param tim time to wait, zero to send immediately
     * @param ifc lower layer to use
     * @param pck packet to send
     */
    public static void sendPack(int tim, ifcDn ifc, packHolder pck) {
        if (tim < 1) {
            ifc.sendPack(pck);
            return;
        }
        ifcDelayTx d = new ifcDelayTx();
        d.delay = tim;
        d.lower = ifc;
        d.pack = pck.copyBytes(true, true);
        new Thread(d).start();
    }

    /**
     * delayed packet receiver
     *
     * @param tim time to wait, zero to send immediately
     * @param ifc lower layer to use
     * @param pck packet to send
     */
    public static void recvPack(int tim, ifcUp ifc, packHolder pck) {
        if (tim < 1) {
            ifc.recvPack(pck);
            return;
        }
        ifcDelayRx d = new ifcDelayRx();
        d.delay = tim;
        d.lower = ifc;
        d.pack = pck.copyBytes(true, true);
        new Thread(d).start();
    }

}

class ifcDelayTx implements Runnable {

    public ifcDn lower;

    public int delay;

    public packHolder pack;

    public void run() {
        bits.sleep(delay);
        try {
            lower.sendPack(pack);
        } catch (Exception e) {
            logger.traceback(e);
        }
    }

}

class ifcDelayRx implements Runnable {

    public ifcUp lower;

    public int delay;

    public packHolder pack;

    public void run() {
        bits.sleep(delay);
        try {
            lower.recvPack(pack);
        } catch (Exception e) {
            logger.traceback(e);
        }
    }

}
