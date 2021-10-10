package net.freertr.ifc;

import net.freertr.pack.packHolder;
import net.freertr.util.bits;
import net.freertr.util.logger;

/**
 * delayed sender
 *
 * @author matecsaba
 */
public class ifcDelay implements Runnable {

    /**
     * create instance
     */
    public ifcDelay() {
    }

    private ifcDn lower;

    private int delay;

    private packHolder pack;

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
        ifcDelay d = new ifcDelay();
        d.delay = tim;
        d.lower = ifc;
        d.pack = pck.copyBytes(true, true);
        new Thread(d).start();
    }

    public void run() {
        bits.sleep(delay);
        try {
            lower.sendPack(pack);
        } catch (Exception e) {
            logger.traceback(e);
        }
    }

}
