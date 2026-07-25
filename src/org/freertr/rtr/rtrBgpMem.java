package org.freertr.rtr;

import org.freertr.pack.packHolder;
import org.freertr.util.bits;

/**
 * bgp last message memory
 *
 * @author matecsaba
 */
public class rtrBgpMem {

    /**
     * data
     */
    protected final byte data[][];

    /**
     * type
     */
    protected final int type[];

    /**
     * time
     */
    protected final long time[];

    /**
     * direction
     */
    protected final boolean[] sent;

    /**
     * next packet number
     */
    protected int next;

    /**
     * create instance
     *
     * @param s size
     */
    public rtrBgpMem(int s) {
        data = new byte[s][0];
        type = new int[s];
        time = new long[s];
        sent = new boolean[s];
    }

    /**
     * got update
     *
     * @param dir direction: false=rx, true=tx
     * @param typ type
     * @param dat data bytes
     */
    public synchronized void gotMessage(boolean dir, int typ, byte[] dat) {
        data[next] = dat;
        type[next] = typ;
        sent[next] = dir;
        time[next] = bits.getTime();
        next = (next + 1) % sent.length;
    }

}
