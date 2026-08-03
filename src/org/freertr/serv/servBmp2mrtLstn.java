package org.freertr.serv;

import org.freertr.addr.addrIP;
import org.freertr.pack.packHolder;
import org.freertr.pipe.pipeSide;
import org.freertr.prt.prtGenConn;
import org.freertr.util.bits;
import org.freertr.util.logger;

/**
 * bmp server connection
 *
 * @author matecsaba
 */
public abstract class servBmp2mrtLstn implements Runnable, Comparable<servBmp2mrtLstn> {

    /**
     * connection to use
     */
    protected final prtGenConn conn;

    /**
     * pipeline to use
     */
    protected final pipeSide pipe;

    /**
     * server to use
     */
    protected final servBmp2mrt lower;

    /**
     * create instance
     *
     * @param pip pipe to use
     * @param prnt parent to use
     * @param id connection to use
     */
    public servBmp2mrtLstn(pipeSide pip, servBmp2mrt prnt, prtGenConn id) {
        pipe = pip;
        lower = prnt;
        conn = id;
        logger.startThread(this);
    }

    public int compareTo(servBmp2mrtLstn o) {
        return conn.compareTo(o.conn);
    }

    /**
     * do start
     */
    protected abstract void doStart();

    /**
     * got a message
     *
     * @param as as number
     * @param from connection source
     * @param peer remote peer address
     * @param typ message type
     * @param pck message body
     */
    protected abstract void doDump(int as, addrIP from, addrIP peer, int typ, packHolder pck);

    public void run() {
        logger.warn("neighbor " + conn.peerAddr + " up");
        try {
            doStart();
        } catch (Exception e) {
            logger.traceback(e);
        }
        lower.lstns.put(this);
        for (;;) {
            if (pipe.isClosed() != 0) {
                break;
            }
            bits.sleep(1000);
        }
        lower.lstns.del(this);
        pipe.setClose();
        logger.error("neighbor " + conn.peerAddr + " down");
    }

}
