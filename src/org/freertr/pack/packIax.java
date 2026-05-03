package org.freertr.pack;

import org.freertr.pipe.pipeSide;
import org.freertr.util.logger;

/**
 * inter asterisk exchange (rfc5456) packet
 *
 * @author matecsaba
 */
public class packIax {

    /**
     * port number
     */
    public final static int port = 4569;

    private final pipeSide pipe;

    /**
     * create packet handler
     *
     * @param lower lower layer to use
     */
    public packIax(pipeSide lower) {
        pipe = lower;
        clear();
    }

    /**
     * clear packet data
     */
    public void clear() {

    

    //////
    }

    /**
     * bytes ready to receive
     *
     * @return bytes
     */
    public int ready2rx() {
        return pipe.ready2rx();
    }

    /**
     * check if closed
     *
     * @return status
     */
    public int isClosed() {
        return pipe.isClosed();
    }

    /**
     * receive one packet
     *
     * @param pck buffer to use
     * @return bytes received
     */
    public int recvPack(packHolder pck) {
        clear();
        int i = pck.pipeRecv(pipe, 0, -1, 143);
        if (i < 1) {
            return i;
        }
        i = pck.msbGetW(0);

        return pck.dataSize();
    }

    /**
     * dump the message
     *
     * @param dir direction
     */
    public void dump(String dir) {
        logger.debug(dir + ": ");
    }

}
