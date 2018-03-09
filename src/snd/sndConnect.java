package snd;

import pack.packHolder;
import pack.packRtp;

/**
 * connect sound sources
 *
 * @author matecsaba
 */
public class sndConnect {

    private final packRtp side1;

    private final packRtp side2;

    /**
     * create connection
     *
     * @param s1 side one
     * @param s2 side two
     */
    public sndConnect(packRtp s1, packRtp s2) {
        side1 = s1;
        side2 = s2;
        new sndConnectDoer(s1, s2);
        new sndConnectDoer(s2, s1);
    }

    /**
     * close connection
     */
    public void setClose() {
        side1.setClose();
        side2.setClose();
    }

    /**
     * test if the any side was closed
     *
     * @return status
     */
    public int isClosed() {
        return side1.isClosed() | side2.isClosed();
    }

}

class sndConnectDoer implements Runnable {

    private packRtp rx;

    private packRtp tx;

    public sndConnectDoer(packRtp s1, packRtp s2) {
        rx = s1;
        tx = s2;
        new Thread(this).start();
    }

    public void run() {
        packHolder pck = new packHolder(true, true);
        for (;;) {
            if (rx.isClosed() != 0) {
                break;
            }
            if (tx.isClosed() != 0) {
                break;
            }
            pck.clear();
            if (rx.recvPack(pck, true) < 1) {
                break;
            }
            tx.sendPack(pck);
        }
        rx.setClose();
        tx.setClose();
    }

}
