package org.freertr.clnt;

import org.freertr.pack.packXotPad;
import org.freertr.pipe.pipeLine;
import org.freertr.pipe.pipeSide;
import org.freertr.util.logger;

/**
 * x25 over tcp (rfc1613) client
 *
 * @author matecsaba
 */
public class clntXotPad implements Runnable {

    /**
     * xot pad handler
     */
    protected final packXotPad conn;

    /**
     * pipeline
     */
    protected final pipeLine pl;

    /**
     * xot pad pipe
     */
    protected final pipeSide psx;

    /**
     * client pipe
     */
    protected final pipeSide psc;

    /**
     * create instance
     *
     * @param pipe pipeline to use
     */
    public clntXotPad(pipeSide pipe) {
        conn = new packXotPad(pipe);
        pl = new pipeLine(32768, false);
        psx = pl.getSide();
        psc = pl.getSide();
        psx.setReady();
        psc.setReady();
    }

    /**
     * start work
     *
     * @param called called number
     * @param calling calling number
     * @return true on error, false on success
     */
    public boolean startWork(String called, String calling) {
        conn.sendPack(conn.createCallReq(called, calling));
        if (conn.parseCallAcc(conn.recvPack())) {
            return true;
        }
        new Thread(this).start();
        new clntXotPadTx(this).startWork();
        return false;
    }

    /**
     * get pipeline
     *
     * @return pipe
     */
    public pipeSide getPipe() {
        return psc;
    }

    /**
     * stop work
     */
    public void stopWork() {
        conn.setClose();
        psc.setClose();
        psx.setClose();
        pl.setClose();
    }

    public void run() {
        try {
            conn.doerRx(psx);
        } catch (Exception e) {
            logger.traceback(e);
        }
        stopWork();
    }

}

class clntXotPadTx implements Runnable {

    private final clntXotPad lower;

    public clntXotPadTx(clntXotPad parent) {
        lower = parent;
    }

    public void run() {
        try {
            lower.conn.doerTx(lower.psx);
        } catch (Exception e) {
            logger.traceback(e);
        }
        lower.stopWork();
    }

    public void startWork() {
        new Thread(this).start();
    }

}
