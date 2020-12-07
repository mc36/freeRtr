package pipe;

import cry.cryHashFcs16;
import java.util.Timer;
import java.util.TimerTask;
import pack.packHolder;
import pack.packRtp;
import util.bits;
import util.logger;

/**
 * synchronous hdlc framer
 *
 * @author matecsaba
 */
public class pipeSync {

    public final static int size = 4;

    /**
     * handle a framer
     *
     * @param pipe pipeline to use
     * @param rtp tdm connection
     * @param pre channels to prepend
     * @param use channels to use
     * @param app channels to append
     */
    public pipeSync(pipeSide pipe, packRtp rtp, int pre, int use, int app) {
        new pipeSyncTx(pipe, rtp, pre, use, app);
        new pipeSyncRx(pipe, rtp, pre, use, app);
    }

}

class pipeSyncTx extends TimerTask {

    private pipeSide upper;

    private packRtp conn;

    private Timer keepTimer = new Timer();

    private int chnPre;

    private int chnUse;

    private int chnApp;

    public pipeSyncTx(pipeSide pipe, packRtp rtp, int pre, int use, int app) {
        upper = pipe;
        conn = rtp;
        chnPre = pre;
        chnUse = use;
        chnApp = app;
        int paySiz = 1280 / (pre + use + app);
        int payInt = 1000 / (8000 / paySiz);
        keepTimer.scheduleAtFixedRate(this, 10, payInt);
    }

    public void doer() {
        if (conn.isClosed() != 0) {
            keepTimer.cancel();
            upper.setClose();
            return;
        }
        if (upper.isClosed() != 0) {
            keepTimer.cancel();
            conn.setClose();
            return;
        }

    }

    public void run() {
        try {
            doer();
        } catch (Exception e) {
            logger.traceback(e);
            upper.setClose();
            conn.setClose();
        }
    }

}

class pipeSyncRx implements Runnable {

    private pipeSide upper;

    private packRtp conn;

    private int chnPre;

    private int chnUse;

    private int chnApp;

    private packHolder resBuf;

    private int resVal;

    private int resLen;

    private int resOne;

    public pipeSyncRx(pipeSide pipe, packRtp rtp, int pre, int use, int app) {
        upper = pipe;
        conn = rtp;
        chnPre = pre;
        chnUse = use;
        chnApp = app;
        resBuf = new packHolder(true, true);
        new Thread(this).start();
    }

    public void decodeBit(boolean val) {
        if (val) {
            if (resOne > 5) {
                return;
            }
            resVal |= 1 << resLen;
            resLen++;
            resOne++;
            return;
        }
        if (resOne < 5) {
            resOne = 0;
            resLen++;
            return;
        }
        if (resOne == 5) {
            resOne = 0;
            return;
        }
        resOne = 0;
        resLen -= 7;
        int siz = resBuf.dataSize();
        if (resLen < 0) {
            siz--;
            if (siz < 0) {
                siz = 0;
            }
            resLen = 0;
        } else {
            resVal &= (1 << resLen) - 1;
        }
        siz -= 2;
        if (siz < 1) {
            resBuf.clear();
            return;
        }
        cryHashFcs16 fcsH = new cryHashFcs16();
        fcsH.init();
        resBuf.hashData(fcsH, 0, siz);
        byte[] fcsB = fcsH.finish();
        if (resBuf.msbGetW(siz) != bits.msbGetW(fcsB, 0)) {
            resBuf.clear();
            return;
        }
        resBuf.setDataSize(siz);
        resBuf.pipeSend(upper, 0, resBuf.dataSize(), 1);
        resBuf.clear();
    }

    public void decodeByte(int val) {
        for (int i = 0; i < 8; i++) {
            decodeBit(((val >>> i) & 1) != 0);
        }
        if (resLen < 8) {
            return;
        }
        resBuf.putByte(0, resVal);
        resBuf.putSkip(1);
        resBuf.merge2end();
        resLen -= 8;
        resVal = resVal >>> 8;
        if (resBuf.dataSize() < (packHolder.maxData - 16)) {
            return;
        }
        resBuf.clear();
    }

    public void doer() {
        packHolder pck = new packHolder(true, true);
        for (;;) {
            if (conn.isClosed() != 0) {
                return;
            }
            if (upper.isClosed() != 0) {
                return;
            }
            if (conn.recvPack(pck, true) < 1) {
                return;
            }
            pck.getSkip(pipeSync.size);
            for (;;) {
                pck.getSkip(chnPre);
                if (pck.dataSize() < chnUse) {
                    break;
                }
                for (int i = 0; i < chnUse; i++) {
                    decodeByte(pck.getByte(i));
                }
                pck.getSkip(chnUse);
                pck.getSkip(chnApp);
            }
        }
    }

    public void run() {
        try {
            doer();
        } catch (Exception e) {
            logger.traceback(e);
        }
        upper.setClose();
        conn.setClose();
    }

}
