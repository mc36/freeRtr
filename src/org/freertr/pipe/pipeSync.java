package org.freertr.pipe;

import org.freertr.cry.cryHashFcs16;
import org.freertr.pack.packHolder;
import org.freertr.pack.packRtp;
import org.freertr.util.bits;
import org.freertr.util.logger;

/**
 * synchronous hdlc framer
 *
 * @author matecsaba
 */
public class pipeSync {

    /**
     * size of header
     */
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

class pipeSyncTx implements Runnable {

    private pipeSide upper;

    private packRtp conn;

    private int syncSrc;

    private int seq;

    private int chnPre;

    private int chnUse;

    private int chnApp;

    private packHolder pck = new packHolder(true, true);

    private packHolder resBuf = new packHolder(true, true);

    private int resVal;

    private int resLen;

    private int resOne;

    private int paySiz;

    private int payInt;

    private final static int payMax = 1280;

    public pipeSyncTx(pipeSide pipe, packRtp rtp, int pre, int use, int app) {
        upper = pipe;
        conn = rtp;
        chnPre = pre;
        chnUse = use;
        chnApp = app;
        syncSrc = bits.randomD();
        paySiz = payMax / (pre + use + app);
        payInt = 1000 / (8000 / paySiz);
        paySiz = (paySiz + 1) * use;
        logger.startThread(this);
    }

    public void placeBit(boolean val) {
        if (!val) {
            resLen++;
            return;
        }
        resVal |= 1 << resLen;
        resLen++;
    }

    public void placeBytes() {
        for (; resLen >= 8;) {
            resBuf.putByte(0, resVal);
            resBuf.putSkip(1);
            resBuf.merge2end();
            resLen -= 8;
            resVal = resVal >>> 8;
        }
    }

    public void placeFlag() {
        placeBit(false);
        placeBit(true);
        placeBit(true);
        placeBit(true);
        placeBit(true);
        placeBit(true);
        placeBit(true);
        placeBit(false);
        placeBytes();
        resOne = 0;
    }

    public void encodeBit(boolean val) {
        placeBit(val);
        if (!val) {
            resOne = 0;
            return;
        }
        resOne++;
        if (resOne < 5) {
            return;
        }
        placeBit(false);
        resOne = 0;
    }

    public void encodeByte(int val) {
        for (int i = 0; i < 8; i++) {
            encodeBit(((val >>> i) & 1) != 0);
        }
        placeBytes();
    }

    public void doer() {
        for (;;) {
            bits.sleep(payInt);
            if (conn.isClosed() != 0) {
                upper.setClose();
                break;
            }
            if (upper.isClosed() != 0) {
                conn.setClose();
                break;
            }
            if (resBuf.dataSize() < 1) {
                resBuf.clear();
            }
            for (; resBuf.dataSize() <= paySiz;) {
                pck.clear();
                if (pck.pipeRecv(upper, 0, 8192, 142) < 1) {
                    break;
                }
                int siz = pck.dataSize();
                cryHashFcs16 fcs = new cryHashFcs16();
                fcs.init();
                pck.hashData(fcs, 0, siz);
                for (int i = 0; i < siz; i++) {
                    encodeByte(pck.getByte(i));
                }
                byte[] buf = fcs.finish();
                encodeByte(buf[0]);
                encodeByte(buf[1]);
                placeFlag();
            }
            for (; resBuf.dataSize() <= paySiz;) {
                placeFlag();
            }
            pck.clear();
            byte[] buf = new byte[chnUse];
            for (; pck.dataSize() < payMax;) {
                pck.putFill(0, chnPre, 0x7e);
                pck.putSkip(chnPre);
                resBuf.getCopy(buf, 0, 0, chnUse);
                resBuf.getSkip(chnUse);
                pck.putCopy(buf, 0, 0, chnUse);
                pck.putSkip(chnUse);
                pck.putFill(0, chnApp, 0x7e);
                pck.putSkip(chnApp);
                pck.merge2end();
            }
            pck.msbPutW(0, 0);
            pck.msbPutW(2, seq);
            seq++;
            pck.putSkip(pipeSync.size);
            pck.merge2beg();
            conn.typeTx = 0;
            conn.syncTx = syncSrc;
            conn.sendPack(pck);
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

    private packHolder resBuf = new packHolder(true, true);

    private int resVal;

    private int resLen;

    private int resOne;

    public pipeSyncRx(pipeSide pipe, packRtp rtp, int pre, int use, int app) {
        upper = pipe;
        conn = rtp;
        chnPre = pre;
        chnUse = use;
        chnApp = app;
        logger.startThread(this);
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
