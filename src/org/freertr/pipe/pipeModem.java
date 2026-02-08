package org.freertr.pipe;

import org.freertr.pack.packHolder;
import org.freertr.pack.packRtp;
import org.freertr.enc.encCodec;
import org.freertr.enc.encFsk;
import org.freertr.util.bits;
import org.freertr.util.logger;

/**
 * modem pipe
 *
 * @author matecsaba
 */
public class pipeModem {

    private pipeModem() {
    }

    /**
     * answer a call
     *
     * @param pipe pipeline to use
     * @param codec codec to use
     * @param rtp voice connection
     */
    public static void answer(pipeSide pipe, encCodec codec, packRtp rtp) {
        new pipeModemTx(pipe, codec, rtp, encFsk.ituV21carrier[1]);
        new pipeModemRx(pipe, codec, rtp, encFsk.ituV21carrier[0]);
    }

    /**
     * originate a call
     *
     * @param pipe pipeline to use
     * @param codec codec to use
     * @param rtp voice connection
     */
    public static void originate(pipeSide pipe, encCodec codec, packRtp rtp) {
        new pipeModemTx(pipe, codec, rtp, encFsk.ituV21carrier[0]);
        new pipeModemRx(pipe, codec, rtp, encFsk.ituV21carrier[1]);
    }

}

class pipeModemTx implements Runnable {

    private pipeSide pipe;

    private encCodec codec;

    private packRtp rtp;

    private encFsk modem = new encFsk();

    private packHolder queue = new packHolder(true, true);

    private int syncSrc;

    private packHolder pck = new packHolder(true, true);

    private final static int paySiz = 160;

    private final static int payInt = 1000 / (8000 / paySiz);

    public pipeModemTx(pipeSide line, encCodec sound, packRtp conn, int freq) {
        pipe = line;
        codec = sound;
        rtp = conn;
        syncSrc = bits.randomD();
        modem.carrier = freq;
        modem.sampDat = new int[1024];
        logger.startThread(this);
    }

    public void doer() {
        for (;;) {
            bits.sleep(payInt);
            if (rtp.isClosed() != 0) {
                pipe.setClose();
                break;
            }
            if (pipe.isClosed() != 0) {
                rtp.setClose();
                break;
            }
            for (; queue.dataSize() < paySiz;) {
                modem.sampPos = 0;
                byte[] buf = new byte[1];
                int i = pipe.nonBlockGet(buf, 0, buf.length);
                if (i != buf.length) {
                    modem.encodeSilence();
                } else {
                    modem.encodeByte(buf[0] & 0xff);
                }
                byte[] res = codec.encodeBuf(modem.sampDat, 0, modem.sampPos);
                queue.putCopy(res, 0, 0, res.length);
                queue.putSkip(res.length);
                queue.merge2end();
            }
            byte[] buf = new byte[paySiz];
            queue.getCopy(buf, 0, 0, buf.length);
            queue.getSkip(buf.length);
            pck.clear();
            pck.putCopy(buf, 0, 0, buf.length);
            pck.putSkip(buf.length);
            rtp.typeTx = codec.getRTPtype();
            rtp.syncTx = syncSrc;
            rtp.sendPack(pck);
        }
    }

    public void run() {
        try {
            doer();
        } catch (Exception e) {
            logger.traceback(e);
            pipe.setClose();
            rtp.setClose();
        }
    }

}

class pipeModemRx implements Runnable {

    private pipeSide pipe;

    private encCodec codec;

    private packRtp rtp;

    private encFsk modem = new encFsk();

    public pipeModemRx(pipeSide line, encCodec sound, packRtp conn, int freq) {
        pipe = line;
        codec = sound;
        rtp = conn;
        modem.carrier = freq;
        modem.sampDat = new int[1024];
        logger.startThread(this);
    }

    public void doer() {
        packHolder pck = new packHolder(true, true);
        for (;;) {
            if (rtp.isClosed() != 0) {
                return;
            }
            if (pipe.isClosed() != 0) {
                return;
            }
            if (rtp.recvPack(pck, true) < 1) {
                return;
            }
            if (rtp.typeRx != codec.getRTPtype()) {
                continue;
            }
            modem.sampAdd(codec.decodeBuf(pck.getCopy()));
            for (;;) {
                modem.findStart();
                modem.sampDel(modem.sampPos);
                if (modem.sampSiz < modem.decodeMin()) {
                    break;
                }
                int i = modem.decodeByte();
                byte[] buf = new byte[1];
                buf[0] = (byte) i;
                pipe.nonBlockPut(buf, 0, buf.length);
            }
        }
    }

    public void run() {
        try {
            doer();
        } catch (Exception e) {
            logger.traceback(e);
        }
        pipe.setClose();
        rtp.setClose();
    }

}
