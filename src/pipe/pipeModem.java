package pipe;

import java.util.Timer;
import java.util.TimerTask;
import pack.packHolder;
import pack.packRtp;
import snd.sndCodec;
import snd.sndFsk;
import util.bits;
import util.logger;

/**
 * modem pipe
 *
 * @author matecsaba
 */
public class pipeModem {

    /**
     * answer a call
     *
     * @param pipe pipeline to use
     * @param codec codec to use
     * @param rtp voice connection
     */
    public static void answer(pipeSide pipe, sndCodec codec, packRtp rtp) {
        new pipeModemTx(pipe, codec, rtp, sndFsk.ituV21carrier[1]);
        new pipeModemRx(pipe, codec, rtp, sndFsk.ituV21carrier[0]);
    }

    /**
     * originate a call
     *
     * @param pipe pipeline to use
     * @param codec codec to use
     * @param rtp voice connection
     */
    public static void originate(pipeSide pipe, sndCodec codec, packRtp rtp) {
        new pipeModemTx(pipe, codec, rtp, sndFsk.ituV21carrier[0]);
        new pipeModemRx(pipe, codec, rtp, sndFsk.ituV21carrier[1]);
    }

}

class pipeModemTx extends TimerTask {

    private pipeSide pipe;

    private sndCodec codec;

    private packRtp rtp;

    private Timer keepTimer = new Timer();

    private sndFsk modem = new sndFsk();

    private packHolder queue = new packHolder(true, true);

    private int syncSrc;

    private packHolder pck = new packHolder(true, true);

    private final static int paySiz = 160;

    private final static int payInt = 1000 / (8000 / paySiz);

    public pipeModemTx(pipeSide line, sndCodec sound, packRtp conn, int freq) {
        pipe = line;
        codec = sound;
        rtp = conn;
        syncSrc = bits.randomD();
        modem.carrier = freq;
        modem.sampDat = new int[1024];
        keepTimer.scheduleAtFixedRate(this, 10, payInt);
    }

    public void doer() {
        if (rtp.isClosed() != 0) {
            keepTimer.cancel();
            pipe.setClose();
            return;
        }
        if (pipe.isClosed() != 0) {
            keepTimer.cancel();
            rtp.setClose();
            return;
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
        pck.RTPtyp = codec.getRTPtype();
        pck.RTPsrc = syncSrc;
        rtp.sendPack(pck);
    }

    public void run() {
        try {
            doer();
        } catch (Exception e) {
            logger.traceback(e);
            rtp.setClose();
        }
    }

}

class pipeModemRx implements Runnable {

    private pipeSide pipe;

    private sndCodec codec;

    private packRtp rtp;

    private sndFsk modem = new sndFsk();

    public pipeModemRx(pipeSide line, sndCodec sound, packRtp conn, int freq) {
        pipe = line;
        codec = sound;
        rtp = conn;
        modem.carrier = freq;
        modem.sampDat = new int[1024];
        new Thread(this).start();
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
            if (pck.RTPtyp != codec.getRTPtype()) {
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
