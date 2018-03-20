package snd;

import java.util.ArrayList;
import java.util.List;
import java.util.Timer;
import java.util.TimerTask;
import pack.packHolder;
import pack.packRtp;
import util.bits;
import util.logger;

/**
 * wave file format
 *
 * @author matecsaba
 */
public class sndWave {

    /**
     * header size
     */
    public final static int size = 44;

    private final sndCodec codr;

    private final packRtp strm;

    /**
     * state: 1=running, 2=need2stop, 4=stopped
     */
    protected int state;

    /**
     * detected codes
     */
    protected String dtmf;

    /**
     * buffer
     */
    public byte[] buf;

    private sndDtmf detect;

    /**
     * make wave handler
     *
     * @param codec codec to use
     * @param rtp voice connection
     */
    public sndWave(sndCodec codec, packRtp rtp) {
        codr = codec;
        strm = rtp;
        state = 0;
        buf = new byte[0];
        dtmf = "";
        detect = new sndDtmf();
        detect.sampDat = new int[detect.sampRate];
    }

    /**
     * stop work
     */
    public void stopWork() {
        state |= 2;
    }

    /**
     * check if stopped
     *
     * @return true if yes, false if still running
     */
    public boolean isStopped() {
        return (state & 4) != 0;
    }

    /**
     * wait until stopped
     */
    public void wait4stop() {
        for (;;) {
            if (isStopped()) {
                break;
            }
            bits.sleep(100);
        }
    }

    /**
     * get dtmf codes
     *
     * @return codes if any
     */
    public String getDtmf() {
        String a = dtmf;
        dtmf = "";
        return a;
    }

    /**
     * start playback
     */
    public void startPlay() {
        new sndWavePlay(this, codr, strm);
    }

    /**
     * start record
     */
    public void startRecord() {
        new sndWaveRec(this, codr, strm);
    }

    /**
     * start dtmf
     */
    public void startDtmf() {
        new sndWaveDtmf(this, codr, strm);
    }

    /**
     * do dtmf wor
     *
     * @param buf buffer to process
     */
    protected void doDtmf(byte[] buf) {
        detect.sampAdd(codr.decodeBuf(buf));
        dtmf += detect.getCode();
    }

    /**
     * make header
     *
     * @param buf buffer to use
     * @param codec codec value
     */
    public static void makeHeader(byte[] buf, int codec) {
        bits.msbPutD(buf, 0, 0x52494646);
        bits.lsbPutD(buf, 4, buf.length - 8);
        bits.msbPutD(buf, 8, 0x57415645);
        bits.msbPutD(buf, 12, 0x666D7420);
        bits.lsbPutD(buf, 16, 0x10);
        bits.lsbPutW(buf, 20, codec);
        bits.lsbPutW(buf, 22, 1); // channels
        bits.lsbPutD(buf, 24, 8000); // sample rate
        bits.lsbPutD(buf, 28, 8000); // bytes / sec
        bits.lsbPutW(buf, 32, 1); // align
        bits.lsbPutW(buf, 34, 8); // bits / sample
        bits.msbPutD(buf, 36, 0x64617461);
        bits.lsbPutD(buf, 40, buf.length - 44);
    }

}

class sndWavePlay extends TimerTask {

    private sndWave lower;

    private Timer keepTimer = new Timer();

    private int syncSrc;

    private packHolder pck;

    private sndCodec codec;

    private packRtp rtp;

    private int pos;

    private final static int paySiz = 160;

    private final static int payInt = 1000 / (8000 / paySiz);

    public sndWavePlay(sndWave parent, sndCodec codr, packRtp strm) {
        lower = parent;
        rtp = strm;
        codec = codr;
        pck = new packHolder(true, true);
        syncSrc = bits.randomD();
        pos = sndWave.size;
        lower.state = 1;
        keepTimer.scheduleAtFixedRate(this, 10, payInt);
    }

    public void run() {
        try {
            doer();
        } catch (Exception e) {
            logger.traceback(e);
            rtp.setClose();
        }
    }

    public void doer() {
        if (rtp.isClosed() != 0) {
            keepTimer.cancel();
            lower.state |= 4;
            return;
        }
        if (lower.buf == null) {
            keepTimer.cancel();
            lower.state |= 4;
            return;
        }
        if ((lower.state & 2) != 0) {
            keepTimer.cancel();
            lower.state |= 4;
            return;
        }
        if ((pos + paySiz) > lower.buf.length) {
            keepTimer.cancel();
            lower.state |= 4;
            return;
        }
        pck.clear();
        pck.putCopy(lower.buf, pos, 0, paySiz);
        pck.putSkip(paySiz);
        pck.RTPtyp = codec.getRTPtype();
        pck.RTPsrc = syncSrc;
        rtp.sendPack(pck);
        pos += paySiz;
    }

}

class sndWaveRec implements Runnable {

    private sndWave lower;

    private packHolder pck;

    private sndCodec codec;

    private packRtp rtp;

    private List<Byte> got;

    public sndWaveRec(sndWave parent, sndCodec codr, packRtp strm) {
        lower = parent;
        rtp = strm;
        codec = codr;
        pck = new packHolder(true, true);
        got = new ArrayList<Byte>();
        for (int i = 0; i < sndWave.size; i++) {
            got.add((byte) 0);
        }
        lower.buf = new byte[0];
        lower.state = 1;
        new Thread(this).start();
    }

    public void run() {
        try {
            doer();
        } catch (Exception e) {
            logger.traceback(e);
            rtp.setClose();
        }
        lower.state |= 4;
    }

    public void doer() {
        for (;;) {
            if (rtp.isClosed() != 0) {
                break;
            }
            if ((lower.state & 2) != 0) {
                break;
            }
            pck.clear();
            if (rtp.recvPack(pck, true) < 1) {
                break;
            }
            if (pck.RTPtyp != codec.getRTPtype()) {
                continue;
            }
            byte[] buf = pck.getCopy();
            for (int i = 0; i < buf.length; i++) {
                got.add(buf[i]);
            }
            lower.doDtmf(buf);
        }
        byte[] buf = new byte[got.size()];
        for (int i = 0; i < buf.length; i++) {
            buf[i] = got.get(i);
        }
        sndWave.makeHeader(buf, codec.getWAVtype());
        lower.buf = buf;
    }

}

class sndWaveDtmf implements Runnable {

    private sndWave lower;

    private packHolder pck;

    private sndCodec codec;

    private packRtp rtp;

    public sndWaveDtmf(sndWave parent, sndCodec codr, packRtp strm) {
        lower = parent;
        rtp = strm;
        codec = codr;
        pck = new packHolder(true, true);
        lower.state = 1;
        new Thread(this).start();
    }

    public void run() {
        try {
            doer();
        } catch (Exception e) {
            logger.traceback(e);
            rtp.setClose();
        }
        lower.state |= 4;
    }

    public void doer() {
        for (;;) {
            if (rtp.isClosed() != 0) {
                break;
            }
            if ((lower.state & 2) != 0) {
                break;
            }
            pck.clear();
            if (rtp.recvPack(pck, true) < 1) {
                break;
            }
            if (pck.RTPtyp != codec.getRTPtype()) {
                continue;
            }
            byte[] buf = pck.getCopy();
            lower.doDtmf(buf);
        }
    }

}
