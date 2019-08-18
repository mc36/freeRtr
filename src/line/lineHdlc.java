package line;

import addr.addrEmpty;
import addr.addrType;
import cry.cryHashFcs16;
import ifc.ifcDn;
import ifc.ifcNull;
import ifc.ifcUp;
import pack.packHolder;
import pipe.pipeSide;
import util.bits;
import util.counter;
import util.logger;
import util.state;

/**
 * asynchronous hdlc (rfc1662) framer
 *
 * @author matecsaba
 */
public class lineHdlc implements Runnable, ifcDn {

    /**
     * counter of this interface
     */
    public counter cntr = new counter();

    /**
     * server that handler received packets
     */
    public ifcUp upper = new ifcNull();

    /**
     * server that sends our bytes
     */
    public pipeSide lower;

    /**
     * async char map
     */
    public int asyncMask = 0xffffffff;

    /**
     * mask upper characters too
     */
    public boolean maskUpper = false;

    /**
     * flag character
     */
    public final static int charFlag = 0x7e;

    /**
     * escape character
     */
    public final static int charEsc = 0x7d;

    /**
     * escape xorer
     */
    public final static int charXor = 0x20;

    /**
     * get counter
     *
     * @return counter
     */
    public counter getCounter() {
        return cntr;
    }

    /**
     * close interface
     */
    public void closeDn() {
        lower.setClose();
    }

    /**
     * flap interface
     */
    public void flapped() {
        lower.setClose();
    }

    /**
     * set upper layer
     *
     * @param server upper layer
     */
    public void setUpper(ifcUp server) {
        upper = server;
        upper.setParent(this);
    }

    /**
     * get state
     *
     * @return state
     */
    public state.states getState() {
        return state.states.up;
    }

    /**
     * set filter
     *
     * @param promisc promiscous mode
     */
    public void setFilter(boolean promisc) {
    }

    /**
     * get hw address
     *
     * @return hw address
     */
    public addrType getHwAddr() {
        return new addrEmpty();
    }

    /**
     * get mtu size
     *
     * @return mtu size
     */
    public int getMTUsize() {
        return 1504;
    }

    /**
     * get bandwidth
     *
     * @return bandwidth
     */
    public long getBandwidth() {
        return 128000;
    }

    public String toString() {
        return "hdlc on async";
    }

    /**
     * create new instance
     *
     * @param pipe
     */
    public lineHdlc(pipeSide pipe) {
        lower = pipe;
        new Thread(this).start();
    }

    /**
     * wait until working
     */
    public void wait4working() {
        for (;;) {
            bits.sleep(1000);
            if (lower.isClosed() != 0) {
                break;
            }
        }
    }

    public void run() {
        try {
            doRecvLoop();
        } catch (Exception e) {
            logger.traceback(e);
        }
        lower.setClose();
    }

    /**
     * send packet
     *
     * @param pck packet
     */
    public void sendPack(packHolder pck) {
        cryHashFcs16 sum = new cryHashFcs16();
        sum.init();
        pck.hashData(sum, 0, pck.dataSize());
        byte[] cb = sum.finish();
        pck.putCopy(cb, 0, 0, cb.length);
        pck.putSkip(cb.length);
        pck.merge2end();
        cntr.tx(pck);
        byte[] bd = new byte[1024];
        int bs = 1;
        bd[0] = charFlag;
        for (int p = 0; p < pck.dataSize(); p++) {
            if (bs >= (bd.length - 16)) {
                lower.morePut(bd, 0, bs);
                bs = 0;
            }
            int i = pck.getByte(p);
            int o = i;
            if (maskUpper) {
                o = o & 0x7f;
            }
            boolean esc = false;
            if ((i == charFlag) || (i == charEsc)) {
                esc = true;
            }
            if (o < 32) {
                esc |= ((1 << o) & asyncMask) != 0;
            }
            if (esc) {
                bd[bs] = charEsc;
                bs++;
                i = i ^ charXor;
            }
            bd[bs] = (byte) i;
            bs++;
        }
        bd[bs] = charFlag;
        bs++;
        lower.morePut(bd, 0, bs);
    }

    private void doRecvLoop() {
        for (;;) {
            packHolder pck = new packHolder(true, true);
            int siz = 0;
            for (;;) {
                if (siz > 64) {
                    pck.putSkip(siz);
                    pck.merge2end();
                    siz = 0;
                    if (pck.dataSize() > (packHolder.maxData - 16)) {
                        pck.clear();
                        cntr.drop(pck, counter.reasons.tooLong);
                    }
                }
                byte[] buf = new byte[1];
                if (lower.moreGet(buf, 0, buf.length) != buf.length) {
                    return;
                }
                int i = buf[0] & 0xff;
                if (i == charFlag) {
                    break;
                }
                if (i == charEsc) {
                    if (lower.moreGet(buf, 0, buf.length) != buf.length) {
                        return;
                    }
                    i = buf[0] & 0xff;
                    i = i ^ charXor;
                }
                pck.putByte(siz, i);
                siz++;
            }
            pck.putSkip(siz);
            pck.merge2end();
            siz = pck.dataSize();
            if (siz < 1) {
                continue;
            }
            if (siz < 3) {
                cntr.drop(pck, counter.reasons.tooSmall);
                continue;
            }
            cryHashFcs16 sum = new cryHashFcs16();
            sum.init();
            pck.hashData(sum, 0, siz - 2);
            byte[] cb = sum.finish();
            byte[] cg = new byte[cb.length];
            pck.getCopy(cg, 0, siz - cg.length, cg.length);
            if (bits.byteComp(cg, 0, cb, 0, cg.length) != 0) {
                cntr.drop(pck, counter.reasons.badSum);
                continue;
            }
            pck.setDataSize(siz - cg.length);
            cntr.rx(pck);
            upper.recvPack(pck);
        }
    }

}
