package line;

import addr.addrEmpty;
import addr.addrType;
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
 * asynchronous slip (rfc1055) framer
 *
 * @author matecsaba
 */
public class lineSlip implements Runnable, ifcDn {

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
     * end character
     */
    public final static int charEnd = 192;

    /**
     * escape character
     */
    public final static int charEsc = 219;

    /**
     * escaped end
     */
    public final static int escEnd = 220;

    /**
     * escaped end
     */
    public final static int escEsc = 221;

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
        return "slip on async";
    }

    /**
     * create new instance
     *
     * @param pipe
     */
    public lineSlip(pipeSide pipe) {
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
        cntr.tx(pck);
        byte[] bd = new byte[1024];
        int bs = 1;
        bd[0] = (byte) charEnd;
        for (int p = 0; p < pck.dataSize(); p++) {
            if (bs >= (bd.length - 16)) {
                lower.morePut(bd, 0, bs);
                bs = 0;
            }
            int i = pck.getByte(p);
            switch (i) {
                case charEnd:
                    bd[bs] = (byte) charEsc;
                    bs++;
                    i = escEnd;
                    break;
                case charEsc:
                    bd[bs] = (byte) charEsc;
                    bs++;
                    i = escEsc;
                    break;
            }
            bd[bs] = (byte) i;
            bs++;
        }
        bd[bs] = (byte) charEnd;
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
                if (i == charEnd) {
                    break;
                }
                if (i == charEsc) {
                    if (lower.moreGet(buf, 0, buf.length) != buf.length) {
                        return;
                    }
                    switch (buf[0] & 0xff) {
                        case escEnd:
                            i = charEnd;
                            break;
                        case escEsc:
                            i = charEsc;
                            break;
                    }
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
            cntr.rx(pck);
            upper.recvPack(pck);
        }
    }

}
