package ifc;

import addr.addrMac;
import pack.packHolder;
import util.bits;
import util.counter;
import util.logger;
import util.state;

/**
 * random packet handler
 *
 * @author matecsaba
 */
public class ifcRandom implements ifcUp, Runnable {

    /**
     * ethertype to use
     */
    public int ethtyp;

    /**
     * minimum packet size
     */
    public int sizMin;

    /**
     * maximum packet size
     */
    public int sizMax;

    /**
     * minimum interval
     */
    public int intMin;

    /**
     * maximum interval
     */
    public int intMax;

    private ifcDn lower = new ifcNull();

    private addrMac hwadr = addrMac.getRandom();

    private addrMac bcast = addrMac.getBroadcast();

    private counter cntr = new counter();

    private boolean need2work;

    public String toString() {
        return "random on " + lower;
    }

    /**
     * get config
     *
     * @param c handle
     * @return config
     */
    public static String getCfg(ifcRandom c) {
        if (c == null) {
            return "";
        }
        return bits.toHexW(c.ethtyp) + " " + c.sizMin + " " + c.sizMax + " " + c.intMin + " " + c.intMax;
    }

    public void setParent(ifcDn parent) {
        lower = parent;
        try {
            hwadr = (addrMac) lower.getHwAddr();
        } catch (Exception e) {
        }
    }

    public void setState(state.states stat) {
    }

    public void closeUp() {
    }

    public counter getCounter() {
        return cntr;
    }

    public void recvPack(packHolder pck) {
        cntr.rx(pck);
    }

    /**
     * stop work
     */
    public void stopWord() {
        need2work = false;
    }

    /**
     * start work
     */
    public void startWord() {
        need2work = true;
        new Thread(this).start();
    }

    private void doRound() {
        bits.sleep(bits.random(intMin, intMax));
        packHolder pck = new packHolder(true, true);
        int o = bits.random(sizMin, sizMax);
        for (int i = 0; i < o; i++) {
            pck.putByte(i, bits.randomB());
        }
        pck.putSkip(o);
        pck.merge2beg();
        pck.msbPutW(0, ethtyp);
        pck.putSkip(2);
        pck.merge2beg();
        pck.ETHsrc.setAddr(hwadr);
        pck.ETHtrg.setAddr(bcast);
        lower.sendPack(pck);
    }

    public void run() {
        for (;;) {
            if (!need2work) {
                break;
            }
            try {
                doRound();
            } catch (Exception e) {
                logger.traceback(e);
            }
        }
    }

}
