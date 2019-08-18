package ifc;

import addr.addrMac;
import addr.addrType;
import pack.packHolder;
import util.bits;
import util.counter;
import util.debugger;
import util.logger;
import util.state;
import util.typLenVal;

/**
 * synchronous ethernet (itu g8262) handler
 *
 * @author matecsaba
 */
public class ifcSyncE implements ifcUp, Runnable {

    /**
     * ethertype
     */
    public final static int ethtyp = 0x8809;

    /**
     * magic
     */
    public final static int magic = 0x0a0019a7;

    private ifcDn lower = new ifcNull();

    private addrType hwadr;

    private counter cntr = new counter();

    private boolean need2run = true;

    /**
     * create new instance
     */
    public ifcSyncE() {
        new Thread(this).start();
    }

    /**
     * stop working
     */
    public void stopWork() {
        need2run = false;
    }

    public String toString() {
        return "synceth on " + lower;
    }

    private typLenVal getTlv() {
        return new typLenVal(0, 8, 8, 16, 1, 3, 3, 1, 0, 512, true);
    }

    /**
     * received packet
     *
     * @param pck packet
     */
    public void recvPack(packHolder pck) {
        cntr.rx(pck);
        if (pck.msbGetW(0) != ethtyp) {
            cntr.drop(pck, counter.reasons.badEthTyp);
            return;
        }
        pck.getSkip(2);
        if (pck.msbGetD(0) != magic) {
            return;
        }
        if (debugger.ifcSynceEvnt) {
            logger.debug("received packet");
        }
    }

    /**
     * set parent
     *
     * @param parent parent
     */
    public void setParent(ifcDn parent) {
        lower = parent;
        hwadr = lower.getHwAddr();
    }

    /**
     * set state
     *
     * @param stat state
     */
    public void setState(state.states stat) {
    }

    /**
     * close interface
     */
    public void closeUp() {
    }

    /**
     * get counter
     *
     * @return counter
     */
    public counter getCounter() {
        return cntr;
    }

    public void run() {
        for (;;) {
            if (!need2run) {
                return;
            }
            bits.sleep(1000);
            if (debugger.ifcSynceEvnt) {
                logger.debug("sending packet");
            }
            packHolder pck = new packHolder(true, true);
            typLenVal tlv = getTlv();
            pck.ETHtrg.fromString("0100:0ccd:cdd0");
            if (hwadr.getSize() == addrMac.size) {
                pck.ETHsrc.fromBuf(hwadr.getBytes(), 0);
            }
            pck.msbPutW(0, ethtyp); // ethertype
            pck.msbPutD(2, magic); // magic
            pck.msbPutW(6, 0x01); // event
            pck.msbPutD(8, 0x10000000); // flags
            pck.putSkip(12);
            tlv.valDat[0] = 0xb; // g813
            tlv.putBytes(pck, 1, 1, tlv.valDat); // quality level
            tlv.valDat[0] = 0; // timestamp
            bits.msbPutD(tlv.valDat, 0, (int) bits.getTime()); // time
            tlv.putBytes(pck, 2, 5, tlv.valDat); // timestamp
            pck.merge2beg();
            lower.sendPack(pck);
        }
    }

}
