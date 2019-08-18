package ifc;

import addr.addrMac;
import addr.addrType;
import cfg.cfgAll;
import pack.packHolder;
import pack.packPtp;
import util.bits;
import util.counter;
import util.debugger;
import util.logger;
import util.state;

/**
 * precision time protocol (ieee1588) handler
 *
 * @author matecsaba
 */
public class ifcPtp implements ifcUp, Runnable {

    /**
     * receive time
     */
    public boolean receive;

    private ifcDn lower = new ifcNull();

    private addrType hwadr;

    private counter cntr = new counter();

    private boolean need2run = true;

    /**
     * create new instance
     */
    public ifcPtp() {
        new Thread(this).start();
    }

    /**
     * stop working
     */
    public void stopWork() {
        need2run = false;
    }

    public String toString() {
        return "ptp on " + lower;
    }

    /**
     * received packet
     *
     * @param pck packet
     */
    public void recvPack(packHolder pck) {
        cntr.rx(pck);
        if (pck.msbGetW(0) != packPtp.ethtyp) {
            cntr.drop(pck, counter.reasons.badEthTyp);
            return;
        }
        pck.getSkip(2);
        if (debugger.ifcPtpEvnt) {
            logger.debug("received packet");
        }
        if (!receive) {
            return;
        }
        packPtp ptp = new packPtp();
        if (ptp.parsePacket(pck)) {
            return;
        }
        if (debugger.ifcPtpEvnt) {
            logger.debug("offsets: old=" + cfgAll.timeServerOffset + " new=" + ptp.offset + " diff=" + (cfgAll.timeServerOffset - ptp.offset));
        }
        long diff = cfgAll.timeServerOffset - ptp.offset;
        if (diff < 0) {
            diff = -diff;
        }
        if (diff > 1000) {
            logger.info("setting clock to " + bits.time2str(cfgAll.timeZoneName, bits.getTime() + ptp.offset, 3));
        }
        cfgAll.timeServerOffset = ptp.offset;
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
        int clk = bits.randomD();
        int seq = 0;
        for (;;) {
            if (!need2run) {
                return;
            }
            bits.sleep(1000);
            seq++;
            if (debugger.ifcPtpEvnt) {
                logger.debug("sending packet");
            }
            packHolder pck1 = new packHolder(true, true);
            packPtp.setMac(pck1.ETHtrg);
            if (hwadr.getSize() == addrMac.size) {
                pck1.ETHsrc.fromBuf(hwadr.getBytes(), 0);
            }
            packHolder pck2 = pck1.copyBytes(true, true);
            packPtp ptp = new packPtp();
            ptp.domain = 0;
            ptp.port = 1;
            ptp.clock = clk;
            ptp.sequence = seq;
            ptp.offset = cfgAll.timeServerOffset;
            ptp.createSync(pck1);
            packPtp.genHead(pck1);
            lower.sendPack(pck1);
            ptp.createFollow(pck2);
            packPtp.genHead(pck2);
            lower.sendPack(pck2);
        }
    }

}
