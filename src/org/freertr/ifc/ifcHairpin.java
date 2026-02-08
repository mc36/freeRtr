package org.freertr.ifc;

import java.util.List;
import org.freertr.addr.addrMac;
import org.freertr.addr.addrType;
import org.freertr.cfg.cfgInit;
import org.freertr.pack.packHolder;
import org.freertr.pipe.pipeLine;
import org.freertr.pipe.pipeSide;
import org.freertr.user.userHelp;
import org.freertr.util.bits;
import org.freertr.util.cmds;
import org.freertr.util.counter;
import org.freertr.util.logger;
import org.freertr.util.state;

/**
 * connect two interfaces
 *
 * @author matecsaba
 */
public class ifcHairpin {

    /**
     * description of this bridge
     */
    public String description = "";

    /**
     * interface is not ethernet
     */
    public boolean notEther;

    /**
     * buffer size
     */
    public int bufSiz = 65536;

    private ifcHairpinWorker s1;

    private ifcHairpinWorker s2;

    private pipeLine pip;

    /**
     * create new instance
     */
    public ifcHairpin() {
        s1 = new ifcHairpinWorker(this);
        s2 = new ifcHairpinWorker(this);
        setupBuffer(new pipeLine(64 * 1024, true));
    }

    private void setupBuffer(pipeLine p) {
        pip = p;
        s1.queueRx = pip.getSide();
        s2.queueRx = pip.getSide();
        s1.queueTx = s1.queueRx;
        s2.queueTx = s2.queueRx;
    }

    /**
     * get side one
     *
     * @return interface handler
     */
    public ifcDn getSide1() {
        return s1;
    }

    /**
     * get side two
     *
     * @return interface handler
     */
    public ifcDn getSide2() {
        return s2;
    }

    /**
     * stop this hairpin
     */
    public void stopWork() {
        s1.need2work = false;
        s2.need2work = false;
        pip.setClose();
    }

    /**
     * start this hairpin
     */
    public void startWork() {
        s1.start();
        s2.start();
    }

    /**
     * get help text
     *
     * @param l storage
     */
    public static void getHelp(userHelp l) {
        l.add(null, false, 1, new int[]{2, -1}, "description", "description of this hairpin");
        l.add(null, false, 2, new int[]{2, -1}, "[text]", "text describing this hairpin");
        l.add(null, false, 1, new int[]{-1}, "ethernet", "specify type of hairpin");
        l.add(null, false, 1, new int[]{2}, "buffer", "specify buffer size");
        l.add(null, false, 2, new int[]{-1}, "<num>", "buffer size in bytes");
        l.add(null, true, 1, new int[]{2}, "random12drop", "specify packet loss probability");
        l.add(null, true, 2, new int[]{-1}, "<num>", "one to this");
        l.add(null, true, 1, new int[]{2}, "random12burst", "specify burstiness probability");
        l.add(null, true, 2, new int[]{3}, "<num>", "one to this");
        l.add(null, true, 3, new int[]{4}, "<num>", "minimum time in ms");
        l.add(null, true, 4, new int[]{-1}, "<num>", "maximum time in ms");
        l.add(null, true, 1, new int[]{2}, "random12duplicate", "specify duplication probability");
        l.add(null, true, 2, new int[]{-1}, "<num>", "one to this");
        l.add(null, true, 1, new int[]{2}, "random12reorder", "specify reorder probability");
        l.add(null, true, 2, new int[]{-1}, "<num>", "one to this");
        l.add(null, true, 1, new int[]{2}, "random12delay", "specify delay probability");
        l.add(null, true, 2, new int[]{3}, "<num>", "one to this");
        l.add(null, true, 3, new int[]{4}, "<num>", "minimum time in ms");
        l.add(null, true, 4, new int[]{-1}, "<num>", "maximum time in ms");
        l.add(null, true, 1, new int[]{2}, "random12corrupt", "specify corruption probability");
        l.add(null, true, 2, new int[]{-1}, "<num>", "one to this");
        l.add(null, true, 1, new int[]{2}, "random21drop", "specify packet loss probability");
        l.add(null, true, 2, new int[]{-1}, "<num>", "one to this");
        l.add(null, true, 1, new int[]{2}, "random21burst", "specify burstiness probability");
        l.add(null, true, 2, new int[]{3}, "<num>", "one to this");
        l.add(null, true, 3, new int[]{4}, "<num>", "minimum time in ms");
        l.add(null, true, 4, new int[]{-1}, "<num>", "maximum time in ms");
        l.add(null, true, 1, new int[]{2}, "random21duplicate", "specify duplication probability");
        l.add(null, true, 2, new int[]{-1}, "<num>", "one to this");
        l.add(null, true, 1, new int[]{2}, "random21reorder", "specify reorder probability");
        l.add(null, true, 2, new int[]{-1}, "<num>", "one to this");
        l.add(null, true, 1, new int[]{2}, "random21delay", "specify delay probability");
        l.add(null, true, 2, new int[]{3}, "<num>", "one to this");
        l.add(null, true, 3, new int[]{4}, "<num>", "minimum time in ms");
        l.add(null, true, 4, new int[]{-1}, "<num>", "maximum time in ms");
        l.add(null, true, 1, new int[]{2}, "random21corrupt", "specify corruption probability");
        l.add(null, true, 2, new int[]{-1}, "<num>", "one to this");
    }

    /**
     * get configuration
     *
     * @param l storage
     * @param beg beginning
     */
    public void getConfig(List<String> l, String beg) {
        cmds.cfgLine(l, description.length() < 1, cmds.tabulator, "description", description);
        cmds.cfgLine(l, notEther, beg, "ethernet", "");
        l.add(beg + "buffer " + bufSiz);
        s1.getCfg(l, beg + "random21");
        s2.getCfg(l, beg + "random12");
    }

    /**
     * do configuration
     *
     * @param cmd command
     */
    public void doConfig(cmds cmd) {
        String s = cmd.word();
        if (s.equals("description")) {
            description = cmd.getRemaining();
            return;
        }
        if (s.equals("ethernet")) {
            notEther = false;
            return;
        }
        if (s.equals("buffer")) {
            bufSiz = bits.str2num(cmd.word());
            pipeLine old = pip;
            setupBuffer(new pipeLine(bufSiz, true));
            old.setClose();
            return;
        }
        ifcHairpinWorker cs = null;
        if (s.startsWith("random12")) {
            cs = s2;
        }
        if (s.startsWith("random21")) {
            cs = s1;
        }
        if (cs != null) {
            s = s.substring(8, s.length());
            if (s.equals("drop")) {
                cs.randDrop = bits.str2num(cmd.word());
                return;
            }
            if (s.equals("burst")) {
                cs.randBurstP = bits.str2num(cmd.word());
                cs.randBurstB = bits.str2num(cmd.word());
                cs.randBurstE = bits.str2num(cmd.word());
                return;
            }
            if (s.equals("duplicate")) {
                cs.randDup = bits.str2num(cmd.word());
                return;
            }
            if (s.equals("reorder")) {
                cs.randReord = bits.str2num(cmd.word());
                return;
            }
            if (s.equals("delay")) {
                cs.randDelayP = bits.str2num(cmd.word());
                cs.randDelayB = bits.str2num(cmd.word());
                cs.randDelayE = bits.str2num(cmd.word());
                return;
            }
            if (s.equals("corrupt")) {
                cs.randCrrpt = bits.str2num(cmd.word());
                return;
            }
            cmd.badCmd();
            return;
        }
        if (!s.equals(cmds.negated)) {
            cmd.badCmd();
            return;
        }
        s = cmd.word();
        if (s.equals("description")) {
            description = "";
            return;
        }
        if (s.equals("ethernet")) {
            notEther = true;
            return;
        }
        cs = null;
        if (s.startsWith("random12")) {
            cs = s2;
        }
        if (s.startsWith("random21")) {
            cs = s1;
        }
        if (cs != null) {
            s = s.substring(8, s.length());
            if (s.equals("drop")) {
                cs.randDrop = 0;
                return;
            }
            if (s.equals("burst")) {
                cs.randBurstP = 0;
                cs.randBurstB = 0;
                cs.randBurstE = 0;
                return;
            }
            if (s.equals("duplicate")) {
                cs.randDup = 0;
                return;
            }
            if (s.equals("reorder")) {
                cs.randReord = 0;
                return;
            }
            if (s.equals("delay")) {
                cs.randDelayP = 0;
                cs.randDelayB = 0;
                cs.randDelayE = 0;
                return;
            }
            if (s.equals("corrupt")) {
                cs.randCrrpt = 0;
                return;
            }
            cmd.badCmd();
            return;
        }
        cmd.badCmd();
    }

}

class ifcHairpinWorker implements ifcDn, Runnable {

    /**
     * need to work
     */
    public boolean need2work = true;

    /**
     * parent
     */
    public final ifcHairpin parent;

    /**
     * receive side
     */
    public pipeSide queueRx;

    /**
     * transmit side
     */
    public pipeSide queueTx;

    /**
     * drop probability
     */
    public int randDrop = 0;

    /**
     * duplication probability
     */
    public int randDup = 0;

    /**
     * burstiness probability
     */
    public int randBurstP = 0;

    /**
     * burstiness minimum
     */
    public int randBurstB = 0;

    /**
     * burstiness maximum
     */
    public int randBurstE = 0;

    /**
     * reorder probability
     */
    public int randReord = 0;

    /**
     * delay probability
     */
    public int randDelayP = 0;

    /**
     * delay minimum
     */
    public int randDelayB = 0;

    /**
     * delay maximum
     */
    public int randDelayE = 0;

    /**
     * corrupt probability
     */
    public int randCrrpt = 0;

    private counter cntr = new counter();

    public addrType hwaddr = addrMac.getRandom();

    private ifcUp upper = new ifcNull();

    public ifcHairpinWorker(ifcHairpin lower) {
        parent = lower;
    }

    public counter getCounter() {
        return cntr;
    }

    public state.states getState() {
        return state.states.up;
    }

    public void setFilter(boolean promisc) {
    }

    public addrType getHwAddr() {
        return hwaddr;
    }

    public void setUpper(ifcUp server) {
        upper = server;
        upper.setParent(this);
    }

    public void closeDn() {
    }

    public void flapped() {
    }

    public void sendPack(packHolder pck) {
        if (cfgInit.booting) {
            return;
        }
        pck.merge2beg();
        if (!parent.notEther) {
            ifcEther.createETHheader(pck, false);
        }
        byte[] buf = pck.getCopy();
        queueTx.nonBlockPut(buf, 0, buf.length);
    }

    public int getMTUsize() {
        return 1500;
    }

    public long getBandwidth() {
        return 100000000;
    }

    public void doWork() {
        packHolder pck = new packHolder(true, true);
        byte[] buf = new byte[packHolder.maxHead];
        for (;;) {
            if (!need2work) {
                break;
            }
            if (randBurstP > 0) {
                if (bits.random(0, randBurstP) == 0) {
                    bits.sleep(bits.random(randBurstB, randBurstE));
                }
            }
            int i = queueRx.blockingGet(buf, 0, buf.length);
            if (i < 0) {
                continue;
            }
            if (randDrop > 0) {
                if (bits.random(0, randDrop) == 0) {
                    continue;
                }
            }
            if (randCrrpt > 0) {
                if (bits.random(0, randCrrpt) == 0) {
                    buf[bits.random(0, i)] += (byte) bits.random(0, 255);
                }
            }
            buf2pck(buf, pck, i);
            if (randDup > 0) {
                if (bits.random(0, randDup) == 0) {
                    upper.recvPack(pck.copyBytes(true, true));
                }
            }
            if (randReord > 0) {
                if (bits.random(0, randReord) == 0) {
                    i = queueRx.blockingGet(buf, 0, buf.length);
                    if (i < 0) {
                        continue;
                    }
                    packHolder pck2 = new packHolder(true, true);
                    buf2pck(buf, pck2, i);
                    upper.recvPack(pck2);
                }
            }
            if (randDelayP > 0) {
                if (bits.random(0, randDelayP) == 0) {
                    ifcDelay.recvPack(bits.random(randDelayB, randDelayE), upper, pck);
                    continue;
                }
            }
            upper.recvPack(pck);
        }
    }

    private void buf2pck(byte[] buf, packHolder pck, int len) {
        pck.clear();
        pck.putCopy(buf, 0, 0, len);
        pck.putSkip(len);
        pck.merge2beg();
        if (parent.notEther) {
            return;
        }
        ifcEther.parseETHheader(pck, false);
    }

    public void start() {
        logger.startThread(this);
    }

    public void run() {
        try {
            doWork();
        } catch (Exception e) {
            logger.exception(e);
        }
    }

    public void getCfg(List<String> l, String beg) {
        l.add(beg + "drop " + randDrop);
        l.add(beg + "burst " + randBurstP + " " + randBurstB + " " + randBurstE);
        l.add(beg + "duplicate " + randDup);
        l.add(beg + "reorder " + randReord);
        l.add(beg + "delay " + randDelayP + " " + randDelayB + " " + randDelayE);
        l.add(beg + "corrupt " + randCrrpt);
    }

}
